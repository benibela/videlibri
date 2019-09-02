package de.benibela.videlibri

import android.app.Activity
import android.content.Intent
import android.graphics.Paint
import android.graphics.Typeface
import android.os.Bundle
import android.support.v4.view.ViewCompat
import android.support.v7.app.ActionBar
import android.text.InputType
import android.util.Log
import android.view.Gravity
import android.view.Menu
import android.view.View
import android.widget.EditText
import android.widget.LinearLayout
import android.widget.Spinner
import android.widget.TextView
import de.benibela.videlibri.jni.Bridge
import de.benibela.videlibri.jni.FormInput
import de.benibela.videlibri.jni.FormParams
import de.benibela.videlibri.jni.FormSelect
import java.util.*
import android.view.ViewGroup
import kotlin.math.roundToInt


internal interface SearchEventHandler {
    fun onSearchEvent(event: Bridge.SearchEvent): Boolean
}
private class SearchParamHolder(
        val input: FormInput,
        val layout: LinearLayout,
        val caption: TextView,
        val view: View) {
    val edit get() = view as? EditText
    val spinner get() = view as? Spinner
}
class Search: VideLibriBaseActivity(), SearchEventHandler{
    private val searchParamHolders = mutableMapOf<String, SearchParamHolder>()

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setVideLibriView(R.layout.searchlayout)

        libId = savedInstanceState?.getString("libId") ?: intent?.getStringExtra("libId") ?: ""
        libName = savedInstanceState?.getString("libName") ?: intent?.getStringExtra("libName") ?: ""


        if (libName.isEmpty() && libId != "")
            libName = Bridge.VLGetLibraryDetails(libId)?.prettyName ?: ""

        val lib = findViewById<TextView>(R.id.library)
        lib.setText(libName + " (" + tr(R.string.change) + ")")
        lib.paintFlags = lib.paintFlags or Paint.UNDERLINE_TEXT_FLAG
        if (libId.isEmpty() || (intent?.getBooleanExtra("showLibList", false)?:false) && savedInstanceState == null) {
            libId = LibraryList.lastSelectedFallbackLibraryId() ?: ""
            if (libId.isEmpty()) changeSearchLib()
            else libName = LibraryList.lastSelectedLibName ?: ""
        }

        findViewById<View>(R.id.library).setOnClickListener( { changeSearchLib() })

        val searchStartClickListener = View.OnClickListener {
            obtainSearcher()

            if ("debug" == searchParamHolders["year"]?.edit?.text?.toString()) {
                activateHiddenDebugMode()
                return@OnClickListener
            }

            val intent = Intent(this@Search, SearchResult::class.java)
            val book = Bridge.Book()
            for (e in searchParamHolders.entries) {
                val view = e.value.view
                val value = when (view) {
                    is EditText -> view.text.toString()
                    is Spinner -> (e.value.input as? FormSelect)?.optionValues?.getOrNull(view.selectedItemPosition)
                    else -> null
                } ?: continue
                when (e.key) {
                    "title" -> book.title = value
                    "author" -> book.author = value
                    "year" -> book.year = value
                    "id" -> book.id = value
                    else -> book.setProperty(e.key, value)
                }
            }
            intent.putExtra("searchQuery", book)
            startActivity(intent)
        }
        findViewById<View>(R.id.button).setOnClickListener(searchStartClickListener)

        setTitle("")
        supportActionBar?.let {
            it.setDisplayShowTitleEnabled(false)
            it.setDisplayShowCustomEnabled(true)
            it.setCustomView(getLayoutInflater().inflate(R.layout.searchlayout_bar, null),
                    ActionBar.LayoutParams(ActionBar.LayoutParams.MATCH_PARENT, ActionBar.LayoutParams.MATCH_PARENT, Gravity.CENTER
            ))
            it.customView.findViewById<View>(R.id.button).setOnClickListener(searchStartClickListener)
        }

        if (libId != "") {
            obtainSearcher()
            updateSearchParamsViews()
        }

        val defaultQuery = intent?.getSerializableExtra("query")
        if (defaultQuery is Map<*, *>) {
            for (key in arrayOf("title", "author"))
                searchParamHolders[key]?.edit?.setText(defaultQuery.get(key).toString())
        }
    }

    override fun onSaveInstanceState(outState: Bundle?) {
        super.onSaveInstanceState(outState)
        outState?.apply{
            putString("libId", libId)
            putString("libName", libName)
        }
    }

    internal val REQUEST_CHOOSE_LIBRARY = 1234

    internal var libId: String = ""
    internal var libName: String = ""

    internal var searcher: Bridge.SearcherAccess? = null


    fun gcSearchers() {
        for (i in searchers.indices.reversed()) {
            //Log.d("VideLibri", " GC Searcher: " + i + "/"+searchers.size()+ ": "+searchers.get(i).nativePtr+" // "+(System.currentTimeMillis() - searchers.get(i).heartBeat));
            if (System.currentTimeMillis() - searchers[i].heartBeat > SEARCHER_HEARTH_BEAT_TIMEOUT || searchers[i].state == SEARCHER_STATE_FAILED) {
                searchers[i].free()
                searchers.removeAt(i)
            }
        }
    }


    protected fun obtainSearcher() {
        gcSearchers()
        if (searchers.size > 0) {
            val candidate = searchers[searchers.size - 1]
            if (candidate.libId == libId)
                when (candidate.state) {
                    SEARCHER_STATE_INIT, SEARCHER_STATE_CONNECTED -> {
                        searcher = candidate
                        return
                    }
                }
        }
        searcher = Bridge.SearcherAccess(libId)
        searcher?.let {
            it.heartBeat = System.currentTimeMillis()
            it.state = SEARCHER_STATE_INIT
            it.connect()
            if (it.nativePtr != 0L) {
                beginLoading(VideLibriBaseActivityOld.LOADING_SEARCH_CONNECTING)
                searchers.add(it)
            }
        }
    }


    override fun onResume() {
        super.onResume()
        obtainSearcher()
        searcher?.let {
            if (it.nativePtr != 0L) {
                for (event in it.pendingEvents)
                    onSearchEvent(event)
            }
            it.pendingEvents.clear()
            if (it.state != SEARCHER_STATE_INIT)
                endLoadingAll(VideLibriBaseActivityOld.LOADING_SEARCH_CONNECTING)
        }
    }

    override fun onPrepareOptionsMenu(menu: Menu?): Boolean {
        val x = super.onPrepareOptionsMenu(menu)
        menu?.findItem(R.id.search)?.setVisible(false)
        return x
    }

    override fun onDestroy() {
        gcSearchers()
        super.onDestroy()
    }


    internal fun changeSearchLib() {
        startActivityForResult<LibraryList>(REQUEST_CHOOSE_LIBRARY,
                "defaultLibId" to libId,
                "reason" to getString(R.string.search_selectlib),
                "search" to true
        )
    }

    private fun updateSearchParamsViews() {
        val searcher = searcher ?: return
        val lay = findViewById<LinearLayout>(R.id.layout)
        lay.removeAllViews()
        val inflater = layoutInflater
        val oldSearchParamHolders = searchParamHolders.toMap()
        searchParamHolders.clear()
        for (param in searcher.searchParams.inputs){
            if (param.name in searchParamHolders) continue;
            val old = oldSearchParamHolders[param.name]?.takeIf { it.input == param }
            if (old != null) {
                lay.addView(old.layout)
                searchParamHolders[param.name] = old
            } else {
                val row = inflater.inflate(if (param is FormSelect) R.layout.searchlayout_row_spinner else R.layout.searchlayout_row_edit, lay, true)
                val layout = row.findViewById<LinearLayout>(R.id.innerLayout).also {
                    it.id = ViewCompat.generateViewId()
                }
                val caption = row.findViewById<TextView>(R.id.textView).also {
                    it.text = param.caption
                    it.id = ViewCompat.generateViewId()
                    when (param.name) {
                        "title", "author", "free" -> it.setTypeface(null, Typeface.BOLD)
                    }
                }
                val inputView: View = if (param is FormSelect) {
                    val spinner = row.findViewById<Spinner>(R.id.spinner)
                    spinner.setItems(param.optionCaptions)
                    val i = param.optionValues.indexOf(param.value)
                    spinner.setSelection(if (i >= 0) i else 0)
                    spinner
                } else {
                    val edit = row.findViewById<EditText>(R.id.edit)
                    when (param.name) {
                        "year" -> edit.setRawInputType(InputType.TYPE_CLASS_NUMBER)
                    }
                    edit
                }

                searchParamHolders[param.name] = SearchParamHolder(param, layout, caption, inputView)
                val id = (if (param is FormSelect) "FormSelect" else "FormInput") + param.name
                inputView.id = namedViewIds.getOrPut(id, { ViewCompat.generateViewId() })
            }
        }

        val portMode = resources.getBoolean(R.bool.port_mode)
        val minimumWidth = if (portMode) 0 else searchParamHolders.values.map{it.caption.paint.measureText(it.caption.text.toString())}.max()?.roundToInt()?:0
        for (h in searchParamHolders.values)
            if (portMode) {
                h.layout.orientation = LinearLayout.VERTICAL
                h.caption.layoutParams = LinearLayout.LayoutParams(LinearLayout.LayoutParams.MATCH_PARENT, LinearLayout.LayoutParams.WRAP_CONTENT);
                h.view.layoutParams = LinearLayout.LayoutParams(LinearLayout.LayoutParams.MATCH_PARENT, LinearLayout.LayoutParams.WRAP_CONTENT);
            } else {
                h.layout.orientation = LinearLayout.HORIZONTAL
                h.caption.layoutParams = LinearLayout.LayoutParams(LinearLayout.LayoutParams.WRAP_CONTENT, LinearLayout.LayoutParams.WRAP_CONTENT);
                h.view.layoutParams = LinearLayout.LayoutParams(LinearLayout.LayoutParams.MATCH_PARENT, LinearLayout.LayoutParams.WRAP_CONTENT);
                h.caption.minimumWidth = minimumWidth + 10
            }

    }

    override fun onActivityResult(requestCode: Int, resultCode: Int, data: Intent?) {
        if (requestCode == REQUEST_CHOOSE_LIBRARY) {
            if (resultCode == Activity.RESULT_OK) {
                libId = LibraryList.lastSelectedLibId ?: ""
                libName = LibraryList.lastSelectedLibName ?: ""
                findViewById<TextView>(R.id.library).setText(libName)

                obtainSearcher()
                updateSearchParamsViews()
            } else if ("" == libId) finish()
        } else
            super.onActivityResult(requestCode, resultCode, data)
    }

    override fun onSearchEvent(event: Bridge.SearchEvent): Boolean {
        if (debugTester?.onSearchEvent(event) ?: false) return true
        if (event.searcherAccess !== searcher) return false
        event.searcherAccess?.let { s ->
            when (event.kind) {
                Bridge.SearchEventKind.CONNECTED -> {
                    endLoadingAll(VideLibriBaseActivityOld.LOADING_SEARCH_CONNECTING)
                    s.heartBeat = System.currentTimeMillis()
                    s.state = SEARCHER_STATE_CONNECTED
                    s.searchParams = event.obj1 as? FormParams
                    updateSearchParamsViews()
                    return true
                }
                Bridge.SearchEventKind.EXCEPTION -> {
                    endLoadingAll(VideLibriBaseActivityOld.LOADING_SEARCH_CONNECTING)
                    s.state = SEARCHER_STATE_FAILED
                    searcher = null
                    gcSearchers()
                    VideLibriApp.showPendingExceptions()
                    return true
                }
                else -> {}
            }
        }
        return false
    }


    internal var debugTester: SearchDebugTester? = null
    fun activateHiddenDebugMode() {
        showDialog("You have activated the secret debug mode") {
            onDismiss = {
                showMessageYesNo("Do you want to search ALL libraries? ") {
                    withActivity<Search> {
                        val book = Bridge.Book()
                        book.title = searchParamHolders["title"]?.edit?.text.toString()
                        book.author = searchParamHolders["author"]?.edit?.text.toString()
                        debugTester = SearchDebugTester(book, libId)
                    }
                }
            }
        }
    }


    companion object {
        internal var searchers = ArrayList<Bridge.SearcherAccess>()

        internal const val SEARCHER_HEARTH_BEAT_TIMEOUT = 5 * 60 * 1000
        internal const val SEARCHER_STATE_INIT = 0 //connecting
        internal const val SEARCHER_STATE_CONNECTED = 1
        internal const val SEARCHER_STATE_SEARCHING = 2
        internal const val SEARCHER_STATE_FAILED = 3

        internal val namedViewIds = mutableMapOf<String, Int>()
    }
}




internal class SearchDebugTester(var query: Bridge.Book, startId: String) {
    var libs: Array<String>
    var pos: Int = 0
    var searcher: Bridge.SearcherAccess? = null

    init {
        libs = Bridge.VLGetLibraryIds()
        pos = 0
        while (pos < libs.size && startId != libs[pos]) pos++
        start()
    }

    private fun start() {
        while (true) {
            val lib = Bridge.VLGetLibraryDetails(libs[pos])
            if (lib?.searchMightWork ?: false) break
            pos++
            if (pos >= libs.size) return
        }
        Log.i("VIDELIBRI", "============================================================")
        Log.i("VIDELIBRI", "Testing search: " + libs[pos])
        Log.i("VIDELIBRI", "============================================================")
        searcher = Bridge.SearcherAccess(libs[pos])
        searcher?.connect()
        withActivity<Search> {
            libId = libs[pos]
            libName = libs[pos]
            findViewById<TextView>(R.id.library).setText(libName)
            beginLoading(VideLibriBaseActivityOld.LOADING_SEARCH_SEARCHING)
        }
    }

    fun onSearchEvent(event: Bridge.SearchEvent): Boolean {
        if (event.searcherAccess !== searcher) return false
        val currentSearcher = searcher ?: return false
        when (event.kind) {
            Bridge.SearchEventKind.CONNECTED -> currentSearcher.start(query)
            Bridge.SearchEventKind.FIRST_PAGE -> {
                if (currentSearcher.nextPageAvailable) {
                    currentSearcher.nextPage()
                } else {
                    currentSearcher.free()
                    pos++
                    if (pos < libs.size)
                        start()
                    else
                        endComplete()
                }
            }
            Bridge.SearchEventKind.NEXT_PAGE -> {
                currentSearcher.free()
                pos++
                if (pos < libs.size)
                    start()
                else
                    endComplete()
            }
            Bridge.SearchEventKind.EXCEPTION -> {
                currentSearcher.free()
                endComplete()
                VideLibriApp.showPendingExceptions()
            }
        }
        return true
    }

    private fun endComplete() {
        searcher = null
        withActivity<Search> {
            endLoadingAll(VideLibriBaseActivityOld.LOADING_SEARCH_SEARCHING)
            debugTester = null
        }
    }
}
