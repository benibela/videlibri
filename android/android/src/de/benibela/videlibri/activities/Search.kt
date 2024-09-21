package de.benibela.videlibri.activities

import android.app.Activity
import android.content.Intent
import android.graphics.Paint
import android.graphics.Point
import android.graphics.Typeface
import android.os.Build
import android.os.Bundle
import android.os.Parcelable
import android.text.InputType
import android.util.Log
import android.view.Menu
import android.view.View
import android.view.WindowInsets
import android.widget.EditText
import android.widget.LinearLayout
import android.widget.Spinner
import android.widget.TextView
import androidx.core.view.ViewCompat
import de.benibela.videlibri.R
import de.benibela.videlibri.VideLibriApp
import de.benibela.videlibri.databinding.SearchlayoutBinding
import de.benibela.videlibri.databinding.SearchlayoutRowEditBinding
import de.benibela.videlibri.databinding.SearchlayoutRowSpinnerBinding
import de.benibela.videlibri.jni.*
import de.benibela.videlibri.utils.*
import kotlinx.parcelize.Parcelize
import java.util.*
import kotlin.math.roundToInt


internal interface SearchEventHandler {
    fun onSearchEvent(event: SearchEvent): Boolean
}
private class SearchParamHolder(
        val input: FormInput,
        val layout: LinearLayout,
        val caption: TextView,
        val view: View) {
    val edit get() = view as? EditText
    val spinner get() = view as? Spinner
}
class Search: VideLibriBaseActivity(), SearchEventHandler {
    @Parcelize
    class State(var libId: String = "",
                var libName: String = "",

                var focusedChild: String = "",
                val editState: MutableMap<String, Parcelable> = mutableMapOf(),
                val spinnerState: MutableMap<String, Parcelable> = mutableMapOf()
    ): Parcelable

    var state = State()
    lateinit var binding: SearchlayoutBinding
    private val searchParamHolders = mutableMapOf<String, SearchParamHolder>()

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        binding = setVideLibriView(SearchlayoutBinding::inflate)
        registerState(::state)

        state.apply {
            if (libId == "") {
                libId = intent?.getStringExtra("libId") ?: ""
                libName = intent?.getStringExtra("libName") ?: ""
            }

            if (libName.isEmpty() && libId != "")
                libName = Bridge.VLGetLibraryDetails(libId)?.prettyName ?: ""

            val lib = binding.library
            lib.text = "$libName (${getString(R.string.change)})"
            lib.paintFlags = lib.paintFlags or Paint.UNDERLINE_TEXT_FLAG
            if (libId.isEmpty() || (intent?.getBooleanExtra("showLibList", false) == true) && savedInstanceState == null) {
                libId = LibraryList.lastSelectedFallbackLibraryId() ?: ""
                if (libId.isEmpty()) this@Search.changeSearchLib()
                else libName = LibraryList.lastSelectedLibName ?: ""
            }
        }

        binding.library.setOnClickListener { changeSearchLib() }

        val searchStartClickListener = View.OnClickListener {
            if (searchParamHolders.entries.all { it.value.edit?.text?.isBlank() ?: true }) {
                showMessage(R.string.search_no_terms)
                return@OnClickListener
            }

            obtainSearcher()

            if ("debug" == searchParamHolders["year"]?.edit?.text?.toString()) {
                activateHiddenDebugMode()
                return@OnClickListener
            }

            val intent = Intent(this@Search, SearchResult::class.java)
            val book = Bridge.Book()
            for (e in searchParamHolders.entries) {
                val value = when (val view = e.value.view) {
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
        binding.button.setOnClickListener(searchStartClickListener)

        title = ""
        supportActionBar?.let {
            it.setDisplayShowTitleEnabled(false)
            it.setDisplayShowCustomEnabled(true)
            it.setCustomView(R.layout.searchlayout_bar)
            it.customView.findViewById<View>(R.id.button).setOnClickListener(searchStartClickListener)
        }

        if (state.libId != "") {
            obtainSearcher()
            updateSearchParamsViews()
        }

        val defaultQuery = intent?.getSerializableExtra("query")
        if (defaultQuery is Map<*, *>) {
            for (key in arrayOf("title", "author"))
                searchParamHolders[key]?.edit?.setText(defaultQuery[key].toString())
        }
    }


    private var searcher: SearcherAccess? = null


    private fun gcSearchers() {
        for (i in searchers.indices.reversed()) {
            //Log.d("VideLibri", " GC Searcher: " + i + "/"+searchers.size()+ ": "+searchers.get(i).nativePtr+" // "+(System.currentTimeMillis() - searchers.get(i).heartBeat));
            if (System.currentTimeMillis() - searchers[i].heartBeat > SEARCHER_HEARTH_BEAT_TIMEOUT || searchers[i].state == SEARCHER_STATE_FAILED) {
                searchers[i].free()
                searchers.removeAt(i)
            }
        }
    }


    private fun obtainSearcher() {
        gcSearchers()
        if (searchers.size > 0) {
            val candidate = searchers[searchers.size - 1]
            if (candidate.libId == state.libId)
                when (candidate.state) {
                    SEARCHER_STATE_INIT, SEARCHER_STATE_CONNECTED -> {
                        searcher = candidate
                        return
                    }
                }
        }
        searcher = SearcherAccess(state.libId)
        searcher?.let {
            it.heartBeat = System.currentTimeMillis()
            it.state = SEARCHER_STATE_INIT
            it.connect()
            if (it.nativePtr != 0L) {
                searchers.add(it)
            } else
                it.state = SEARCHER_STATE_FAILED
            refreshLoadingIcon()
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
        }
    }

    override fun onSaveInstanceState(outState: Bundle) {
        state.spinnerState.clear()
        state.editState.clear()
        for (holder in searchParamHolders){
            holder.value.edit?.onSaveInstanceState()?.let { state.editState[holder.key] = it }
            holder.value.spinner?.onSaveInstanceState()?.let { state.spinnerState[holder.key] = it }
            if (holder.value.view.isFocused) state.focusedChild = holder.key
        }
        super.onSaveInstanceState(outState)
    }

    override fun onRestoreInstanceState(savedInstanceState: Bundle) {
        super.onRestoreInstanceState(savedInstanceState)
        for (s in state.editState)
            searchParamHolders[s.key]?.edit?.onRestoreInstanceState(s.value)
        for (s in state.spinnerState)
            searchParamHolders[s.key]?.spinner?.onRestoreInstanceState(s.value)

    }

    override fun onPrepareOptionsMenu(menu: Menu?): Boolean {
        val x = super.onPrepareOptionsMenu(menu)
        menu?.findItem(R.id.search)?.isVisible = false
        return x
    }

    override fun onDestroy() {
        gcSearchers()
        super.onDestroy()
    }


    private fun changeSearchLib() {
        startActivityForResult<LibraryList>(
                "defaultLibId" to state.libId,
                "reason" to getString(R.string.search_selectlib),
                "search" to true
        ) {resultCode, data -> withActivity<Search> {
            if (resultCode == Activity.RESULT_OK) {
                state.libId = LibraryList.lastSelectedLibId ?: ""
                state.libName = LibraryList.lastSelectedLibName ?: ""
                binding.library.text = state.libName

                obtainSearcher()
                updateSearchParamsViews()
            } else if ("" == state.libId) finish()
        }
        }
    }

    private fun getDisplaySize(): Point {
        //see https://stackoverflow.com/questions/1016896/how-to-get-screen-dimensions-as-pixels-in-android/1016941#1016941
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.R) {
            val metrics = windowManager.currentWindowMetrics
            val windowInsets = metrics.windowInsets
            val insets = windowInsets.getInsetsIgnoringVisibility(
                WindowInsets.Type.navigationBars() or WindowInsets.Type.displayCutout()
            )

            val insetsWidth = insets.right + insets.left
            val insetsHeight = insets.top + insets.bottom
            val bounds = metrics.bounds
            return Point(
                bounds.width() - insetsWidth,
                bounds.height() - insetsHeight
            )
        } else @Suppress("DEPRECATION") {
            val display = windowManager.defaultDisplay
            val size = Point()
            display.getSize(size)
            return size
        }
    }

    private fun updateSearchParamsViews() {
        val searcher = searcher ?: return
        state.focusedChild = searchParamHolders.filter { it.value.view.isFocused }.keys.firstOrNull() ?: state.focusedChild
        val lay = binding.layout
        lay.removeAllViews()
        val inflater = layoutInflater
        val oldSearchParamHolders = searchParamHolders.toMap()
        searchParamHolders.clear()
        var focusView: View? = null
        searcher.searchParams?.inputs?.forEach { param ->
            if (param.name !in searchParamHolders) {
                val old = oldSearchParamHolders[param.name]?.takeIf { it.input == param }
                val new = if (old != null) {
                    lay.addView(old.layout)
                    old
                } else {
                    val holder: SearchParamHolder
                    if (param is FormSelect) {
                        val rowBinding = SearchlayoutRowSpinnerBinding.inflate(inflater, lay, true)
                        holder = SearchParamHolder(param, rowBinding.innerLayout, rowBinding.textView, rowBinding.spinner)
                        rowBinding.spinner.setItems(param.optionCaptions)
                        val i = param.optionValues.indexOf(param.value)
                        rowBinding.spinner.setSelection(if (i >= 0) i else 0)
                    } else {
                        val rowBinding = SearchlayoutRowEditBinding.inflate(inflater, lay, true)
                        holder = SearchParamHolder(param, rowBinding.innerLayout, rowBinding.textView, rowBinding.edit)
                        when (param.name) {
                            "year" -> rowBinding.edit.setRawInputType(InputType.TYPE_CLASS_NUMBER)
                        }
                    }
                    holder.layout.isSaveEnabled = false
                    holder.view.isSaveEnabled = false
                    holder.view.id = ViewCompat.generateViewId()
                    holder.caption.isSaveEnabled = false
                    holder.caption.text = param.caption
                    when (param.name) {
                        "title", "author", "free" -> holder.caption.setTypeface(null, Typeface.BOLD)
                    }
                    holder
                }
                searchParamHolders[param.name] = new
                if (param.name == state.focusedChild || focusView == null) focusView = new.view
            }
        }
        focusView?.requestFocus()


        val portMode = resources.getBoolean(R.bool.port_mode)
        val displayWidth = getDisplaySize().x
        var minimumWidth = 0
        for (h in searchParamHolders.values) {
            val captionWidth = h.caption.paint.measureText(h.caption.text.toString()).roundToInt()
            if (portMode || (displayWidth > 0 && captionWidth > displayWidth / 3) ) {
                h.layout.orientation = LinearLayout.VERTICAL
                h.caption.layoutParams = LinearLayout.LayoutParams(LinearLayout.LayoutParams.MATCH_PARENT, LinearLayout.LayoutParams.WRAP_CONTENT)
                h.view.layoutParams = LinearLayout.LayoutParams(LinearLayout.LayoutParams.MATCH_PARENT, LinearLayout.LayoutParams.WRAP_CONTENT)
            } else {
                h.layout.orientation = LinearLayout.HORIZONTAL
                h.caption.layoutParams = LinearLayout.LayoutParams(LinearLayout.LayoutParams.WRAP_CONTENT, LinearLayout.LayoutParams.WRAP_CONTENT)
                h.view.layoutParams = LinearLayout.LayoutParams(LinearLayout.LayoutParams.MATCH_PARENT, LinearLayout.LayoutParams.WRAP_CONTENT)
                if (captionWidth > minimumWidth) minimumWidth = captionWidth
            }
        }
        if (!portMode) {
            minimumWidth += 10 // some border
            for (h in searchParamHolders.values)
                h.caption.minimumWidth = minimumWidth + 10
        }

    }


    override fun onSearchEvent(event: SearchEvent): Boolean {
        if (debugTester?.onSearchEvent(event) == true) return true
        if (event.searcherAccess !== searcher) return false
        event.searcherAccess?.let { s ->
            when (event) {
                is SearchEvent.Connected ->  {
                    s.heartBeat = System.currentTimeMillis()
                    s.state = SEARCHER_STATE_CONNECTED
                    s.searchParams = event.params
                    updateSearchParamsViews()
                    refreshLoadingIcon()
                    return true
                }
                is SearchEvent.Exception -> {
                    s.state = SEARCHER_STATE_FAILED
                    searcher = null
                    gcSearchers()
                    VideLibriApp.showPendingExceptions()
                    refreshLoadingIcon()
                    return true
                }
                else -> {}
            }
        }
        return false
    }


    internal var debugTester: SearchDebugTester? = null
    private fun activateHiddenDebugMode() {
        showDialog("You have activated the secret debug mode") {
            onDismiss = {
                showMessageYesNo("Do you want to search ALL libraries? ") {
                    withActivity<Search> {
                        val book = Bridge.Book()
                        book.title = searchParamHolders["title"]?.edit?.text.toString()
                        book.author = searchParamHolders["author"]?.edit?.text.toString()
                        debugTester = SearchDebugTester(book, state.libId)
                    }
                }
            }
        }
    }

    override val isLoading: Boolean
        get() = super.isLoading || searcher?.state == SEARCHER_STATE_INIT

    override fun listLoadingTasksStrings(tasks: MutableList<Int>) {
        super.listLoadingTasksStrings(tasks)
        if (searcher?.state == SEARCHER_STATE_INIT) tasks += R.string.loading_search_connecting
    }

    companion object {
        internal var searchers = ArrayList<SearcherAccess>()

        internal const val SEARCHER_HEARTH_BEAT_TIMEOUT = 5 * 60 * 1000
        internal const val SEARCHER_STATE_INIT = 0 //connecting
        internal const val SEARCHER_STATE_CONNECTED = 1
        internal const val SEARCHER_STATE_SEARCHING = 2
        internal const val SEARCHER_STATE_FAILED = 3
    }
}




internal class SearchDebugTester(private var query: Bridge.Book, startId: String) {
    private var libs: Array<String> = Bridge.VLGetLibraryIds()
    var pos: Int = 0
    private var searcher: SearcherAccess? = null

    init {
        pos = 0
        while (pos < libs.size && startId != libs[pos]) pos++
        start()
    }

    private fun start() {
        while (true) {
            val lib = Bridge.VLGetLibraryDetails(libs[pos])
            if (lib?.searchMightWork == true) break
            pos++
            if (pos >= libs.size) return
        }
        Log.i("VIDELIBRI", "============================================================")
        Log.i("VIDELIBRI", "Testing search: " + libs[pos])
        Log.i("VIDELIBRI", "============================================================")
        searcher = SearcherAccess(libs[pos])
        searcher?.connect()
        withActivity<Search> {
            state.libId = libs[pos]
            state.libName = libs[pos]
            binding.library.text = state.libName
            refreshLoadingIcon()
        }
    }

    fun onSearchEvent(event: SearchEvent): Boolean {
        if (event.searcherAccess !== searcher) return false
        val currentSearcher = searcher ?: return false
        when (event) {
            is SearchEvent.Connected -> currentSearcher.start(query)
            is SearchEvent.FirstPage -> {
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
            is SearchEvent.NextPage -> {
                currentSearcher.free()
                pos++
                if (pos < libs.size)
                    start()
                else
                    endComplete()
            }
            is SearchEvent.Exception -> {
                currentSearcher.free()
                endComplete()
                VideLibriApp.showPendingExceptions()
            }
            else -> {}
        }
        return true
    }

    private fun endComplete() {
        searcher = null
        withActivity<Search> {
            debugTester = null
            refreshLoadingIcon()
        }
    }
}
