package de.benibela.videlibri

import android.app.Activity
import android.content.Intent
import android.graphics.Paint
import android.os.Bundle
import android.preference.PreferenceManager
import android.support.v7.app.ActionBar
import android.util.Log
import android.view.Menu
import android.view.View
import android.widget.Spinner
import android.widget.TextView
import de.benibela.videlibri.jni.Bridge
import java.util.*
import android.view.Gravity



internal interface SearchEventHandler {
    fun onSearchEvent(event: Bridge.SearchEvent): Boolean
}
class Search: VideLibriBaseActivity(), SearchEventHandler{
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

        val defaultQuery = intent?.getSerializableExtra("query")
        if (defaultQuery is Map<*, *>) {
            findViewById<TextView>(R.id.title).text = defaultQuery.get("title").toString()
            findViewById<TextView>(R.id.author).text = defaultQuery.get("author").toString()
        }

        findViewById<View>(R.id.library).setOnClickListener( { changeSearchLib() })

        val searchStartClickListener = View.OnClickListener {
            obtainSearcher()

            if ("debug" == getTextViewText(R.id.year)) {
                activateHiddenDebugMode()
                return@OnClickListener
            }

            val intent = Intent(this@Search, SearchResult::class.java)
            val book = Bridge.Book()
            book.title = getTextViewText(R.id.title)
            book.author = getTextViewText(R.id.author)
            book.setProperty("keywords", getTextViewText(R.id.keywords))
            book.year = getTextViewText(R.id.year)
            book.setProperty("isbn", getTextViewText(R.id.isbn))
            intent.putExtra("searchQuery", book)

            for (i in BRANCH_NAMES.indices) {
                if (findViewById<View>(BRANCH_LAYOUT_IDS[i]).getVisibility() != View.GONE) {
                    val spinner = findViewById<Spinner>(BRANCH_IDS[i])
                    intent.putExtra(BRANCH_NAMES[i], spinner.selectedItemPosition)
                    val branch = spinner.selectedItem as? String ?: continue
                    val sp = PreferenceManager.getDefaultSharedPreferences(this@Search)
                    val editor = sp.edit()
                    editor.putString("Search|" + libId + "|" + BRANCH_NAMES[i], branch)
                    editor.apply()
                }
            }
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
            setBranchViewes(true)
        }
    }

    override fun onSaveInstanceState(outState: Bundle?) {
        super.onSaveInstanceState(outState)
        outState?.apply{
            putString("libId", libId);
            putString("libName", libName);
        }
    }

    internal val REQUEST_CHOOSE_LIBRARY = 1234

    internal var libId: String = ""
    internal var libName: String = ""

    internal var searcher: Bridge.SearcherAccess? = null

    internal val BRANCH_NAMES = arrayOf("homeBranch", "searchBranch")
    internal val BRANCH_LAYOUT_IDS = intArrayOf(R.id.homeBranchLayout, R.id.searchBranchLayout)
    internal val BRANCH_IDS = intArrayOf(R.id.homeBranch, R.id.searchBranch)

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

    internal fun setBranchViewes(init: Boolean) {
        val sp = if (init) PreferenceManager.getDefaultSharedPreferences(this) else null
        for (i in BRANCH_NAMES.indices) {
            val branches: Array<String>? = if (BRANCH_IDS[i] == R.id.homeBranch) searcher?.homeBranches else searcher?.searchBranches
            if (branches != null && branches.isNotEmpty()) {
                val spinner = findViewById<Spinner>(BRANCH_IDS[i])
                val current = if (init) sp?.getString("Search|" + libId + "|" + BRANCH_NAMES[i], null) else spinner.selectedItem as? String

                findViewById<View>(BRANCH_LAYOUT_IDS[i]).visibility = View.VISIBLE
                spinner.setItems(branches)
                spinner.setSelection(current, branches)
            } else
                findViewById<View>(BRANCH_LAYOUT_IDS[i]).visibility = View.GONE
        }
    }

    override fun onActivityResult(requestCode: Int, resultCode: Int, data: Intent?) {
        if (requestCode == REQUEST_CHOOSE_LIBRARY) {
            if (resultCode == Activity.RESULT_OK) {
                libId = LibraryList.lastSelectedLibId ?: ""
                libName = LibraryList.lastSelectedLibName ?: ""
                findViewById<TextView>(R.id.library).setText(libName)

                obtainSearcher()
                setBranchViewes(true)
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
                    s.homeBranches = event.obj1 as? Array<String>
                    s.searchBranches = event.obj2 as? Array<String>
                    setBranchViewes(false)
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
                        book.title = findViewById<TextView>(R.id.title).text.toString()
                        book.author = findViewById<TextView>(R.id.author).text.toString()
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
            Bridge.SearchEventKind.CONNECTED -> currentSearcher.start(query, -1, -1)
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
