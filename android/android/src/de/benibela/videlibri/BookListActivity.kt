package de.benibela.videlibri

import android.annotation.SuppressLint
import android.content.Intent
import android.content.SharedPreferences
import android.os.Build
import android.os.Bundle
import android.os.Parcelable
import android.util.Log
import android.view.*
import android.widget.AdapterView
import android.widget.Button
import android.widget.ListView
import de.benibela.videlibri.jni.Bridge
import kotlinx.android.parcel.Parcelize
import java.util.*

data class BookListDisplayOptions(
        var noDetailsInOverview: Boolean = false,
        var showRenewCount: Boolean = false,
        var groupingKey: String = "",
        var sortingKey: String = "",
        var filterKey: String = ""
)
{
    fun isGrouped() = groupingKey != ""
    fun readFromPreferences(sp: SharedPreferences){
        noDetailsInOverview = sp.getBoolean("noLendBookDetails", false)
        showRenewCount = sp.getBoolean("showRenewCount", true)
        sortingKey = sp.getString("sorting", "dueDate")
        groupingKey = sp.getString("grouping", "_dueWeek")
        filterKey = sp.getString("filtering", "")
    }
}

@SuppressLint("Registered")
open class BookListActivity: VideLibriBaseActivity(){
    internal var port_mode: Boolean = false

    internal lateinit var list: BookListFragment
    internal lateinit var details: BookDetails
    private var detailsPortHolder: View? = null
    private var listPortHolder: View? = null

    @Parcelize
    class State(
            var portInDetailMode: Boolean = false,
            var currentBookPos: Int = 0,
            var listFirstItem: Int = 0,
            var selectedBooksIndices: ArrayList<Int>? = null
    ): Parcelable
    var state = State()

    public override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        Log.d("VL", "onCreate: $port_mode")
        setVideLibriView(R.layout.booklistactivity)
        registerState(::state)
        port_mode = resources.getBoolean(R.bool.port_mode)
        list = BookListFragment(this)
        details = BookDetails(this)


        if (port_mode) {
            detailsPortHolder = findViewById(R.id.bookdetailslayout)
            listPortHolder = findViewById(R.id.booklistlayout)
        }
    }

    override fun onResume() {
        super.onResume()
        port_mode = resources.getBoolean(R.bool.port_mode) //should not have changed
    }

    override fun onSaveInstanceState(outState: Bundle) {
        if (state.listFirstItem == 0) {
            //only use new position, if there is no old position.
            //when the device rotates too fast, the activity is recreated (here), before the listview is initialized
            state.listFirstItem = findViewById<ListView>(R.id.booklistview)?.firstVisiblePosition ?: 0
            //perhaps better use the list view save/restore state function??
        }
        selectedBooks?.let {
            val selindices = ArrayList<Int>(it.size)
            for (j in bookCache.indices)
                for (i in it)
                    if (i === bookCache[j]) {
                        selindices.add(j)
                        break
                    }
            state.selectedBooksIndices = selindices
        }
        super.onSaveInstanceState(outState)
    }

    override fun onCreateOptionsMenuOverflow(menu: Menu, inflater: MenuInflater) {
        super.onCreateOptionsMenuOverflow(menu, inflater)
        inflater.inflate(R.menu.booklistmenu, menu)
    }

    fun currentBook(): Bridge.Book? =
        if (detailsVisible() && details.book != null) details.book
        else bookCache.getOrNull(state.currentBookPos)


    override fun onOptionsItemIdSelected(id: Int): Boolean {
        when (id) {
            R.id.share -> {
                val sendIntent = Intent()
                sendIntent.action = Intent.ACTION_SEND
                sendIntent.putExtra(Intent.EXTRA_TEXT,
                        (if (listVisible()) list.exportShare(false) + "\n\n" else "")
                                + (if (detailsVisible()) details.exportShare(false) + "\n\n" else "")
                                + tr(R.string.share_export_footer)
                )
                if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.JELLY_BEAN)
                    sendIntent.putExtra(Intent.EXTRA_HTML_TEXT,
                            ((if (listVisible()) list.exportShare(true) + "<br>\n<br>\n" else "")
                                    + (if (detailsVisible()) details.exportShare(true) + "<br>\n<br>\n" else "")
                                    + tr(R.string.share_export_footer))
                    )
                sendIntent.type = "text/plain"
                startActivity(Intent.createChooser(sendIntent, getText(R.string.menu_share)))
                return true
            }
        }
        return super.onOptionsItemIdSelected(id)
    }

    fun displayBookCache(partialSize: Int) {
        //Log.i("VL","Book count: "+partialSize);
        val sa = BookOverviewAdapter(this, bookCache, partialSize, displayOptions)
        val bookListView = findViewById<ListView>(R.id.booklistview)
        bookListView.onItemClickListener = AdapterView.OnItemClickListener { _, _, i, _ ->
            bookCache.getOrNull(i)?.let {
                if ("" != displayOptions.groupingKey && BookFormatter.isGroupingHeaderFakeBook(it))
                    return@OnItemClickListener
                viewDetails(i)
            }
        }
        bookListView.adapter = sa
        if (!cacheShown) {
            cacheShown = true
            onBookCacheAvailable()
        }
    }

    internal fun displayBookCache() {
        displayBookCache(bookCache.size)
    }

    fun updateDisplayBookCache() {
        val bookListView = findViewById<ListView>(R.id.booklistview)
        val sa = bookListView.adapter as BookOverviewAdapter
        sa.setBooks(bookCache)
    }

    open fun onPlaceHolderShown(position: Int) {}

    //called when the user touches a book
    open fun viewDetails(bookpos: Int) {
        showDetails(bookpos)
    }

    //shows the detail view
    fun showDetails(startbookpos: Int) {
        var bookpos = startbookpos
        if (bookpos !in bookCache.indices) return
        while (BookFormatter.isGroupingHeaderFakeBook(bookCache[bookpos])) {
            bookpos++
            if (bookpos >= bookCache.size) return
        }
        state.currentBookPos = bookpos
        if (port_mode) {
            detailsPortHolder?.visibility = View.VISIBLE
            listPortHolder?.visibility = View.INVISIBLE
            state.portInDetailMode = true
        }
        details.setBook(bookCache[bookpos])
        invalidateOptionsMenu()
    }


    open fun onBookCacheAvailable() {
        //Log.d("VideLIBRI", "onBookCacheAvailable" + currentBookPos + " / " + listFirstItem + " / " + bookCache.size() );
        state.selectedBooksIndices?.let {
            val sb = selectedBooks ?:  ArrayList<Bridge.Book>()
            for (i in it)
                bookCache.getOrNull(i)?.let { book -> sb.add(book) }
            selectedBooks = sb
            state.selectedBooksIndices = null
            (findViewById<ListView>(R.id.booklistview).adapter as BookOverviewAdapter).notifyDataSetChanged()
        }
        if (!port_mode || ( state.portInDetailMode && state.currentBookPos in bookCache.indices ) )
            showDetails(state.currentBookPos)
        else
            showList()
        val bookListView = findViewById<ListView>(R.id.booklistview)
        bookListView.post {
            if (state.listFirstItem > 0)
                bookListView.setSelectionFromTop(state.listFirstItem, 0)
            state.listFirstItem = 0
        }
    }

    override fun onContextItemSelected(item: MenuItem): Boolean {
        val toCopy: String? = when (item.itemId) {
            R.id.copy -> contextMenuSelectedItem?.toString()
            R.id.copyall -> if (!detailsVisible() || contextMenuSelectedItem is Bridge.Book)
                list.exportShare(false)
            else if (contextMenuSelectedItem is BookDetails.Details)
                details.exportShare(false)
            else
                list.exportShare(false) + "\n\n" + details.exportShare(false) //this case should not happen
            else -> null
        }
        toCopy?.let { Util.Clipboard.setText(this, it) }
        contextMenuSelectedItem = null
        return super.onContextItemSelected(item)
    }

    //shows the list. returns if the list was already visible
    fun showList(): Boolean {
        if (!port_mode) return true
        if (detailsVisible()) {
            listPortHolder?.visibility = View.VISIBLE
            detailsPortHolder?.visibility = View.INVISIBLE
            state.portInDetailMode = false
            invalidateOptionsMenu()
            return false
        } else
            return true
    }

    override fun onBackPressed() {
        if (showList())
            super.onBackPressed()
    }

    fun detailsVisible(): Boolean {
        return if (!port_mode) true else detailsPortHolder?.visibility == View.VISIBLE
    }

    fun listVisible(): Boolean {
        return if (!port_mode) true else !detailsVisible()
    }


    @JvmField var bookCache = ArrayList<Bridge.Book>()
    val displayOptions = BookListDisplayOptions()

    protected var selectedBooks: ArrayList<Bridge.Book>? = null

    @JvmField var bookActionButton: Button? = null //set from detail fragment

    @JvmField internal var cacheShown = false


    internal var contextMenuSelectedItem: Any? = null
    override fun onCreateContextMenu(menu: ContextMenu?, v: View?, menuInfo: ContextMenu.ContextMenuInfo?) {
        super.onCreateContextMenu(menu, v, menuInfo)
        val inflater = menuInflater
        inflater.inflate(R.menu.detailcontextmenu, menu)
        if (v is ListView && menuInfo is AdapterView.AdapterContextMenuInfo) {
            contextMenuSelectedItem = v.getItemAtPosition(menuInfo.position)
        }
    }


    open fun onBookActionButtonClicked(book: Bridge.Book) {} //called from detail fragment

}
