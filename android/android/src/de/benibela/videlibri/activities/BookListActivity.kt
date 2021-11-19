package de.benibela.videlibri.activities

import android.annotation.SuppressLint
import android.content.Intent
import android.os.Build
import android.os.Bundle
import android.os.Parcelable
import android.util.Log
import android.view.*
import android.widget.AdapterView
import android.widget.Button
import android.widget.ListView
import androidx.recyclerview.widget.DividerItemDecoration
import androidx.recyclerview.widget.LinearLayoutManager
import de.benibela.videlibri.R
import de.benibela.videlibri.components.BookDetails
import de.benibela.videlibri.components.BookListFragment
import de.benibela.videlibri.components.BookOverviewAdapter
import de.benibela.videlibri.jni.BookListDisplayOptions
import de.benibela.videlibri.jni.Bridge
import de.benibela.videlibri.utils.Clipboard
import kotlinx.android.parcel.Parcelize
import java.util.*

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
            var selectedBooksIndices: List<Int>? = null
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

        for (listview in listOf(list.listview, details.listview)) {
            registerForContextMenu(listview)
            listview.layoutManager = LinearLayoutManager(this)
            listview.addItemDecoration(DividerItemDecoration(this, DividerItemDecoration.VERTICAL))
            listview.addOnItemLongClickListener { _, vh ->
                contextMenuSelectedItem = if (listview == details.listview)
                    details.adapter?.details?.getOrNull(vh.adapterPosition - 1)
                else
                    list.adapter?.books?.getOrNull(vh.adapterPosition)
                false
            }
        }

        if (port_mode) {
            detailsPortHolder = findViewById(R.id.bookdetailslayout)
            listPortHolder = findViewById(R.id.booklistlayout)
        }

        list.listview.addOnItemClickListener { _, vh ->
            val i = vh.adapterPosition
            if (bookCache.getOrNull(i)?.isGroupingHeader == false)
                viewDetails(i)
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
            state.listFirstItem = (list.listview.layoutManager as? LinearLayoutManager)?.findFirstCompletelyVisibleItemPosition() ?: 0
            //perhaps better use the list view save/restore state function??
        }
        selectedBooks?.let { sb ->
            state.selectedBooksIndices = bookCache.mapIndexedNotNull { i, book -> i.takeIf { sb.contains(book) } }
        }
        super.onSaveInstanceState(outState)
    }

    override fun onCreateOptionsMenuOverflow(menu: Menu, inflater: MenuInflater) {
        super.onCreateOptionsMenuOverflow(menu, inflater)
        inflater.inflate(R.menu.booklistmenu, menu)
    }

    fun currentBook(): Bridge.Book? =
        if (detailsVisible()) details.book
        else bookCache.getOrNull(state.currentBookPos)


    override fun onOptionsItemIdSelected(id: Int): Boolean {
        when (id) {
            R.id.share -> {
                val sendIntent = Intent()
                sendIntent.action = Intent.ACTION_SEND
                sendIntent.putExtra(Intent.EXTRA_TEXT,
                        (if (listVisible()) list.exportShare(false) + "\n\n" else "")
                                + (if (detailsVisible()) details.exportShare(false) + "\n\n" else "")
                                + getString(R.string.share_export_footer)
                )
                if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.JELLY_BEAN)
                    sendIntent.putExtra(Intent.EXTRA_HTML_TEXT,
                            ((if (listVisible()) list.exportShare(true) + "<br>\n<br>\n" else "")
                                    + (if (detailsVisible()) details.exportShare(true) + "<br>\n<br>\n" else "")
                                    + getString(R.string.share_export_footer))
                    )
                sendIntent.type = "text/plain"
                startActivity(Intent.createChooser(sendIntent, getText(R.string.menu_share)))
                return true
            }
        }
        return super.onOptionsItemIdSelected(id)
    }

    fun displayBookCache(expectedCount: Int) {
        //Log.i("VL","Book count: "+partialSize);
        list.adapter = BookOverviewAdapter(this, bookCache, expectedCount, displayOptions)
        if (!cacheShown) {
            cacheShown = true
            onBookCacheAvailable()
        }
    }

    internal fun displayBookCache() {
        displayBookCache(bookCache.size)
    }

    open fun onPlaceHolderShown(position: Int) {}

    //called when the user touches a book
    open fun viewDetails(bookpos: Int) {
        showDetails(bookpos)
    }

    //shows the detail view
    private fun showDetails(startBookPos: Int) {
        val bookPos = (startBookPos until bookCache.size).find { !bookCache[it].isGroupingHeader } ?: return
        state.currentBookPos = bookPos
        if (port_mode) {
            detailsPortHolder?.visibility = View.VISIBLE
            listPortHolder?.visibility = View.INVISIBLE
            state.portInDetailMode = true
        }
        details.book = bookCache[bookPos]
        invalidateOptionsMenu()
    }


    open fun onBookCacheAvailable() {
        //Log.d("VideLIBRI", "onBookCacheAvailable" + currentBookPos + " / " + listFirstItem + " / " + bookCache.size() );
        state.selectedBooksIndices?.let {sbi ->
            selectedBooks = sbi.mapNotNull { bookCache.getOrNull(it) }.union(selectedBooks ?: setOf()).toMutableSet()
            state.selectedBooksIndices = null
            list.adapter?.notifyDataSetChanged()
        }
        if (!port_mode || ( state.portInDetailMode && state.currentBookPos in bookCache.indices ) )
            showDetails(state.currentBookPos)
        else
            showList()
        de.benibela.videlibri.utils.runOnUiThread {
            if (state.listFirstItem > 0)
                list.listview.smoothScrollToPosition(state.listFirstItem)
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
        toCopy?.let { Clipboard.text = it }
        contextMenuSelectedItem = null
        return super.onContextItemSelected(item)
    }

    //shows the list. returns if the list was already visible
    fun showList(): Boolean =
        if (!port_mode) true
        else if (detailsVisible()) {
            listPortHolder?.visibility = View.VISIBLE
            detailsPortHolder?.visibility = View.INVISIBLE
            state.portInDetailMode = false
            invalidateOptionsMenu()
            false
        } else
            true


    override fun onBackPressed() {
        if (showList())
            super.onBackPressed()
    }

    fun detailsVisible(): Boolean =
            if (!port_mode) true else detailsPortHolder?.visibility == View.VISIBLE


    fun listVisible(): Boolean =
            if (!port_mode) true else !detailsVisible()


    @JvmField var bookCache = ArrayList<Bridge.Book>()
    var displayOptions = BookListDisplayOptions()

    var selectedBooks: MutableSet<Bridge.Book>? = null

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
