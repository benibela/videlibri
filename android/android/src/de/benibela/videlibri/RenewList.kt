package de.benibela.videlibri

import android.os.Bundle
import android.preference.PreferenceManager
import android.view.View
import android.widget.Button
import android.widget.ListView
import de.benibela.videlibri.jni.Bridge
import java.util.ArrayList

class RenewList : BookListActivity() {
    lateinit var button: Button

    private var truecount: Int = 0

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)

        title = tr(R.string.renew_title_start)
        selectedBooks = ArrayList()
        updateViewFilters()


        button = findViewById(R.id.buttonbelowlist)
        button.apply {
            visibility = View.VISIBLE
            isEnabled = false
            text = tr(R.string.booklist_noselection)
            setOnClickListener {
                VideLibriApp.renewBooks(selectedBooks?.toTypedArray() ?: return@setOnClickListener)
                finish()
            }
        }
    }

    override fun onResume() {
        super.onResume()
        updateViewFilters()
        displayBookCache()
    }

    override fun onBookCacheAvailable() {
        super.onBookCacheAvailable()
        updateRenewButton()
    }

    private fun updateViewFilters() {
        val sp = PreferenceManager.getDefaultSharedPreferences(this)
        displayOptions.readFromPreferences(sp)
        displayOptions.filterKey = ""
        val oldSelection = selectedBooks
        bookCache = makePrimaryBookCache(false, true)
        truecount = bookCache.size
        bookCache = displayOptions.let { filterToSecondaryBookCache(bookCache, it.groupingKey, it.sortingKey, "", "") }
        val newSelection = ArrayList<Bridge.Book>(selectedBooks?.size?:0)
        oldSelection?.forEach {selbook ->
            for (book in bookCache)
                if (selbook.equalsBook(book)) {
                    newSelection.add(book)
                    break
                }
        }
        selectedBooks = newSelection
    }

    override fun viewDetails(bookpos: Int) {
        val book = bookCache[bookpos]
        val deleteIndex = selectedBooks?.indexOfFirst{ it === book} ?: -1
        if (deleteIndex == -1) selectedBooks?.add(book)
        else selectedBooks?.removeAt(deleteIndex)
        updateRenewButton()
        (findViewById<ListView>(R.id.booklistview).adapter as BookOverviewAdapter).notifyDataSetChanged()
        if (!port_mode) super.viewDetails(bookpos)
    }


    private fun updateRenewButton() {
        if (selectedBooks?.size == 0) {
            title = tr(R.string.renew_title_select)
            button.isEnabled = false
            button.text = tr(R.string.renew_noselection)
        } else {
            title = tr(R.string.renew_title_selectionDD, selectedBooks?.size, truecount)
            button.isEnabled = true
            button.text = tr(R.string.renew_renewD, selectedBooks?.size)
        }
    }
}
