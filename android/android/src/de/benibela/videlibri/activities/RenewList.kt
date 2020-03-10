package de.benibela.videlibri.activities

import android.os.Bundle
import android.preference.PreferenceManager
import android.view.View
import android.widget.Button
import de.benibela.videlibri.R
import de.benibela.videlibri.VideLibriApp
import de.benibela.videlibri.utils.filterToSecondaryBookCache
import de.benibela.videlibri.utils.makePrimaryBookCache

class RenewList : BookListActivity() {
    lateinit var button: Button

    private var trueCount: Int = 0

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)

        title = tr(R.string.renew_title_start)
        selectedBooks = mutableSetOf()
        updateViewFilters()


        button = findViewById(R.id.buttonbelowlist)
        button.apply {
            visibility = View.VISIBLE
            isEnabled = false
            text = tr(R.string.booklist_noselection)
            setOnClickListener {
                VideLibriApp.renewBooks(selectedBooks?.toTypedArray()
                        ?: return@setOnClickListener)
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
        trueCount = bookCache.size
        bookCache = displayOptions.let { filterToSecondaryBookCache(bookCache, it.groupingKey, it.sortingKey, "", "") }

        selectedBooks = oldSelection?.mapNotNull { selBook -> bookCache.find { book -> selBook.equalsBook(book) } }?.toMutableSet() ?: mutableSetOf()
    }

    override fun viewDetails(bookpos: Int) {
        val book = bookCache[bookpos]
        selectedBooks?.let { sb ->
            if (sb.contains(book)) sb.remove(book)
            else sb.add(book)
        }
        updateRenewButton()
        list.adapter?.notifyDataSetChanged()
        if (!port_mode) super.viewDetails(bookpos)
    }


    private fun updateRenewButton() {
        if (selectedBooks?.size == 0) {
            title = tr(R.string.renew_title_select)
            button.isEnabled = false
            button.text = tr(R.string.renew_noselection)
        } else {
            title = tr(R.string.renew_title_selectionDD, selectedBooks?.size, trueCount)
            button.isEnabled = true
            button.text = tr(R.string.renew_renewD, selectedBooks?.size)
        }
    }
}
