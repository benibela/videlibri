package de.benibela.videlibri

import de.benibela.multilevellistview.ClickableRecyclerView

class BookListFragment internal constructor(activity: BookListActivity) : VideLibriFakeFragment(activity) {
    val listview: ClickableRecyclerView = activity.findViewById(R.id.bookOverviewRecyclerView)
    internal var adapter
        get() = listview.adapter as? BookOverviewAdapter
        set(value) { listview.adapter = value }

    fun exportShare(html: Boolean): String {
        val adapter = adapter
        return adapter?.exportShare(html) ?: ""
    }
}