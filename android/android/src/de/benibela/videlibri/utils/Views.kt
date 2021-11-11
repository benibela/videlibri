package de.benibela.videlibri.utils
import android.util.SparseBooleanArray
import android.view.Menu
import android.view.MenuItem
import android.view.View
import android.view.ViewGroup
import android.widget.ArrayAdapter
import android.widget.ListView
import android.widget.Spinner


fun View.forEachDescendantView(f: (View) -> Unit) {
    f(this)
    if (this is ViewGroup) {
        for (i in 0 until childCount)
            getChildAt(i).forEachDescendantView(f)
    }
}



fun Spinner.setItems(items: Array<String>) {
    val adapter = ArrayAdapter(this.context, android.R.layout.simple_spinner_item, items)
    adapter.setDropDownViewResource(android.R.layout.simple_spinner_dropdown_item)
    this.adapter = adapter
}

fun<T> Spinner.setSelection(item: T?, items: Array<T>) {
    val i = items.indexOf(item)
    if (i > 0) this.setSelection(i)
}

fun ListView.setCheckedItemPositions(positions: SparseBooleanArray) {
    for (i in 0 until count)
        setItemChecked(i, positions.get(i, false))
}

inline fun Menu.forItems(f: (MenuItem) -> Unit){
    for (i in 0 until size())
        f(getItem(i))
}

var View.isVisibleNotGone: Boolean
    get() = this.visibility == View.VISIBLE
    set(visible) {
        this.visibility = if (visible) View.VISIBLE else View.GONE
    }
