package de.benibela.videlibri
import android.app.Activity
import android.content.Context
import android.os.Bundle
import android.widget.ArrayAdapter
import android.widget.Spinner

fun showMessage(
        message: String? = null,
        title: String? = null,
        dialogId: Int = 0,
        negative: String? = null,
        neutral: String? = null,
        positive: String? = null,
        more: Bundle? = null
) {
    var neutralButton = neutral
    if (negative == null && neutral == null && positive == null)
        neutralButton = Util.tr(R.string.ok)

    val args = android.os.Bundle()
    args.putInt("id", dialogId)
    args.putString("message", message)
    args.putString("title", title)
    args.putString("negativeButton", negative)
    args.putString("neutralButton", neutralButton)
    args.putString("positiveButton", positive)
    if (more != null)
        args.putBundle("more", more)
    Util.showDialog(args)
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