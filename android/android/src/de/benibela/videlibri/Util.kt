package de.benibela.videlibri
import android.app.Activity
import android.content.Context
import android.os.Bundle

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
