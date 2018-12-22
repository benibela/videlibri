package de.benibela.videlibri
import android.app.Activity
import android.content.Context
import android.content.DialogInterface
import android.os.Bundle
import android.support.annotation.StringRes
import android.view.Menu
import android.view.MenuItem
import android.widget.ArrayAdapter
import android.widget.Spinner
import android.widget.Toast
import de.benibela.videlibri.Util.tr
import java.io.InputStream

inline fun <reified T> currentActivity(): T? = (VideLibriApp.currentActivity as? T)
inline fun <reified T: Activity> withActivity(f: T.() -> Unit) = currentActivity<T>()?.run(f)

fun showToast(message: CharSequence) =
        Toast.makeText(VideLibriApp.currentContext(), message, Toast.LENGTH_SHORT).show()
fun showToast(@StringRes message: Int) =
        Toast.makeText(VideLibriApp.currentContext(), message, Toast.LENGTH_SHORT).show()

fun getString(@StringRes message: Int): String? = Util.tr(message)

fun showMessage(
        message: String? = null,
        title: String? = null
) = showDialog(message, title)
fun showMessage(
        @StringRes message: Int
) = showDialog(getString(message))


fun showDialog(
        message: String? = null,
        title: String? = null,
        dialogId: Int = 0,
        negative: String? = null,
        neutral: String? = null,
        positive: String? = null,
        more: Bundle? = null,
        init: (DialogInstance.() -> Unit)? = null
) {
    val args = android.os.Bundle()
    args.putInt("id", dialogId)
    val instanceId = ++totalDialogInstances
    args.putInt("instanceId", instanceId)
    args.putString("message", message)
    args.putString("title", title)
    args.putString("negativeButton", negative)
    args.putString("neutralButton", neutral)
    args.putString("positiveButton", positive)
    if (more != null)
        args.putBundle("more", more)

    val instance = DialogInstance(args)
    dialogInstances.put(instanceId, instance)
    init?.invoke(instance)
    if (args.getString("negativeButton") == null && args.getString("neutralButton") == null && args.getString("positiveButton") == null)
        args.putString("neutralButton", Util.tr(R.string.ok))
    Util.showPreparedDialog(args)
}

private var totalDialogInstances = 0
private val dialogInstances = mutableMapOf<Int, DialogInstance>()

@Suppress("unused")
data class DialogInstance (
        val args: Bundle
) {
    var onNegativeButton: (DialogInstance.() -> Unit)? = null
    var onNeutralButton: (DialogInstance.() -> Unit)? = null
    var onPositiveButton: (DialogInstance.() -> Unit)? = null
    var onDismiss: (DialogInstance.() -> Unit)? = null

    fun message(caption: String) = args.putString("message", caption)
    fun message(@StringRes caption: Int,  vararg a: Any?) = message(tr(caption, *a))


    fun negativeButton(caption: String, onClicked: (DialogInstance.() -> Unit)? = null){
        args.putString("negativeButton", caption)
        onNegativeButton = onClicked
    }
    fun negativeButton(@StringRes caption: Int, onClicked: (DialogInstance.() -> Unit)? = null) =
        negativeButton(tr(caption), onClicked)


    fun neutralButton(caption: String, onClicked: (DialogInstance.() -> Unit)? = null){
        args.putString("neutralButton", caption)
        onNeutralButton = onClicked
    }
    fun neutralButton(@StringRes caption: Int, onClicked: (DialogInstance.() -> Unit)? = null) =
        neutralButton(tr(caption), onClicked)


    fun positiveButton(caption: String, onClicked: (DialogInstance.() -> Unit)? = null){
        args.putString("positiveButton", caption)
        onPositiveButton = onClicked
    }
    fun positiveButton(@StringRes caption: Int, onClicked: (DialogInstance.() -> Unit)? = null) =
        positiveButton(tr(caption), onClicked)


    fun noButton(onClicked: (DialogInstance.() -> Unit)? = null) = negativeButton(R.string.no, onClicked)
    fun yesButton(onClicked: (DialogInstance.() -> Unit)? = null) = positiveButton(R.string.yes, onClicked)
    fun okButton(onClicked: (DialogInstance.() -> Unit)? = null) = neutralButton(R.string.ok, onClicked)

    companion object {
        @JvmStatic fun onFinished(dialogFragment: Util.DialogFragmentUtil, button: Int){
            val instanceId = dialogFragment.arguments?.getInt("instanceId", -1) ?: -1
            if (instanceId < 0) return
            dialogInstances.get(instanceId)?.apply {
                when (button) {
                    DialogInterface.BUTTON_NEGATIVE -> onNegativeButton?.invoke(this)
                    DialogInterface.BUTTON_NEUTRAL -> onNeutralButton?.invoke(this)
                    DialogInterface.BUTTON_POSITIVE -> onPositiveButton?.invoke(this)
                }
                onDismiss?.invoke(this)
                dialogInstances.remove(instanceId)
            }
        }
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

inline fun Menu.forItems(f: (MenuItem) -> Unit){
    for (i in 0 until size())
        f(getItem(i))
}


fun streamToString(stream: InputStream): String = stream.bufferedReader().use { it.readText() }

