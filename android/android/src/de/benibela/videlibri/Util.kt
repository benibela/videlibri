package de.benibela.videlibri
import android.app.Activity
import android.app.AlertDialog
import android.content.Context
import android.content.DialogInterface
import android.os.Bundle
import android.support.annotation.StringRes
import android.util.Log
import android.view.Menu
import android.view.MenuItem
import android.view.View
import android.widget.ArrayAdapter
import android.widget.Spinner
import android.widget.Toast
import de.benibela.videlibri.Util.MessageHandlerCanceled
import de.benibela.videlibri.Util.tr
import java.io.InputStream

fun <T: CharSequence> T.takeNonEmpty(): T? = takeIf(CharSequence::isNotEmpty)


inline fun <reified T> currentActivity(): T? = (VideLibriApp.currentActivity as? T)
inline fun <reified T: Activity> withActivity(f: T.() -> Unit) = currentActivity<T>()?.run(f)

fun showToast(message: CharSequence) =
        Toast.makeText(VideLibriApp.currentContext(), message, Toast.LENGTH_SHORT).show()
fun showToast(@StringRes message: Int) =
        Toast.makeText(VideLibriApp.currentContext(), message, Toast.LENGTH_SHORT).show()

fun getString(@StringRes message: Int): String? = Util.tr(message)

internal typealias DialogEvent = (DialogInstance.(Util.DialogFragmentUtil) -> Unit)
internal typealias DialogInitEvent = (DialogInstance.() -> Unit)
internal typealias DialogFragmentInitEvent = (Util.DialogFragmentUtil.(AlertDialog.Builder) -> Unit)
internal typealias InputDialogEvent = (DialogInstance.(text: String) -> Unit)
internal typealias ChooseDialogEvent = (DialogInstance.(item: Int) -> Unit)

//default dialogs, no customization, optional lambda argument is called after dialog completion (thus it MUST NOT LEAK)

fun showMessage(
        message: String? = null,
        title: String? = null
) = showDialog(message, title)
fun showMessage(
        @StringRes message: Int
) = showDialog(getString(message))

fun showMessageYesNo(
        message: String? = null,
        title: String? = null,
        onYes: DialogEvent
) = showDialog(message, title) {
    noButton()
    yesButton(onYes)
}
fun showMessageYesNo(
        @StringRes message: Int,
        onYes: DialogEvent
) = showMessageYesNo(getString(message), null, onYes)

fun showInputDialog(
        message: String? = null,
        title: String? = null,
        default: String? = null,
        onResult: InputDialogEvent? = null
) = showDialog(message, title) {
    args.putString("editTextDefault", default)
    args.putInt("special", DialogId.SPECIAL_INPUT_DIALOG)
    okButton { fragment ->
        onResult?.invoke(this, fragment.edit?.text?.toString() ?: "")
    }
}
fun showInputDialog(
        @StringRes message: Int,
        title: String? = null,
        default: String? = null,
        onResult: InputDialogEvent? = null
) = showInputDialog(getString(message), title, default, onResult)


fun showChooseDialog(
        title: String?,
        items: List<String>,
        onResult: ChooseDialogEvent
) = showDialog(null, title) {
    this.items(items, onResult)
}

fun showChooseDialog(
        @StringRes title: Int,
        items: List<String> ,
        onResult: ChooseDialogEvent
) = showChooseDialog(getString(title), items, onResult)

//Customizable dialog, lambda runs before dialog creation

fun showDialog(
        message: String? = null,
        title: String? = null,
        dialogId: Int = 0,
        negative: String? = null,
        neutral: String? = null,
        positive: String? = null,
        more: Bundle? = null,
        init: DialogInitEvent? = null
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
    if ((args.get("negativeButton")
                    ?: args.get("neutralButton")
                    ?: args.get("positiveButton")
                    ?: args.get("items")
                    ) == null)
        args.putString("neutralButton", Util.tr(R.string.ok))
    Util.showPreparedDialog(args)
}

private var totalDialogInstances = 0
private val dialogInstances = mutableMapOf<Int, DialogInstance>()

@Suppress("unused")
data class DialogInstance (
        val args: Bundle
) {
    var onNegativeButton: DialogEvent? = null
    var onNeutralButton: DialogEvent? = null
    var onPositiveButton: DialogEvent? = null
    var onDismiss: DialogEvent? = null
    var onCancel: DialogEvent? = null
    var onItem: ChooseDialogEvent? = null
    var onCreate: DialogFragmentInitEvent? = null

    fun message(caption: String) = args.putString("message", caption)
    fun message(@StringRes caption: Int,  vararg a: Any?) = message(tr(caption, *a))

    fun items(items: List<String>, onItem: ChooseDialogEvent? = null) {
        args.putStringArray("items", items.toTypedArray())
        this.onItem = onItem
    }

    fun negativeButton(caption: String, onClicked: DialogEvent? = null){
        args.putString("negativeButton", caption)
        onNegativeButton = onClicked
    }
    fun negativeButton(@StringRes caption: Int, onClicked: DialogEvent? = null) =
        negativeButton(tr(caption), onClicked)


    fun neutralButton(caption: String, onClicked: DialogEvent? = null){
        args.putString("neutralButton", caption)
        onNeutralButton = onClicked
    }
    fun neutralButton(@StringRes caption: Int, onClicked: DialogEvent? = null) =
        neutralButton(tr(caption), onClicked)


    fun positiveButton(caption: String, onClicked: DialogEvent? = null){
        args.putString("positiveButton", caption)
        onPositiveButton = onClicked
    }
    fun positiveButton(@StringRes caption: Int, onClicked: DialogEvent? = null) =
        positiveButton(tr(caption), onClicked)


    fun noButton(onClicked: DialogEvent? = null) = negativeButton(R.string.no, onClicked)
    fun yesButton(onClicked: DialogEvent? = null) = positiveButton(R.string.yes, onClicked)
    fun okButton(onClicked: DialogEvent? = null) = neutralButton(R.string.ok, onClicked)

    companion object {
        @JvmStatic fun onPreCreate(dialogFragment: Util.DialogFragmentUtil, builder: AlertDialog.Builder){
            val instanceId = dialogFragment.arguments?.getInt("instanceId", -1) ?: -1
            if (instanceId < 0) return
            dialogFragment.instance = dialogInstances.get(instanceId)
            dialogFragment.instance?.onCreate?.invoke(dialogFragment, builder)
        }
        @JvmStatic fun onFinished(dialogFragment: Util.DialogFragmentUtil, button: Int){
            dialogFragment.instance?.apply {
                when (button) {
                    DialogInterface.BUTTON_NEGATIVE -> onNegativeButton?.invoke(this, dialogFragment)
                    DialogInterface.BUTTON_NEUTRAL -> onNeutralButton?.invoke(this, dialogFragment)
                    DialogInterface.BUTTON_POSITIVE -> onPositiveButton?.invoke(this, dialogFragment)
                    MessageHandlerCanceled -> onCancel?.invoke(this, dialogFragment)
                }
                if (button >= 0 && onItem != null && args.containsKey("items")) onItem?.invoke(this, button)
                onDismiss?.invoke(this, dialogFragment)
                dialogInstances.remove(dialogFragment.arguments?.getInt("instanceId", -1))
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

var View.isVisibleNotGone: Boolean
    get() = this.visibility == View.VISIBLE
    set(visible) {
        this.visibility = if (visible) View.VISIBLE else View.GONE
    }

fun streamToString(stream: InputStream): String = stream.bufferedReader().use { it.readText() }

