package de.benibela.videlibri
import android.app.Activity
import android.app.AlertDialog
import android.app.Dialog
import android.app.NotificationManager
import android.content.Context
import android.content.DialogInterface
import android.content.SharedPreferences
import android.content.res.AssetManager
import android.os.Bundle
import android.preference.PreferenceManager
import androidx.annotation.StringRes
import androidx.fragment.app.DialogFragment
import androidx.appcompat.app.AppCompatActivity
import android.util.SparseBooleanArray
import android.view.Menu
import android.view.MenuItem
import android.view.View
import android.widget.*
import de.benibela.videlibri.Util.MessageHandlerCanceled
import de.benibela.videlibri.Util.tr
import java.io.IOException
import java.io.InputStream

fun <T: CharSequence> T.takeNonEmpty(): T? = takeIf(CharSequence::isNotEmpty)



inline fun <reified T> currentActivity(): T? = (VideLibriApp.currentActivity as? T)
inline fun <reified T: Activity> withActivity(f: T.() -> Unit) = currentActivity<T>()?.run(f)

fun showToast(message: CharSequence) =
        Toast.makeText(VideLibriApp.currentContext(), message, Toast.LENGTH_SHORT).show()
fun showToast(@StringRes message: Int) =
        Toast.makeText(VideLibriApp.currentContext(), message, Toast.LENGTH_SHORT).show()

fun getString(@StringRes message: Int): String? = Util.tr(message)

internal typealias DialogInitEvent = (DialogInstance.() -> Unit)
internal typealias DialogEvent = (DialogFragmentUtil.() -> Unit)
internal typealias DialogFragmentInitEvent = (DialogFragmentUtil.(AlertDialog.Builder) -> Unit)
internal typealias InputDialogEvent = (DialogFragmentUtil.(text: String) -> Unit)
internal typealias ChooseDialogEvent = (DialogFragmentUtil.(item: Int) -> Unit)

//default dialogs, no customization, optional lambda argument is called after dialog completion (thus it MUST NOT LEAK)
@JvmOverloads
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
    editWithOkButton(default, onResult)
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
        args.putString("neutralButton", getString(R.string.ok))
    showPreparedDialog(args)
}

private var totalDialogInstances = 0
private val dialogInstances = mutableMapOf<Int, DialogInstance>()

@Suppress("unused")
data class DialogInstance (
        private val args: Bundle
) {
    internal var flags = 0
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

    fun editWithOkButton(default: String? = null, onResult: InputDialogEvent? = null){
        flags = flags or FLAG_INPUT_DIALOG
        args.putString("editTextDefault", default)
        okButton {
            onResult?.invoke(this, edit?.text?.toString() ?: "")
        }
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
        const val FLAG_INPUT_DIALOG = 2
    }
}

class DialogFragmentUtil : DialogFragment(), DialogInterface.OnClickListener, DialogInterface.OnCancelListener {
    internal var instance: DialogInstance? = null
    internal var edit: EditText? = null
    override fun onCreateDialog(savedInstanceState: Bundle?): Dialog {
        val args = arguments ?: return Dialog(activity)
        val builder = AlertDialog.Builder(activity)
        builder.setOnCancelListener(this)
        args.getInt("instanceId", -1).takeIf { it >= 0 }?.let {
            instance = dialogInstances.get(it)
        }

        args.getString("title")?.let { builder.setTitle(it) }
        args.getString("message")?.let { builder.setMessage(it) }
        args.getStringArray("items")?.let { builder.setItems(it, this) }

        args.getString("negativeButton")?.let { builder.setNegativeButton(it, this) }
        args.getString("neutralButton")?.let { builder.setNeutralButton(it, this) }
        args.getString("positiveButton")?.let { builder.setPositiveButton(it, this) }

        instance?.apply {
            if (flags and DialogInstance.FLAG_INPUT_DIALOG != 0) {
                edit = EditText(activity)
                args.getString("editTextDefault")?.let(edit!!::setText)
                builder.setView(edit)
            }
            onCreate?.invoke(this@DialogFragmentUtil, builder)
        }

        return builder.create()
    }

    fun onFinished(button: Int){
        instance?.apply {
            when (button) {
                DialogInterface.BUTTON_NEGATIVE -> onNegativeButton?.invoke(this@DialogFragmentUtil)
                DialogInterface.BUTTON_NEUTRAL -> onNeutralButton?.invoke(this@DialogFragmentUtil)
                DialogInterface.BUTTON_POSITIVE -> onPositiveButton?.invoke(this@DialogFragmentUtil)
                MessageHandlerCanceled -> onCancel?.invoke(this@DialogFragmentUtil)
            }
            if (button >= 0 && onItem != null) onItem?.invoke(this@DialogFragmentUtil, button)
            onDismiss?.invoke(this@DialogFragmentUtil)
            dialogInstances.remove(this@DialogFragmentUtil.arguments?.getInt("instanceId", -1))
        }
    }

    override fun onClick(dialogInterface: DialogInterface, i: Int) {
        onFinished(i)
    }

    override fun onCancel(dialog: DialogInterface?) {
        onFinished(MessageHandlerCanceled)
    }
}


internal fun showPreparedDialog(args: Bundle) {
    (VideLibriApp.currentActivity as? AppCompatActivity?)
            ?.also { showPreparedDialog(it, args) }
            ?: VideLibriApp.pendingDialogs.add(args)
}

fun showPreparedDialog(activity: AppCompatActivity, args: Bundle) {
    val frag = DialogFragmentUtil()
    frag.arguments = args
    frag.show(activity.supportFragmentManager, null)
}


fun Bundle.putSparseBooleanArray(key: String, value: SparseBooleanArray) {
    putIntArray(key, IntArray(value.size() * 2) { i ->
        when (i and 1) {
            0 -> value.keyAt(i / 2)
            1 -> if (value.valueAt(i / 2)) 1 else 0
            else -> 0
        }
    })
}
fun Bundle.getSparseBooleanArray(key: String): SparseBooleanArray? {
    val a = getIntArray(key) ?: return null
    if (a.size and 1 == 1) return null
    val res = SparseBooleanArray()
    for (i in 0 until a.size step 2)
        res.put(a[i], a[i + 1] == 1)
    return res
}


inline fun <reified T: View> Activity.forEachView(vararg ids: Int, f: (T) -> Unit ) {
    ids.forEach { f.invoke(findViewById(it)) }
}

inline val Context.notificationManager: NotificationManager?
    get() = getSystemService(Context.NOTIFICATION_SERVICE) as NotificationManager?

inline val Context.preferences: SharedPreferences
    get() = PreferenceManager.getDefaultSharedPreferences(this)


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

fun InputStream.readAllText(): String = bufferedReader().use { it.readText() }
inline fun InputStream.useLines(f: (String) -> Unit) {
    bufferedReader().use {r ->
        var line = r.readLine()
        while (line != null) {
            f(line)
            line = r.readLine()
        }
    }
}

fun AssetManager.exists(fileName: String): Boolean {
    try {
        val stream = open(fileName)
        if (stream != null) {
            stream.close()
            return true
        }
    } catch (ignored: IOException) {

    }
    return false
}

