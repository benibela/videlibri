package de.benibela.videlibri.utils

import android.app.Activity
import android.app.NotificationManager
import android.content.ClipData
import android.content.ClipboardManager
import android.content.Context
import android.content.res.AssetManager
import android.content.res.Resources.NotFoundException
import android.os.Handler
import android.os.Looper
import android.widget.Toast
import androidx.annotation.StringRes
import de.benibela.videlibri.R
import de.benibela.videlibri.VideLibriApp
import java.io.IOException
import java.util.*


@Suppress("NOTHING_TO_INLINE")
inline val currentContext get() = VideLibriApp.currentContext()
inline fun <reified T> currentActivity(): T? = (VideLibriApp.currentActivity as? T)
inline fun <reified T: Activity> withActivity(f: T.() -> Unit) = currentActivity<T>()?.run(f)

fun runOnUiThread(runnable: () -> Unit) = (VideLibriApp.uiHandler ?: Handler(Looper.getMainLooper())).post(runnable)

fun showToast(message: CharSequence, length: Int = Toast.LENGTH_SHORT) =
        Toast.makeText(currentContext, message, length).show()
fun showToast(@StringRes message: Int, length: Int = Toast.LENGTH_SHORT) =
        Toast.makeText(currentContext, message, length).show()

//fun getString(@StringRes message: Int): String = Util.tr(message) ?: "??"
fun getString(@StringRes message: Int,  vararg args: Any?): String = currentContext?.let { context -> try {
        try {
            context.getString(message, *args)
        } catch (e: IllegalFormatException) {
            context.getString(message)
        }
    } catch (e: NotFoundException) {
        "missing translation: $message"
    }
} ?: "?tr?$message"



inline val Context.notificationManager: NotificationManager?
    get() = getSystemService(Context.NOTIFICATION_SERVICE) as NotificationManager?


fun AssetManager.exists(fileName: String): Boolean {
    try {
        val stream = open(fileName)
        stream.close()
        return true
    } catch (ignored: IOException) {

    }
    return false
}


internal object Clipboard {
    private val manager = currentContext?.getSystemService(Context.CLIPBOARD_SERVICE) as? ClipboardManager
    var text: CharSequence?
        get() = manager?.primaryClip?.getItemAt(0)?.coerceToText(currentContext)
        set(toCopy) {
            val clip = ClipData.newPlainText("Book details", toCopy) ?: return
            manager?.setPrimaryClip(clip)
            showToast(currentContext?.getString(R.string.clipboard_copiedS, toCopy).toString())
        }
}
