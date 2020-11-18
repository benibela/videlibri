package de.benibela.videlibri.utils

import android.app.Activity
import android.app.NotificationManager
import android.content.ClipData
import android.content.ClipboardManager
import android.content.Context
import android.content.SharedPreferences
import android.content.res.AssetManager
import android.os.Handler
import android.os.Looper
import android.preference.PreferenceManager
import android.widget.Toast
import androidx.annotation.StringRes
import de.benibela.videlibri.R
import de.benibela.videlibri.VideLibriApp
import java.io.IOException


@Suppress("NOTHING_TO_INLINE")
inline val currentContext get() = VideLibriApp.currentContext()
inline fun <reified T> currentActivity(): T? = (VideLibriApp.currentActivity as? T)
inline fun <reified T: Activity> withActivity(f: T.() -> Unit) = currentActivity<T>()?.run(f)

fun runOnUiThread(runnable: () -> Unit) = (VideLibriApp.uiHandler ?: Handler(Looper.getMainLooper())).post(runnable)

fun showToast(message: CharSequence) =
        Toast.makeText(currentContext, message, Toast.LENGTH_SHORT).show()
fun showToast(@StringRes message: Int) =
        Toast.makeText(currentContext, message, Toast.LENGTH_SHORT).show()

fun getString(@StringRes message: Int): String = Util.tr(message) ?: "??"



inline val Context.notificationManager: NotificationManager?
    get() = getSystemService(Context.NOTIFICATION_SERVICE) as NotificationManager?

inline val Context.preferences: SharedPreferences
    get() = PreferenceManager.getDefaultSharedPreferences(this)



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
    val manager = currentContext?.getSystemService(Context.CLIPBOARD_SERVICE) as? ClipboardManager
    var text: CharSequence?
        get() = manager?.primaryClip?.getItemAt(0)?.coerceToText(currentContext)
        set(toCopy) {
            val clip = ClipData.newPlainText("Book details", toCopy) ?: return
            manager?.setPrimaryClip(clip)
            showToast(Util.tr(R.string.clipboard_copiedS, toCopy))
        }
}
