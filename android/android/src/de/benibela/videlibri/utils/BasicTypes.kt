package de.benibela.videlibri.utils

import java.io.InputStream
import java.text.DateFormat
import java.util.*


fun <T: CharSequence> T.takeNonEmpty(): T? = takeIf(CharSequence::isNotEmpty)
fun CharSequence.ensureSuffix(suffix: CharSequence) =
        if (endsWith(suffix)) this
        else "$this$suffix"
fun CharSequence.escapeForHtml(): CharSequence =
        if (!this.contains("<") && !this.contains("&")) this
        else this.toString().replace(Regex("&"), "&amp;").replace(Regex("<"), "&lt;")


fun CharSequence.countOf(c: Char) = this.count { it == c }


private val dateFormatShort: DateFormat? by lazy {
    currentContext?.let { android.text.format.DateFormat.getDateFormat(it) } ?: DateFormat.getDateInstance(DateFormat.SHORT)
}
private val dateFormatFull: DateFormat? by lazy {
    DateFormat.getDateInstance(DateFormat.FULL) ?: currentContext?.let { android.text.format.DateFormat.getLongDateFormat(it) }
}
@Suppress("DEPRECATION")
fun Date.formatShort() = dateFormatShort?.format(this) ?: "${year+1900}-$month-$day"
fun Date.formatFull() = dateFormatFull?.format(this) ?: this.formatShort()


/*
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
    for (i in a.indices step 2)
        res.put(a[i], a[i + 1] == 1)
    return res
}
*/



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
