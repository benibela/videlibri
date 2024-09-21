package de.benibela.videlibri.utils


import android.content.Context
import android.graphics.Color
import androidx.annotation.StringRes
import de.benibela.videlibri.*
import de.benibela.videlibri.jni.BookListDisplayOptions
import de.benibela.videlibri.jni.BookStatus

import de.benibela.videlibri.jni.Bridge
import de.benibela.videlibri.jni.globalOptionsShared


internal object BookFormatter {
    @Suppress("ClassName")
    internal object tr {
        internal object bookStatus {
            lateinit var provided: String
            lateinit var ordered: String
            lateinit var reserved: String
            lateinit var problematic: String
        }
        operator fun invoke(@StringRes id: Int): String = getString(id)
        @JvmStatic fun init(context: Context? = null){
            //if (bookStatus::provided.isInitialized) return
            val c = context ?: VideLibriApp.currentContext()
            ?: return
            bookStatus.provided = c.getString(R.string.book_status_provided)
            bookStatus.ordered = c.getString(R.string.book_status_ordered)
            bookStatus.reserved = c.getString(R.string.book_status_reserved)
            bookStatus.problematic = c.getString(R.string.book_status_problematic)
        }
    }


    @JvmStatic fun shortened(s: String): String =
            if (s.length < 300) s
            else s.substring(0, 300) + "..."

    @JvmStatic fun formatDate(pascalDate: Int): String = PascalDate(pascalDate).formatShortRelative()
    @JvmStatic fun formatDateFull(pascalDate: Int): String = PascalDate(pascalDate).formatFullRelative()

}

fun Bridge.Book.getMoreText(): String =
        listOf(BookFormatter.shortened(author.trim { it <= ' ' }),
                year,
                id).filter { it.isNotBlank() }.joinToString(" ; ")

fun Bridge.Book.getDateText(options: BookListDisplayOptions): String =
        if (account != null && !history) { //lend book
            when (status) {
                BookStatus.Provided -> BookFormatter.tr.bookStatus.provided
                BookStatus.Ordered, BookStatus.Reserved -> BookFormatter.tr.bookStatus.ordered
                else -> {
                    val fd = BookFormatter.formatDate(dueDate)
                    if (options.showRenewCount) {
                        val renewCount = this["renewCount"]
                        if ("" != renewCount && "0" != renewCount) "${renewCount}V $fd"
                        else fd
                    } else fd
                }
            }
        } else when (status) {
            BookStatus.Available ->  "\u2713"
            BookStatus.Lend -> "\u2717"
            BookStatus.Virtual ->  "?"
            BookStatus.Presentation -> "\u2717"
            BookStatus.InterLoan ->  "\u2717"
            else -> ""
        }

fun Bridge.Book.getStatusColor(): Int =
        if (history)
            -1
        else if ((account != null || isGroupingHeader)
                && dueDate != 0
                && dueDate - PascalDate.todayInt <= globalOptionsShared.nearTime)
            Color.RED
        else
            when (status) {
                //lend
                BookStatus.Normal -> Color.GREEN
                BookStatus.Problematic -> Color.YELLOW
                BookStatus.Ordered, BookStatus.Reserved -> Color.CYAN
                BookStatus.Provided -> Color.MAGENTA
                //search
                BookStatus.Available -> Color.GREEN
                BookStatus.Lend -> Color.RED
                BookStatus.Virtual -> Color.CYAN
                BookStatus.Presentation -> Color.RED
                BookStatus.InterLoan -> Color.RED


                else -> Color.YELLOW //Template did not set status. Assume not renewable
            }

fun Bridge.Book.getStatusText(): String =
        getProperty("status").takeNonEmpty() ?:
        when (status) {
            BookStatus.Problematic -> BookFormatter.tr.bookStatus.problematic
            BookStatus.Ordered -> BookFormatter.tr.bookStatus.ordered
            BookStatus.Provided -> BookFormatter.tr.bookStatus.provided
            else -> ""
        }

operator fun Bridge.Book.get(key: String) = this.getProperty(key)

inline fun Bridge.Book.forEachAdditionalProperty(f: (key: String, value: String) -> Unit) {
    var i = 0
    while (i < additionalProperties.size) {
        val key = additionalProperties[i]
        val value = additionalProperties[i + 1]
        f(key, value)
        i += 2
    }
}

