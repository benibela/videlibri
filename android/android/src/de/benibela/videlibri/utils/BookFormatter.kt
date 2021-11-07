package de.benibela.videlibri.utils


import android.content.Context
import android.graphics.Color
import androidx.annotation.StringRes
import de.benibela.videlibri.*
import de.benibela.videlibri.activities.BookListDisplayOptions

import de.benibela.videlibri.jni.Bridge


internal object BookFormatter {
    @Suppress("ClassName")
    internal object tr {
        internal object bookStatus {
            lateinit var provided: String
            lateinit var ordered: String
            lateinit var problematic: String
        }
        operator fun invoke(@StringRes id: Int): String = getString(id)
        @JvmStatic fun init(context: Context? = null){
            //if (bookStatus::provided.isInitialized) return
            val c = context ?: VideLibriApp.currentContext()
            ?: return
            bookStatus.provided = c.getString(R.string.book_status_provided)
            bookStatus.ordered = c.getString(R.string.book_status_ordered)
            bookStatus.problematic = c.getString(R.string.book_status_problematic)
        }
    }


    @JvmStatic fun shortened(s: String): String =
            if (s.length < 300) s
            else s.substring(0, 300) + "..."


    @JvmStatic
    private fun formatDate(pascalDate: Int, full: Boolean): String {
        if (pascalDate == 0) return tr(R.string.unknown_date)
        if (Bridge.currentPascalDate > 0 && VideLibriApp.currentContext() != null) {
            when (pascalDate - Bridge.currentPascalDate) {
                -2 -> return tr(R.string.daybeforeyesterday)
                -1 -> return tr(R.string.yesterday)
                0 -> return tr(R.string.today)
                1 -> return tr(R.string.tomorrow)
                2 -> return tr(R.string.dayaftertomorrow)
            }
        }
        val date = Bridge.pascalDateToDate(pascalDate)
        return if (full) date.formatFull() else date.formatShort()
    }

    @JvmStatic fun formatDate(pascalDate: Int): String = formatDate(pascalDate, false)
    @JvmStatic fun formatDateFull(pascalDate: Int): String = formatDate(pascalDate, true)
}

fun Bridge.Book.getMoreText(): String =
        listOf(BookFormatter.shortened(author.trim { it <= ' ' }),
                year,
                id).filter { it.isNotBlank() }.joinToString(" ; ")

fun Bridge.Book.getDateText(options: BookListDisplayOptions): String =
        if (account != null && !history) { //lend book
            when (status) {
                Bridge.Book.StatusEnum.Provided -> BookFormatter.tr.bookStatus.provided
                Bridge.Book.StatusEnum.Ordered -> BookFormatter.tr.bookStatus.ordered
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
            Bridge.Book.StatusEnum.Available ->  "\u2713"
            Bridge.Book.StatusEnum.Lend -> "\u2717"
            Bridge.Book.StatusEnum.Virtual ->  "?"
            Bridge.Book.StatusEnum.Presentation -> "\u2717"
            Bridge.Book.StatusEnum.InterLoan ->  "\u2717"
            else -> ""
        }

fun Bridge.Book.getStatusColor(): Int =
        if (history)
            -1
        else if ((account != null || isGroupingHeader)
                && dueDate != 0
                && dueDate - Bridge.currentPascalDate <= Bridge.globalOptions.nearTime)
            Color.RED
        else
            when (status) {
                //lend
                Bridge.Book.StatusEnum.Normal -> Color.GREEN
                Bridge.Book.StatusEnum.Problematic -> Color.YELLOW
                Bridge.Book.StatusEnum.Ordered -> Color.CYAN
                Bridge.Book.StatusEnum.Provided -> Color.MAGENTA
                //search
                Bridge.Book.StatusEnum.Available -> Color.GREEN
                Bridge.Book.StatusEnum.Lend -> Color.RED
                Bridge.Book.StatusEnum.Virtual -> Color.CYAN
                Bridge.Book.StatusEnum.Presentation -> Color.RED
                Bridge.Book.StatusEnum.InterLoan -> Color.RED


                else -> Color.YELLOW //Template did not set status. Assume not renewable
            }

fun Bridge.Book.getStatusText(): String =
        getProperty("status").takeNonEmpty() ?:
        when (status) {
            Bridge.Book.StatusEnum.Problematic -> BookFormatter.tr.bookStatus.problematic
            Bridge.Book.StatusEnum.Ordered -> BookFormatter.tr.bookStatus.ordered
            Bridge.Book.StatusEnum.Provided -> BookFormatter.tr.bookStatus.provided
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

