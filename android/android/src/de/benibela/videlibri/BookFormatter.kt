package de.benibela.videlibri


import android.content.Context
import android.graphics.Color
import android.support.annotation.StringRes

import de.benibela.videlibri.jni.Bridge


internal object BookFormatter {
    internal object tr {
        internal object bookStatus {
            lateinit var provided: String
            lateinit var ordered: String
            lateinit var problematic: String
        }
        operator fun invoke(@StringRes id: Int): String = Util.tr(id)
        @JvmStatic fun init(context: Context? = null){
            //if (bookStatus::provided.isInitialized) return
            val c = context ?: VideLibriApp.currentContext() ?: return
            bookStatus.provided = c.getString(R.string.book_status_provided)
            bookStatus.ordered = c.getString(R.string.book_status_ordered)
            bookStatus.problematic = c.getString(R.string.book_status_problematic)
        }
    }

    @JvmStatic fun getStatusColor(book: Bridge.Book): Int =
        if (book.history)
            -1
        else if ((book.account != null || book.isGroupingHeader)
                && book.dueDate != 0
                && book.dueDate - Bridge.currentPascalDate <= Bridge.globalOptions.nearTime)
            Color.RED
        else
            when (book.status) {
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


    @JvmStatic fun isGroupingHeaderFakeBook(book: Bridge.Book): Boolean {
        return book.isGroupingHeader
    }


    @JvmStatic fun shortened(s: String): String =
            if (s.length < 300) s
            else s.substring(0, 300) + "..."

    @JvmStatic fun getBookMoreText(book: Bridge.Book): String =
        listOf(shortened(book.author.trim { it <= ' ' }),
                book.year,
                book.id).filter { it.isNotBlank() }.joinToString(" ; ") 

    @JvmStatic fun getBookDateText(book: Bridge.Book, options: BookListDisplayOptions): String =
        if (book.account != null && !book.history) { //lend book
            when (book.status) {
                Bridge.Book.StatusEnum.Provided -> tr.bookStatus.provided
                Bridge.Book.StatusEnum.Ordered -> tr.bookStatus.ordered
                else -> {
                    val fd = formatDate(book.dueDate)
                    if (options.showRenewCount) {
                        val renewCount = book.getProperty("renewCount")
                        if ("" != renewCount && "0" != renewCount) "${renewCount}V ${fd}"
                        else fd
                    } else fd
                }
            }
        } else when (book.status) {
                Bridge.Book.StatusEnum.Available ->  "\u2713"
                Bridge.Book.StatusEnum.Lend -> "\u2717"
                Bridge.Book.StatusEnum.Virtual ->  "?"
                Bridge.Book.StatusEnum.Presentation -> "\u2717"
                Bridge.Book.StatusEnum.InterLoan ->  "\u2717"
                else -> ""
            }

    @JvmStatic fun getStatusText(book: Bridge.Book): String =
        book.getProperty("status").takeNonEmpty() ?:
            when (book.status) {
                Bridge.Book.StatusEnum.Problematic -> tr.bookStatus.problematic
                Bridge.Book.StatusEnum.Ordered -> tr.bookStatus.ordered
                Bridge.Book.StatusEnum.Provided -> tr.bookStatus.provided
                else -> ""
            }

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
        return if (full) Util.formatDateFull(date) else Util.formatDate(date)
    }

    @JvmStatic fun formatDate(pascalDate: Int): String = formatDate(pascalDate, false)
    @JvmStatic fun formatDateFull(pascalDate: Int): String = formatDate(pascalDate, true)
}
