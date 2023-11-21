package de.benibela.videlibri.utils

import de.benibela.videlibri.*
import de.benibela.videlibri.jni.BookStatus
import de.benibela.videlibri.jni.BookStatusInt
import de.benibela.videlibri.jni.Bridge
import java.util.*

@JvmInline value class PascalDate(private val daysSinceEpoch: Int) {
    val date: Date get() = (beginOfEpoch.clone() as Calendar).also{
        it.add(Calendar.DATE, daysSinceEpoch)
    }.time

    operator fun minus (shift: Int) = PascalDate(daysSinceEpoch - shift)
    operator fun plus (shift: Int) = PascalDate(daysSinceEpoch + shift)


    private fun formatRelative(full: Boolean): String {
        if (daysSinceEpoch == 0) return BookFormatter.tr(R.string.unknown_date)
        if (todayInt > 0 && VideLibriApp.currentContext() != null) {
            when (daysSinceEpoch - todayInt) {
                -2 -> return BookFormatter.tr(R.string.daybeforeyesterday)
                -1 -> return BookFormatter.tr(R.string.yesterday)
                0 -> return BookFormatter.tr(R.string.today)
                1 -> return BookFormatter.tr(R.string.tomorrow)
                2 -> return BookFormatter.tr(R.string.dayaftertomorrow)
            }
        }
        return if (full) date.formatFull() else date.formatShort()
    }

    fun formatShortRelative(): String = formatRelative(false)
    fun formatFullRelative(): String = formatRelative(true)


    val week: Int get() =  (daysSinceEpoch - 2) / 7

    fun formatWeekRelative(): String {
        if (daysSinceEpoch == 0) return getString(R.string.unknown_date)

        val week = week
        if (todayInt > 0)
            when (week - today.week) {
                -1 -> return getString(R.string.last_week)
                0 -> return getString(R.string.this_week)
                1 -> return getString(R.string.next_week)
            }
        val delta = daysSinceEpoch - 2 - week * 7
        return String.format(getString(R.string.week_from_to),
            (this - delta).date.formatShort(),
            (this - delta + 6).date.formatShort())
    }


    companion object {
        val beginOfEpoch = GregorianCalendar(1899, 11, 30) //1899-12-30
        val today inline get() = PascalDate(todayInt)
        val todayInt inline get() = Bridge.currentPascalDate
    }
}


private fun Bridge.Book?.getExtendedProperty(key: String): String = this?.run{
    when (key) {
        "_dueWeek" -> PascalDate(dueDate).formatWeekRelative()
        "_account" -> account?.prettyName
        "dueDate" -> BookFormatter.formatDate(dueDate)
        "_status" -> when (status) {
            BookStatus.Unknown, BookStatus.Normal -> getString(R.string.book_status_normal)
            BookStatus.Problematic -> getString(R.string.book_status_problematic)
            BookStatus.Ordered -> getString(R.string.book_status_ordered)
            BookStatus.Provided -> getString(R.string.book_status_provided)
            BookStatus.Reserved -> getString(R.string.book_status_reserved)
            else -> getString(R.string.book_status_unknown)
        }
        "_issueWeek", "issueDate" -> {
            val date = if (issueDate != 0) issueDate else firstExistsDate
            when {
                date == 0 -> getString(R.string.unknown_date)
                "issueDate" == key -> PascalDate(date).formatShortRelative()
                else -> PascalDate(date).formatWeekRelative()
            }
        }
        "" -> return ""
        else -> return getProperty(key)
    }
} ?: ""

private fun compare(a: Int, b: Int): Int {
    return if (a < b) -1 else if (b < a) 1 else 0
}

private fun compare(a: Boolean, b: Boolean): Int {
    if (a == b) return 0
    return if (a) 1 else -1
}

private fun compareNullFirst(a: Any?, b: Any?): Int {
    return compare(a != null, b != null)
}

fun compareForStateMismatch(book: Bridge.Book, book2: Bridge.Book): Int =
    if (book.history != book2.history) {
        if (book.history)
            1
        else
            -1
    } else if (book.hasOrderedStatus() != book2.hasOrderedStatus()) {
        if (book.hasOrderedStatus())
            1
        else
            -1
    } else compare(book.dueDate != 0, book2.dueDate != 0)

//order: ordered normal provided problematic
private fun statusToOrderedStatus(s: BookStatusInt) = when (s) {
    BookStatus.Unknown -> 2
    BookStatus.Problematic -> 99 //highest
    BookStatus.Normal -> 3
    BookStatus.Ordered -> 0
    BookStatus.Provided -> 77
    BookStatus.Reserved -> 1
    else -> 4
}
private fun compareStatus(s1: BookStatusInt, s2: BookStatusInt): Int =
    if (s1 == s2) 0
    else {
        val o1 = statusToOrderedStatus(s1)
        val o2 = statusToOrderedStatus(s2)
        if (o1 < o2) 1
        else if (o1 > o2) -1
        else 0
    }



private fun compareForKey(book: Bridge.Book?, book2: Bridge.Book?, key: String): Int =
    if (book == null || book2 == null) 0
    else when (key) {
        "_dueWeek" -> {
            val temp = compareForStateMismatch(book, book2)
            if (temp != 0 || book.dueDate == 0) temp
            else compare(PascalDate(book.dueDate).week, PascalDate(book2.dueDate).week)
        }
        "_account" -> {
            val a = book.account
            val a2 = book2.account
            var temp = compareNullFirst(a, a2)
            if (temp != 0 || a == null || a2 == null ) 0
            else {
                temp = compareNullFirst(a.prettyName, a2.prettyName)
                if (temp != 0) 0
                else a.prettyName.compareTo(a2.prettyName)
            }
        }
        "dueDate" -> {
            val temp = compareForStateMismatch(book, book2)
            if (temp != 0 || book.dueDate == 0) temp else compare(book.dueDate, book2.dueDate)
        }
        "_status" -> {
            val temp = compareForStateMismatch(book, book2)
            if (temp != 0) temp else compareStatus(book.status, book2.status)
        }
        "_issueWeek", "issueDate" -> {
            var temp = compareForStateMismatch(book, book2)
            if (temp != 0) temp
            else {
                var d1 = if (book.issueDate != 0) book.issueDate else book.firstExistsDate
                var d2 = if (book2.issueDate != 0) book2.issueDate else book2.firstExistsDate
                temp = compare(d1 != 0, d2 != 0)
                if (temp != 0 || d1 == 0) temp
                else {
                    if ("_issueWeek" == key) {
                        d1 = PascalDate(d1).week
                        d2 = PascalDate(d2).week
                    }
                    compare(d1, d2)
                }
            }
        }
        "" -> 0
        else -> book.getProperty(key).compareTo(book2.getProperty(key))
    }


fun makePrimaryBookCache(addHistoryStart: Boolean,
                         renewableOnly: Boolean): ArrayList<Bridge.Book> {
    val addHistory = addHistoryStart && !renewableOnly //history is not renewable
    val bookCache = ArrayList<Bridge.Book>()
    accounts.let { accounts ->
        for (acc in accounts) {
            if (acc.isHidden)
                continue
            val books: Array<Bridge.Book> = Bridge.VLGetBooks(acc, false) ?: continue
            if (!renewableOnly) {
                bookCache.addAll(books)
            } else
                for (b in books)
                    if (b.status == BookStatus.Unknown || b.status == BookStatus.Normal)
                        bookCache.add(b)
            if (addHistory)
                bookCache.addAll(Bridge.VLGetBooks(acc, true) ?: continue)
        }
    }

    return bookCache
}

fun Bridge.Book.matchesFilter(filter: String, key: String): Boolean =
    if (key.isNotEmpty()) getProperty(key).contains(filter, true)
    else author.contains(filter, true) || title.contains(filter, true)


fun filterToSecondaryBookCache(oldBookCache: ArrayList<Bridge.Book>,
                               groupingKey: String, sortingKey: String,
                               filter: String?, filterKey: String): ArrayList<Bridge.Book> {
    val bookCache = ArrayList<Bridge.Book>()

    if (filter?.isNotEmpty() == true && "__disabled" != filterKey) {
        for (book in oldBookCache)
            if (book.matchesFilter(filter, filterKey))
                bookCache.add(book)
    } else
        bookCache.addAll(oldBookCache)

    bookCache.sortWith { book, book2 ->
        val temp = compareForKey(book, book2, groupingKey)
        if (temp != 0) temp else compareForKey(book, book2, sortingKey)
    }

    if ("" != groupingKey) {
        var lastGroup = ""
        var groupHeader: Bridge.Book? = null
        var i = 0
        while (i < bookCache.size) {
            val b = bookCache[i]
            val newGroup = b.getExtendedProperty(groupingKey)
            if (newGroup != lastGroup) {
                groupHeader = object : Bridge.Book() {
                    override fun isGroupingHeader(): Boolean {
                        return true
                    }
                }
                groupHeader.title = newGroup
                groupHeader.history = true
                groupHeader.status = BookStatus.Ordered
                bookCache.add(i, groupHeader)
                lastGroup = newGroup
                i++
            }
            if (groupHeader != null) {
                if (!b.history) {
                    groupHeader.history = false
                    if (compareStatus(groupHeader.status, b.status) > 0)
                        groupHeader.status = b.status
                    if (b.dueDate != 0 && (groupHeader.dueDate == 0 || groupHeader.dueDate > b.dueDate))
                        groupHeader.dueDate = b.dueDate
                }
            }
            i++
        }
    }

    return bookCache
}