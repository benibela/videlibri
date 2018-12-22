package de.benibela.videlibri

import de.benibela.videlibri.jni.Bridge
import java.util.*

private fun dateToWeek(pascalDate: Int): Int {
    return (pascalDate - 2) / 7
}

private fun getWeekString(pascalDate: Int): String {
    if (pascalDate == 0) return Util.tr(R.string.unknown_date)

    val week = dateToWeek(pascalDate)
    if (Bridge.currentPascalDate > 0)
        when (week - dateToWeek(Bridge.currentPascalDate)) {
            -1 -> return Util.tr(R.string.last_week)
            0 -> return Util.tr(R.string.this_week)
            1 -> return Util.tr(R.string.next_week)
        }
    val delta = pascalDate - 2 - week * 7
    return String.format(Util.tr(R.string.week_from_to),
            Util.formatDate(Bridge.pascalDateToDate(pascalDate - delta)),
            Util.formatDate(Bridge.pascalDateToDate(pascalDate - delta + 6)))
}

private fun Bridge.Book?.getExtendedProperty(key: String): String = this?.run{
    when (key) {
        "_dueWeek" -> getWeekString(dueDate)
        "_account" -> account?.prettyName
        "dueDate" -> BookFormatter.formatDate(dueDate)
        "_status" -> when (status) {
            Bridge.Book.StatusEnum.Unknown, Bridge.Book.StatusEnum.Normal -> Util.tr(R.string.book_status_normal)
            Bridge.Book.StatusEnum.Problematic -> Util.tr(R.string.book_status_problematic)
            Bridge.Book.StatusEnum.Ordered -> Util.tr(R.string.book_status_ordered)
            Bridge.Book.StatusEnum.Provided -> Util.tr(R.string.book_status_provided)
            else -> Util.tr(R.string.book_status_unknown)
        }
        "_issueWeek", "issueDate" -> {
            val date = if (issueDate != 0) issueDate else firstExistsDate
            when {
                date == 0 -> Util.tr(R.string.unknown_date)
                "issueDate" == key -> BookFormatter.formatDate(date)
                else -> getWeekString(date)
            }
        }
        "" -> return ""
        else -> return getProperty(key)
    }
} ?: ""


fun compareForStateMismatch(book: Bridge.Book, book2: Bridge.Book): Int =
    if (book.history != book2.history) {
        if (book.history)
            1
        else
            -1
    } else if ((book.status == Bridge.Book.StatusEnum.Ordered || book.status == Bridge.Book.StatusEnum.Provided) !=
              (book2.status == Bridge.Book.StatusEnum.Ordered || book2.status == Bridge.Book.StatusEnum.Provided)) {
        if (book.status == Bridge.Book.StatusEnum.Ordered || book.status == Bridge.Book.StatusEnum.Provided)
            1
        else
            -1
    } else Util.compare(book.dueDate != 0, book2.dueDate != 0)


//order: ordered normal provided problematic
private fun compareStatus(s1: Bridge.Book.StatusEnum, s2: Bridge.Book.StatusEnum): Int =
    if (s1 == s2) 0
    else when (s1) {
        Bridge.Book.StatusEnum.Unknown, Bridge.Book.StatusEnum.Normal -> when (s2) {
            Bridge.Book.StatusEnum.Problematic, Bridge.Book.StatusEnum.Provided -> 1
            else -> -1
        }
        Bridge.Book.StatusEnum.Problematic -> -1 //problematic is the highest
        Bridge.Book.StatusEnum.Ordered -> 1 //ordered the lowest
        Bridge.Book.StatusEnum.Provided -> {
            if (s2 == Bridge.Book.StatusEnum.Problematic) 1 else -1
        }
        else -> 0
    }


private fun compareForKey(book: Bridge.Book?, book2: Bridge.Book?, key: String): Int =
    if (book == null || book2 == null) 0
    else when (key) {
        "_dueWeek" -> {
            val temp = compareForStateMismatch(book, book2)
            if (temp != 0 || book.dueDate == 0) temp
            else Util.compare(dateToWeek(book.dueDate), dateToWeek(book2.dueDate))
        }
        "_account" -> {
            val a = book.account
            val a2 = book2.account
            var temp = Util.compareNullFirst(a, a2)
            if (temp != 0 || a == null || a2 == null ) 0
            else {
                temp = Util.compareNullFirst(a.prettyName, a2.prettyName)
                if (temp != 0) 0
                else a.prettyName.compareTo(a2.prettyName)
            }
        }
        "dueDate" -> {
            val temp = compareForStateMismatch(book, book2)
            if (temp != 0 || book.dueDate == 0) temp else Util.compare(book.dueDate, book2.dueDate)
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
                temp = Util.compare(d1 != 0, d2 != 0)
                if (temp != 0 || d1 == 0) temp
                else {
                    if ("_issueWeek" == key) {
                        d1 = dateToWeek(d1)
                        d2 = dateToWeek(d2)
                    }
                    Util.compare(d1, d2)
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
        for (facc in accounts) {
            if (facc.isHidden)
                continue
            val books: Array<Bridge.Book> = Bridge.VLGetBooks(facc, false) ?: continue
            if (!renewableOnly) {
                bookCache.addAll(books)
            } else
                for (b in books)
                    if (b.status == Bridge.Book.StatusEnum.Unknown || b.status == Bridge.Book.StatusEnum.Normal)
                        bookCache.add(b)
            if (addHistory)
                bookCache.addAll(Bridge.VLGetBooks(facc, true) ?: continue)
        }
    }

    return bookCache
}

fun Bridge.Book.matchesFilter(filter: String, key: String): Boolean =
    if (key.isNotEmpty()) getProperty(key).toLowerCase().contains(filter)
    else author.toLowerCase().contains(filter) || title.toLowerCase().contains(filter)


fun filterToSecondaryBookCache(oldBookCache: ArrayList<Bridge.Book>,
                               groupingKey: String, sortingKey: String,
                               filterStart: String?, filterKey: String): ArrayList<Bridge.Book> {
    var filter = filterStart
    val bookCache = ArrayList<Bridge.Book>()

    if (filter?.isNotEmpty() == true && "__disabled" != filterKey) {
        filter = filter.toLowerCase()
        for (book in oldBookCache)
            if (book.matchesFilter(filter, filterKey))
                bookCache.add(book)
    } else
        bookCache.addAll(oldBookCache)

    bookCache.sortWith(Comparator { book, book2 ->
        val temp = compareForKey(book, book2, groupingKey)
        if (temp != 0) temp else compareForKey(book, book2, sortingKey)
    })

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
                groupHeader.status = Bridge.Book.StatusEnum.Ordered
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