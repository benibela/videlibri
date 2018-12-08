package de.benibela.videlibri

import android.content.DialogInterface
import android.os.Bundle
import android.util.Log
import android.view.Menu

import java.util.ArrayList

import de.benibela.videlibri.jni.Bridge

class SearchResult : BookListActivity(), SearchEventHandler {

    internal var searcher: Bridge.SearcherAccess? = null
    internal var libId = ""

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        val book = intent.getSerializableExtra("searchQuery") as Bridge.Book?
        if (book == null) {
            Log.i("VideLibri", "search without book. Abort.")
            finish()
            return
        }

        searcher = Search.searchers.lastOrNull()
        setTitle()
        searcher?.let {
            libId = it.libId
            when (it.state) {
                Search.SEARCHER_STATE_INIT -> {
                    //searcher.connect(); //should not happen
                    beginLoading(VideLibriBaseActivityOld.LOADING_SEARCH_CONNECTING)
                    it.waitingForDetails = -1
                    it.nextDetailsRequested = -1
                    it.start(book, intent.getIntExtra("homeBranch", -1), intent.getIntExtra("searchBranch", -1))
                    beginLoading(VideLibriBaseActivityOld.LOADING_SEARCH_SEARCHING)
                }
                Search.SEARCHER_STATE_CONNECTED -> {
                    it.waitingForDetails = -1
                    it.nextDetailsRequested = -1
                    it.start(book, intent.getIntExtra("homeBranch", -1), intent.getIntExtra("searchBranch", -1))
                    beginLoading(VideLibriBaseActivityOld.LOADING_SEARCH_SEARCHING)
                }
                else -> bookCache = it.bookCache
            }
        }
    }

    private fun setTitle() {
        when (searcher?.state) {
            Search.SEARCHER_STATE_SEARCHING -> title = tr(R.string.search_resultcountD, Math.max(bookCache.size, searcher!!.totalResultCount))
            Search.SEARCHER_STATE_INIT, Search.SEARCHER_STATE_CONNECTED -> setTitle(R.string.search_loading)
            Search.SEARCHER_STATE_FAILED -> setTitle(R.string.search_failed)
            null -> setTitle(R.string.search_lost)
        }
    }

    override fun onResume() {
        super.onResume()
        searcher?.let {searcher ->
            if (searcher.nativePtr != 0L) {
                if (!cacheShown)
                    displayBookCache(Math.max(bookCache.size, searcher.totalResultCount))
                for (event in searcher.pendingEvents)
                    onSearchEvent(event)
            }
        }
        searcher?.pendingEvents?.clear()
    }

    override fun onPrepareOptionsMenu(menu: Menu?): Boolean {
        val x = super.onPrepareOptionsMenu(menu)
        menu?.findItem(R.id.search)?.setVisible(false)
        return x
    }

    override fun onSearchEvent(event: Bridge.SearchEvent): Boolean {
        val access = event.searcherAccess
        if (access !== searcher || access == null) return false
        access.heartBeat = System.currentTimeMillis()
        when (event.kind) {
            Bridge.SearchEventKind.CONNECTED -> {
                access.state = Search.SEARCHER_STATE_CONNECTED
                access.homeBranches = event.obj1 as Array<String>
                access.searchBranches = event.obj2 as Array<String>
                access.waitingForDetails = -1
                access.nextDetailsRequested = -1
                access.start(intent.getSerializableExtra("searchQuery") as Bridge.Book, intent.getIntExtra("homeBranch", -1), intent.getIntExtra("searchBranch", -1))
                beginLoading(VideLibriBaseActivityOld.LOADING_SEARCH_SEARCHING)
                endLoadingAll(VideLibriBaseActivityOld.LOADING_SEARCH_CONNECTING)
                return true
            }
            Bridge.SearchEventKind.FIRST_PAGE //obj1 = Book[] books
            -> {
                endLoadingAll(VideLibriBaseActivityOld.LOADING_SEARCH_SEARCHING)
                access.state = Search.SEARCHER_STATE_SEARCHING
                onSearchFirstPageComplete(event.obj1 as Array<Bridge.Book>)
            }
            Bridge.SearchEventKind.NEXT_PAGE  //obj1 = Book[] books
            -> {
                endLoadingAll(VideLibriBaseActivityOld.LOADING_SEARCH_SEARCHING)
                onSearchNextPageComplete(event.obj1 as Array<Bridge.Book>)
            }
            Bridge.SearchEventKind.DETAILS    //obj1 = Book book
            -> onSearchDetailsComplete(event.obj1 as Bridge.Book)
            Bridge.SearchEventKind.ORDER_COMPLETE //obj1 = Book book
            -> {
                endLoading(VideLibriBaseActivityOld.LOADING_SEARCH_ORDER)
                endLoading(VideLibriBaseActivityOld.LOADING_SEARCH_ORDER_HOLDING)
                endLoading(VideLibriBaseActivityOld.LOADING_SEARCH_MESSAGE)
                onOrderComplete(event.obj1 as Bridge.Book)
            }
            Bridge.SearchEventKind.ORDER_CONFIRM  //obj1 = Book book
            -> {
                endLoading(VideLibriBaseActivityOld.LOADING_SEARCH_ORDER)
                endLoading(VideLibriBaseActivityOld.LOADING_SEARCH_ORDER_HOLDING)
                onOrderConfirm(event.obj1 as Bridge.Book)
            }
            Bridge.SearchEventKind.TAKE_PENDING_MESSAGE //arg1 = int kind, obj1 = String caption, obj2 = String[] options
            -> {
                endLoading(VideLibriBaseActivityOld.LOADING_SEARCH_MESSAGE)
                onTakePendingMessage(event.arg1, event.obj1 as String, event.obj2 as Array<String>)
            }
            Bridge.SearchEventKind.PENDING_MESSAGE_COMPLETE -> {
                onOrderFailed()
                endLoading(VideLibriBaseActivityOld.LOADING_SEARCH_MESSAGE)
                endLoading(VideLibriBaseActivityOld.LOADING_SEARCH_ORDER)
                endLoading(VideLibriBaseActivityOld.LOADING_SEARCH_ORDER_HOLDING)
                details.setOrderButtonsClickable()
            }
            Bridge.SearchEventKind.EXCEPTION -> {
                onOrderFailed()
                endLoadingAll(intArrayOf(VideLibriBaseActivityOld.LOADING_SEARCH_CONNECTING, VideLibriBaseActivityOld.LOADING_SEARCH_SEARCHING, VideLibriBaseActivityOld.LOADING_SEARCH_DETAILS, VideLibriBaseActivityOld.LOADING_SEARCH_ORDER, VideLibriBaseActivityOld.LOADING_SEARCH_ORDER_HOLDING, VideLibriBaseActivityOld.LOADING_SEARCH_MESSAGE))
                setTitle()
                //searcher.state = Search.SEARCHER_STATE_FAILED;
                //searcher = null;
                //Search.gcSearchers();
                VideLibriApp.showPendingExceptions()
                return true
            }
        }
        return true
    }

    fun onSearchFirstPageComplete(books: Array<Bridge.Book>) {
        searcher?.let { searcher ->
            searcher.bookCache.clear()
            searcher.bookCache.addAll(books)
            bookCache = searcher.bookCache
            val realCount = Math.max(searcher.totalResultCount, bookCache.size)
            displayBookCache(realCount)
        }
        setTitle()
    }

    fun onSearchNextPageComplete(books: Array<Bridge.Book>) {
        searcher?.let { searcher ->
            searcher.bookCache.addAll(books)
            bookCache = searcher.bookCache
        }
        updateDisplayBookCache()
        setTitle()
    }


    fun onSearchDetailsComplete(book: Bridge.Book) {
        searcher?.let { searcher ->
            val oldWaitingForDetails = searcher.waitingForDetails
            searcher.waitingForDetails = -1 //search has ended

            book.setProperty("__details", "")
            if (oldWaitingForDetails in searcher.bookCache.indices)
                searcher.bookCache[oldWaitingForDetails] = book //still save the search result, so it does not need to be searched again

            endLoading(VideLibriBaseActivityOld.LOADING_SEARCH_DETAILS)

            if (detailsVisible()) {
                val nextDetailsRequested = searcher.nextDetailsRequested
                if (nextDetailsRequested !in bookCache.indices)
                    return
                if (nextDetailsRequested != oldWaitingForDetails) {
                    searcher.waitingForDetails = nextDetailsRequested
                    beginLoading(VideLibriBaseActivityOld.LOADING_SEARCH_DETAILS)
                    searcher.details(bookCache[nextDetailsRequested])
                    return
                }

                details.setBook(book)
            }
        }

    }

    fun onOrderConfirm(book: Bridge.Book) {
        //see bookSearchForm.pas
        searcher?.let { searcher ->
            val question = book.getProperty("orderConfirmation").replace("\\n", "\n")
            val orderConfirmationOptionTitles = book.getProperty("orderConfirmationOptionTitles")

            book.account = searcher.orderingAccount

            bookActionButton?.isClickable = true

            if ("" == question)
                searcher.orderConfirmed(book)
            else if (orderConfirmationOptionTitles == null || "" == orderConfirmationOptionTitles) {
                lastSelectedBookForDialog = book
                Util.showMessageYesNo(DialogId.SEARCHER_ORDER_CONFIRM, question)
            } else {
                val options = orderConfirmationOptionTitles.split("\\\\[|]".toRegex()).dropLastWhile { it.isEmpty() }.toTypedArray()
                lastSelectedBookForDialog = book
                Util.chooseDialog(DialogId.SEARCHER_CHOOSE_ORDER, question, options)
            }
        }
    }

    fun onTakePendingMessage(kind: Int, caption: String, options: Array<String>) {
        when (kind) {
            1 -> Util.showMessageYesNo(DialogId.SEARCHER_MESSAGE_CONFIRM, caption)
            2 -> Util.chooseDialog(DialogId.SEARCHER_MESSAGE_CHOOSE, caption, options)
        }
    }


    fun onOrderComplete(book: Bridge.Book) {
        Util.showMessage(tr(R.string.search_orderedokS, book.title, book.getProperty("status")))
        VideLibriApp.refreshDisplayedLendBooks()
        searcher?.let { searcher ->
            searcher.orderingAccount?.let {
                VideLibriApp.updateAccount(it, false, false) //full update, so the book is only shown if it worked, and canceling work
            }
            searcher.orderingAccount = null //???? todo
        }
        bookActionButton?.isClickable = true
    }


    override fun onPlaceHolderShown(position: Int) {
        searcher?.let { searcher ->
            searcher.nextPageAvailable = false
            beginLoading(VideLibriBaseActivityOld.LOADING_SEARCH_SEARCHING)
            searcher.nextPage()
        }
    }

    override fun viewDetails(bookpos: Int) {
        val oldbook = bookCache[bookpos]
        if (oldbook.account != null && !oldbook.hasOrderedStatus())
            oldbook.history = true //todo: tricky, cannot have a book with account and without order status, or it is shown as renewable.
        super.viewDetails(bookpos)

        searcher?.let { searcher ->
            if (oldbook.hasProperty("__details")) {
                searcher.nextDetailsRequested = -1
                return
            }
            searcher.nextDetailsRequested = bookpos
            if (searcher.waitingForDetails == -1) {
                searcher.waitingForDetails = bookpos
                beginLoading(VideLibriBaseActivityOld.LOADING_SEARCH_DETAILS)
                searcher.details(bookCache[searcher.waitingForDetails])
            }
        }
    }


    fun orderBook(book: Bridge.Book, choosenOrder: Int) {
        if (searcher == null) return
        book.setProperty("choosenOrder", "" + choosenOrder)
        val matchingAccounts = ArrayList<Bridge.Account>()
        for (acc in VideLibriApp.accounts)
            if (acc.libId == libId && acc.name != "")
                matchingAccounts.add(acc)
        if (matchingAccounts.size == 0) {
            Util.showMessage(tr(R.string.search_needaccount))
            return
        }
        //if (matchingAccounts.size() > 1) {
        val temp = arrayOfNulls<String>(matchingAccounts.size)
        for (i in matchingAccounts.indices)
            temp[i] = matchingAccounts[i].prettyName
        lastSelectedBookForDialog = book
        lastMatchingAccountsForDialog = matchingAccounts
        Util.chooseDialog(DialogId.SEARCHER_CHOOSE_TARGET_ACCOUNT, tr(R.string.search_orderTargetAccount, book.title), temp)
        /*} else {
            book.account = matchingAccounts.get(0);
            searcher.orderingAccount = book.account;
            if (bookActionButton != null) bookActionButton.setClickable(false);
            beginLoading(LOADING_SEARCH_ORDER);
            searcher.order(book);
        } */
    }

    private fun onOrderFailed() {
        //if the book has an account, but is not ordered, it will be shown as lend, which is completely wrong

        //unfortunately we do not know which book was supposed to be ordered
        val book = lastSelectedBookForDialog ?: details.book ?: return
        if (!book.hasOrderedStatus())
            book.account = null
    }

    fun orderBookHolding(book: Bridge.Book, choosenHolding: Int) {
        if (searcher == null) return
        val matchingAccounts = ArrayList<Bridge.Account>()
        for (acc in VideLibriApp.accounts)
            if (acc.libId == libId && acc.name != "")
                matchingAccounts.add(acc)
        if (matchingAccounts.size == 0) {
            Util.showMessage(tr(R.string.search_needaccount))
            return
        }
        //if (matchingAccounts.size() > 1) {
        val temp = arrayOfNulls<String>(matchingAccounts.size)
        for (i in matchingAccounts.indices)
            temp[i] = matchingAccounts[i].prettyName
        lastSelectedBookForDialog = book
        lastSelectedHolding = choosenHolding
        lastMatchingAccountsForDialog = matchingAccounts
        Util.chooseDialog(DialogId.SEARCHER_CHOOSE_TARGET_ACCOUNT_FOR_HOLDING, tr(R.string.search_orderTargetAccount, book.title), temp)
        /*} else {
            book.account = matchingAccounts.get(0);
            searcher.orderingAccount = book.account;
            beginLoading(LOADING_SEARCH_ORDER_HOLDING);
            details.setOrderButtonsClickable();
            searcher.order(book, choosenHolding);
        }  */
    }

    override fun onBookActionButtonClicked(book: Bridge.Book) {
        if (book.isOrderable) {
            var orders = 1
            try {
                orders = Integer.parseInt(book.getProperty("orderable"))
            } catch (ignored: NumberFormatException) {
            }

            if (orders == 1)
                orderBook(book, 0)
            else {
                val versions = arrayOfNulls<String>(orders)
                for (i in 0 until orders)
                    versions[i] = book.getProperty("orderTitle$i")
                lastSelectedBookForDialog = book
                Util.chooseDialog(DialogId.SEARCHER_CHOOSE_ORRERTITLE, tr(R.string.search_chooseitem), versions)
            }


        }
    }

    internal override fun onDialogResult(dialogId: Int, buttonId: Int, more: Bundle?): Boolean {
        when (dialogId) {
            DialogId.SEARCHER_MESSAGE_CONFIRM -> {
                beginLoading(VideLibriBaseActivityOld.LOADING_SEARCH_MESSAGE)
                searcher?.completePendingMessage(if (buttonId == DialogInterface.BUTTON_POSITIVE) 1 else 0)
                return true
            }
            DialogId.SEARCHER_MESSAGE_CHOOSE -> {
                beginLoading(VideLibriBaseActivityOld.LOADING_SEARCH_MESSAGE)
                searcher?.completePendingMessage(buttonId)
                if (lastSelectedBookForDialog != null) {
                    if (buttonId == DialogInterface.BUTTON_POSITIVE) {
                        bookActionButton?.isClickable = false
                        beginLoading(VideLibriBaseActivityOld.LOADING_SEARCH_ORDER)
                        searcher?.orderConfirmed(lastSelectedBookForDialog!!)
                    }
                    lastSelectedBookForDialog = null
                }
                return true
            }
            DialogId.SEARCHER_ORDER_CONFIRM -> {
                if (lastSelectedBookForDialog != null) {
                    if (buttonId == DialogInterface.BUTTON_POSITIVE) {
                        bookActionButton?.isClickable = false
                        beginLoading(VideLibriBaseActivityOld.LOADING_SEARCH_ORDER)
                        searcher?.orderConfirmed(lastSelectedBookForDialog!!)
                    }
                    lastSelectedBookForDialog = null
                }
                return true
            }
            DialogId.SEARCHER_CHOOSE_ORDER -> {
                if (lastSelectedBookForDialog != null) {
                    if (buttonId >= 0 /*&& buttonId < options.length*/) {
                        lastSelectedBookForDialog?.setProperty("choosenConfirmation", (buttonId + 1).toString() + "")
                        bookActionButton?.isClickable = false
                        beginLoading(VideLibriBaseActivityOld.LOADING_SEARCH_ORDER)
                        searcher?.orderConfirmed(lastSelectedBookForDialog!!)
                    }
                    lastSelectedBookForDialog = null
                }
                return true
            }
            DialogId.SEARCHER_CHOOSE_ORRERTITLE -> {
                if (buttonId >= 0 /*&& i < versions.length*/)
                    lastSelectedBookForDialog?.let{it -> orderBook(it, buttonId)}
                return true
            }
            DialogId.SEARCHER_CHOOSE_TARGET_ACCOUNT -> {
                lastSelectedBookForDialog?.let { lastSelectedBookForDialog->
                    lastMatchingAccountsForDialog?.let { lastMatchingAccountsForDialog ->
                        searcher?.let { searcher ->
                            if (buttonId in lastMatchingAccountsForDialog.indices) {
                                lastSelectedBookForDialog.account = lastMatchingAccountsForDialog[buttonId]
                                searcher.orderingAccount = lastSelectedBookForDialog.account //this property is lost on roundtrip, saved it on java side
                                bookActionButton?.isClickable = false
                                beginLoading(VideLibriBaseActivityOld.LOADING_SEARCH_ORDER)
                                searcher.order(lastSelectedBookForDialog)
                            }
                        }
                    }
                }
                lastSelectedBookForDialog = null
                lastMatchingAccountsForDialog = null
                return true
            }
            DialogId.SEARCHER_CHOOSE_TARGET_ACCOUNT_FOR_HOLDING -> {
                lastSelectedBookForDialog?.let { lastSelectedBookForDialog->
                    lastMatchingAccountsForDialog?.let { lastMatchingAccountsForDialog ->
                        searcher?.let { searcher ->
                            if (buttonId in lastMatchingAccountsForDialog.indices) {
                                lastSelectedBookForDialog.account = lastMatchingAccountsForDialog[buttonId]
                                searcher.orderingAccount = lastSelectedBookForDialog.account //this property is lost on roundtrip, saved it on java side
                                beginLoading(VideLibriBaseActivityOld.LOADING_SEARCH_ORDER_HOLDING)
                                details.setOrderButtonsClickable()
                                searcher.order(lastSelectedBookForDialog, lastSelectedHolding)
                            }
                        }
                    }
                }
                lastSelectedBookForDialog = null
                lastMatchingAccountsForDialog = null
                return true
            }
        }
        return super.onDialogResult(dialogId, buttonId, more)
    }

    companion object {
        private var lastSelectedBookForDialog: Bridge.Book? = null
        private var lastSelectedHolding = 0
        private var lastMatchingAccountsForDialog: ArrayList<Bridge.Account>? = null
    }
}
