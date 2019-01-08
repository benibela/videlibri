package de.benibela.videlibri

import android.os.Bundle
import android.util.Log
import android.view.Menu
import android.view.MenuInflater
import de.benibela.videlibri.jni.Bridge

class SearchResult : BookListActivity(), SearchEventHandler {

    internal var searcher: Bridge.SearcherAccess? = null
    internal var libId = ""

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        val book = intent.getSerializableExtra("searchQuery") as? Bridge.Book
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

    override fun onCreateOptionsMenuOverflow(menu: Menu, inflater: MenuInflater) {
        inflater.inflate(R.menu.searchresultmenu, menu)
        super.onCreateOptionsMenuOverflow(menu, inflater)
    }
    override fun setOptionMenuVisibility(menu: Menu?) {
        super.setOptionMenuVisibility(menu)
        menu?.forItems {
            it.isVisible = when (it.itemId) {
                R.id.search -> false
                R.id.copy_to_wishlist -> detailsVisible()
                else -> return@forItems
            }
        }
    }

    override fun onOptionsItemIdSelected(id: Int): Boolean {
        when (id) {
            R.id.copy_to_wishlist ->
                currentBook()?.let { book ->
                    var matchingAccounts = accounts.filter { it.libId == libId  }
                    if (matchingAccounts.isEmpty())
                        matchingAccounts = accounts.toArray.toList()
                    if (matchingAccounts.isEmpty())
                        showMessage(tr(R.string.search_needaccount_for_wishlist))
                    else
                        showChooseAccountDialog(getString(R.string.search_getaccount_for_wishlist), matchingAccounts) { account ->
                            book.account = account
                            book.history = true
                            Bridge.VLChangeBook(null, book)
                            LendingList.refreshDisplayedLendBooks()
                            book.account = null
			    showToast(R.string.search_copied_to_wishlist)
                        }
		    return true
                }
        }
        return super.onOptionsItemIdSelected(id)
    }


    override fun onSearchEvent(event: Bridge.SearchEvent): Boolean {
        val access = event.searcherAccess
        if (access !== searcher || access == null) return false
        access.heartBeat = System.currentTimeMillis()
        when (event.kind) {
            Bridge.SearchEventKind.CONNECTED -> {
                access.state = Search.SEARCHER_STATE_CONNECTED
                access.homeBranches = event.obj1 as? Array<String>
                access.searchBranches = event.obj2 as? Array<String>
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


    fun onOrderComplete(book: Bridge.Book) {
        showMessage(getString(R.string.search_orderedokS, book.title, book.getProperty("status")))
        LendingList.refreshDisplayedLendBooks()
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

    override fun onBookActionButtonClicked(book: Bridge.Book) {
        if (book.isOrderable)
            orderBookHolding(book, -1)
    }

    fun orderBookHolding(book: Bridge.Book, choosenHolding: Int) {
        val matchingAccounts = accounts.filter { it.libId == libId && it.isReal }
        if (matchingAccounts.isEmpty()) {
            showMessage(tr(R.string.search_needaccount))
            return
        }
        showChooseAccountDialog(getString(R.string.search_orderTargetAccount, book.title), matchingAccounts) {account ->
            book.account = account
            currentlyOrderedBook = book
            withActivity<SearchResult> {
                val searcher = searcher ?: return@showChooseAccountDialog
                searcher.orderingAccount = book.account //this property is lost on roundtrip, saved it on java side
                if (choosenHolding < 0) {
                    bookActionButton?.isClickable = false
                    beginLoading(VideLibriBaseActivityOld.LOADING_SEARCH_ORDER)
                    searcher.order(book)
                } else {
                    beginLoading(VideLibriBaseActivityOld.LOADING_SEARCH_ORDER_HOLDING)
                    details.setOrderButtonsClickable()
                    searcher.order(book, choosenHolding)
                }
            }
        }
    }

    fun onTakePendingMessage(kind: Int, caption: String, options: Array<String>) {
        when (kind) {
            1 -> showDialog {
                message(caption)
                yesButton { currentActivity<SearchResult>()?.searcher?.completePendingMessage(1) }
                noButton { currentActivity<SearchResult>()?.searcher?.completePendingMessage(0) }
            }
            2 -> showDialog {
                items(options.toList()) { currentActivity<SearchResult>()?.searcher?.completePendingMessage(it) }
                onCancel = { currentActivity<SearchResult>()?.searcher?.completePendingMessage(-1) }
            }
        }
    }

    private fun onOrderFailed() {
        //if the book has an account, but is not ordered, it will be shown as lend, which is completely wrong

        //unfortunately we do not know which book was supposed to be ordered
        val book = currentlyOrderedBook ?: details.book ?: return
        if (!book.hasOrderedStatus())
            book.account = null
    }

    companion object {
        var currentlyOrderedBook: Bridge.Book? = null
    }
}

fun showChooseAccountDialog(
        title: String,
        accounts: List<Bridge.Account>,
        onResult: (DialogFragmentUtil.(account: Bridge.Account) -> Unit)
) = showChooseDialog(title, accounts.map { it.prettyName }) { i -> onResult(accounts[i]) }
