package de.benibela.videlibri.activities

import android.os.Bundle
import android.util.Log
import android.view.Menu
import android.view.MenuInflater
import de.benibela.videlibri.*
import de.benibela.videlibri.jni.Bridge
import de.benibela.videlibri.jni.SearchEvent
import de.benibela.videlibri.jni.SearcherAccess
import de.benibela.videlibri.utils.*
import kotlin.math.max

class SearchResult : BookListActivity(), SearchEventHandler {

    var searcher: SearcherAccess? = null
        private set
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
                    it.waitingForDetails = -1
                    it.nextDetailsRequested = -1
                    it.start(book)
                }
                Search.SEARCHER_STATE_CONNECTED -> {
                    it.waitingForDetails = -1
                    it.nextDetailsRequested = -1
                    it.start(book)
                }
                else -> bookCache = it.bookCache
            }
        }
    }

    private fun setTitle() {
        when (searcher?.state) {
            Search.SEARCHER_STATE_SEARCHING -> title = getString(R.string.search_resultcountD, max(bookCache.size, searcher!!.totalResultCount))
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
                    displayBookCache(max(bookCache.size, searcher.totalResultCount))
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
                        matchingAccounts = Accounts.toArray.toList()
                    if (matchingAccounts.isEmpty())
                        showMessage(getString(R.string.search_needaccount_for_wishlist))
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


    override fun onSearchEvent(event: SearchEvent): Boolean {
        val access = event.searcherAccess
        if (access !== searcher || access == null) return false
        access.heartBeat = System.currentTimeMillis()
        when (event) {
            is SearchEvent.Connected -> {
                access.state = Search.SEARCHER_STATE_CONNECTED
                access.searchParams = event.params
                access.waitingForDetails = -1
                access.nextDetailsRequested = -1
                access.start(intent.getSerializableExtra("searchQuery") as Bridge.Book)
            }
            is SearchEvent.FirstPage
            -> {
                access.loadingTaskList = false
                access.state = Search.SEARCHER_STATE_SEARCHING
                onSearchFirstPageComplete(event.books)
            }
            is SearchEvent.NextPage
            -> {
                access.loadingTaskList = false
                onSearchNextPageComplete(event.books)
            }
            is SearchEvent.Details
            -> onSearchDetailsComplete(event.book)
            is SearchEvent.OrderComplete
            -> {
                access.loadingTaskOrder = false
                access.loadingTaskOrderHolding = false
                access.loadingTaskMessage = false
                onOrderComplete(event.book)
            }
            is SearchEvent.TakePendingMessage
            -> {
                access.loadingTaskMessage = false
                onTakePendingMessage(event.kind, event.caption, event.options)
            }
            is SearchEvent.PendingMessageComplete -> {
                onOrderFailed()
                access.loadingTaskOrder = false
                access.loadingTaskOrderHolding = false
                access.loadingTaskMessage = false
                details.setOrderButtonsClickable()
            }
            is SearchEvent.Exception -> {
                onOrderFailed()
                access.state = Search.SEARCHER_STATE_FAILED
                setTitle()
                //searcher.state = Search.SEARCHER_STATE_FAILED;
                //searcher = null;
                //Search.gcSearchers();
                VideLibriApp.showPendingExceptions()
            }
        }
        refreshLoadingIcon()
        return true
    }

    private fun onSearchFirstPageComplete(books: Array<Bridge.Book>) {
        searcher?.let { searcher ->
            searcher.bookCache.clear()
            searcher.bookCache.addAll(books)
            bookCache = searcher.bookCache
            val realCount = max(searcher.totalResultCount, bookCache.size)
            displayBookCache(realCount)
        }
        setTitle()
    }

    private fun onSearchNextPageComplete(books: Array<Bridge.Book>) {
        searcher?.let { searcher ->
            searcher.nextPageSearchPending = false
            searcher.bookCache.addAll(books)
            bookCache = searcher.bookCache
        }
        list.adapter?.books = bookCache
        setTitle()
    }


    private fun onSearchDetailsComplete(book: Bridge.Book) {
        searcher?.let { searcher ->
            val oldWaitingForDetails = searcher.waitingForDetails
            searcher.waitingForDetails = -1 //search has ended

            book.setProperty("__details", "")
            if (oldWaitingForDetails in searcher.bookCache.indices)
                searcher.bookCache[oldWaitingForDetails] = book //still save the search result, so it does not need to be searched again

            searcher.loadingTaskDetails = false
            refreshLoadingIcon()

            if (detailsVisible()) {
                val nextDetailsRequested = searcher.nextDetailsRequested
                if (nextDetailsRequested !in bookCache.indices)
                    return
                if (nextDetailsRequested != oldWaitingForDetails) {
                    searcher.waitingForDetails = nextDetailsRequested
                    searcher.loadingTaskDetails = true
                    searcher.details(bookCache[nextDetailsRequested])
                    refreshLoadingIcon()
                    return
                }

                details.book = book
            }
        }

    }


    private fun onOrderComplete(book: Bridge.Book) {
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
            if (searcher.nextPageSearchPending) return
            searcher.nextPageAvailable = false
            searcher.nextPageSearchPending = true
            searcher.loadingTaskList = true
            refreshLoadingIcon()
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
                searcher.loadingTaskDetails = true
                refreshLoadingIcon()
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
            showMessage(getString(R.string.search_needaccount))
            return
        }
        showChooseAccountDialog(getString(R.string.search_orderTargetAccount, book.title), matchingAccounts) { account ->
            book.account = account
            currentlyOrderedBook = book
            withActivity<SearchResult> {
                val searcher = searcher ?: return@showChooseAccountDialog
                searcher.orderingAccount = book.account //this property is lost on roundtrip, saved it on java side
                if (choosenHolding < 0) {
                    bookActionButton?.isClickable = false
                    searcher.loadingTaskOrder = true
                    searcher.order(book)
                } else {
                    searcher.loadingTaskOrderHolding = true
                    details.setOrderButtonsClickable()
                    searcher.order(book, choosenHolding)
                }
                refreshLoadingIcon()
            }
        }
    }

    private fun onTakePendingMessage(kind: Int, caption: String, options: Array<String>) {
        when (kind) {
            1 -> showDialog {
                message(caption)
                yesButton { currentActivity<SearchResult>()?.searcher?.completePendingMessage(1) }
                noButton { currentActivity<SearchResult>()?.searcher?.completePendingMessage(0) }
            }
            2 -> showDialog(null, caption) {
                items(options.toList()) { currentActivity<SearchResult>()?.searcher?.completePendingMessage(it) }
                onCancel = { currentActivity<SearchResult>()?.searcher?.completePendingMessage(-1) }
            }
        }
    }

    private fun onOrderFailed() {
        //if the book has an account, but is not ordered, it will be shown as lend, which is completely wrong

        //unfortunately we do not know which book was supposed to be ordered
        val book = currentlyOrderedBook
                ?: details.book
        if (!book.hasOrderedStatus())
            book.account = null
    }


    override val isLoading: Boolean
        get() = super.isLoading || ( searcher?.run {
            when (state){
                Search.SEARCHER_STATE_INIT -> true
                Search.SEARCHER_STATE_CONNECTED -> true
                Search.SEARCHER_STATE_FAILED -> false
                else -> loadingTaskList || loadingTaskDetails || loadingTaskOrder || loadingTaskOrderHolding || loadingTaskMessage
            }
        } ?: false )

    override fun listLoadingTasksStrings(tasks: MutableList<Int>) {
        super.listLoadingTasksStrings(tasks)
        searcher?.run {
            when (state) {
                Search.SEARCHER_STATE_INIT -> tasks += R.string.loading_search_connecting
                Search.SEARCHER_STATE_CONNECTED -> tasks += R.string.loading_search_searching
                Search.SEARCHER_STATE_FAILED -> return
                else -> {
                    if (loadingTaskList) tasks += R.string.loading_search_searching
                    if (loadingTaskDetails) tasks += R.string.loading_search_details
                    if (loadingTaskOrder) tasks += R.string.loading_search_order
                    if (loadingTaskOrderHolding) tasks += R.string.loading_search_order_holding
                    if (loadingTaskMessage) tasks += R.string.loading_search_message
                }
            }
        }
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
