package de.benibela.videlibri.jni

import de.benibela.videlibri.jni.SearchEvent.*

//API to Pascal library search.
//All search methods run asynchronously in a Pascal Thread and set the fields here
//All events are called in the same thread
abstract class SearcherAccessPascal {
    var nativePtr: Long = 0

    @Volatile var totalResultCount = 0
    @Volatile var nextPageAvailable = false
    @Volatile var searchParams:  FormParams? = null

    abstract fun onConnected(params: FormParams)
    abstract fun onSearchFirstPageComplete(books: Array<Bridge.Book>)
    abstract fun onSearchNextPageComplete(books: Array<Bridge.Book>)
    abstract fun onSearchDetailsComplete(book: Bridge.Book)
    abstract fun onOrderComplete(book: Bridge.Book)
    abstract fun onTakePendingMessage(kind: Int, caption: String, options: Array<String>)
    abstract fun onPendingMessageCompleted()
    abstract fun onException()
}

//Kotlin wrapper around Pascal search
//passes the events to the main thread
class SearcherAccess (
    val libId: String
): SearcherAccessPascal() {

    val pendingEvents = ArrayList<SearchEvent>()

    //set in (Kotlin) activity
    var state = 0
    var heartBeat: Long = 0
    var nextPageSearchPending = false

    val bookCache = ArrayList<Bridge.Book>()

    var loadingTaskList = false
    var loadingTaskDetails = false
    var loadingTaskOrder = false
    var loadingTaskOrderHolding = false
    var loadingTaskMessage = false

    //The detail search runs in the background, for a single book.
    //But the user might request other detail searches, before the search is complete.
    //Then wait for the old search to complete, and then start the newest search, unless the user has closed the view
    var waitingForDetails =0 //nr of book currently searched. Only set when the search is started or has ended (-1 if no search is running)
    var nextDetailsRequested = 0 //nr of the book that *should* be searched. Set when requesting a new search, or to -1 to cancel the current search
    var orderingAccount: Bridge.Account? = null
    fun connect() {
        Bridge.VLSearchConnect(this, libId)
    }

    fun start(query: Bridge.Book) {
        Bridge.VLSearchStart(this, query)
    }

    fun nextPage() {
        Bridge.VLSearchNextPage(this)
    }

    fun details(book: Bridge.Book) {
        Bridge.VLSearchDetails(this, book)
    }

    fun order(book: Bridge.Book) {
        Bridge.VLSearchOrder(this, arrayOf(book))
    }

    fun order(book: Bridge.Book, holdingId: Int) {
        Bridge.VLSearchOrder(this, arrayOf(book), intArrayOf(holdingId))
    }

    fun completePendingMessage(result: Int) {
        Bridge.VLSearchCompletePendingMessage(this, result)
    }

    fun free() {
        Bridge.VLSearchEnd(this)
    }

    private fun send(event: SearchEvent) {
        if (Bridge.searchEventHandler == null) return
        event.searcherAccess = this
        Bridge.searchEventHandler!!.sendMessage(Bridge.searchEventHandler!!.obtainMessage(0, event))
    }

    override fun onConnected(params: FormParams) {
        send(Connected(params))
    }

    override fun onSearchFirstPageComplete(books: Array<Bridge.Book>) {
        send(FirstPage(books))
    }
    override fun onSearchNextPageComplete(books: Array<Bridge.Book>) {
        send(NextPage(books))
    }
    override fun onSearchDetailsComplete(book: Bridge.Book) {
        send(Details(book))
    }
    override fun onOrderComplete(book: Bridge.Book) {
        send(OrderComplete(book))
    }
    override fun onTakePendingMessage(kind: Int, caption: String, options: Array<String>) {
        send(TakePendingMessage(kind, caption, options))
    }
    override fun onPendingMessageCompleted() {
        send(PendingMessageComplete())
    }
    override fun onException() {
        send(Exception())
    }
}
