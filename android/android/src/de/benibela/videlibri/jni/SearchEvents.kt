package de.benibela.videlibri.jni

import de.benibela.videlibri.jni.Bridge.SearcherAccess

sealed class SearchEvent {
    var searcherAccess: SearcherAccess? = null
    class Connected(val params: FormParams): SearchEvent()
    class FirstPage(val books: Array<Bridge.Book>): SearchEvent()
    class NextPage(val books: Array<Bridge.Book>): SearchEvent()
    class Details(val book: Bridge.Book): SearchEvent()
    class OrderComplete(val book: Bridge.Book): SearchEvent()
    class TakePendingMessage(val kind: Int, val caption: String, val options: Array<String>): SearchEvent()
    class PendingMessageComplete(): SearchEvent()
    class Exception(): SearchEvent()
}
