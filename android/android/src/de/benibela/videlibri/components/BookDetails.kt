package de.benibela.videlibri.components

import android.app.Activity
import android.graphics.Canvas
import android.graphics.Paint
import android.graphics.Paint.FontMetricsInt
import android.graphics.Typeface
import android.text.SpannableStringBuilder
import android.text.Spanned
import android.text.style.LeadingMarginSpan
import android.text.style.ReplacementSpan
import android.text.style.StyleSpan
import android.util.DisplayMetrics
import android.view.LayoutInflater
import android.view.View
import android.view.ViewGroup
import android.widget.Button
import androidx.recyclerview.widget.RecyclerView
import androidx.viewbinding.ViewBinding
import de.benibela.multilevellistview.ClickableRecyclerView
import de.benibela.videlibri.CoverLoader.loadBookCover
import de.benibela.videlibri.R
import de.benibela.videlibri.activities.BookListActivity
import de.benibela.videlibri.activities.RenewList
import de.benibela.videlibri.activities.SearchResult
import de.benibela.videlibri.databinding.BookDetailsCoverBinding
import de.benibela.videlibri.databinding.BookDetailsHoldingBinding
import de.benibela.videlibri.databinding.BookDetailsPropertyBinding
import de.benibela.videlibri.jni.Bridge
import de.benibela.videlibri.jni.Bridge.Book.StatusEnum
import de.benibela.videlibri.utils.*
import de.benibela.videlibri.utils.BookFormatter.formatDate
import de.benibela.videlibri.utils.BookFormatter.formatDateFull
import java.util.*
import kotlin.math.max
import kotlin.math.min

open class ViewHolderWithBinding<B: ViewBinding>(val binding: B): RecyclerView.ViewHolder(binding.root){
    constructor(parent: ViewGroup, inflate: (inflater: LayoutInflater, root: ViewGroup?, attachToRoot: Boolean) -> B): this(
            inflate(LayoutInflater.from(parent.context), parent, false)
    )
}

class BookDetails internal constructor(activity: BookListActivity) : VideLibriFakeFragment(activity) {
    val listview: ClickableRecyclerView = activity.findViewById(R.id.bookDetailsRecyclerView)
    val adapter get() = listview.adapter as? BookDetailsAdapter

    open class Details(name: String, data: CharSequence?) {
        val name: String = if (isAdditionalDisplayProperty(name)) name.substring(0, name.length - 1) else name
        @JvmField var data: CharSequence = data ?: ""
        override fun toString(): String {
            return "$name: $data"
        }
    }

    internal class DetailsHolding(name: String, data: CharSequence?, holding: Bridge.Book, val holdingId: Int, val orderLabel: String) : Details(name, data) {
        val orderable: Boolean = holding.isOrderableHolding
    }

    class BookDetailsAdapter(private val context: Activity, val details: ArrayList<Details>, private val book: Bridge.Book) : ClickableRecyclerView.Adapter<ViewHolderWithBinding<*>>() {
        override fun getItemCount(): Int = 1 + details.size
        override fun getItemViewType(position: Int) =
                when {
                    position == 0 -> R.layout.book_details_cover
                    position >= holdingStartPosition -> R.layout.book_details_holding
                    else -> R.layout.book_details_property
                }
        override fun onCreateViewHolder(parent: ViewGroup, viewType: Int): ViewHolderWithBinding<*> =
            when (viewType) {
                R.layout.book_details_holding -> ViewHolderWithBinding(parent, BookDetailsHoldingBinding::inflate)
                R.layout.book_details_cover -> ViewHolderWithBinding(parent, BookDetailsCoverBinding::inflate)
                else -> ViewHolderWithBinding(parent, BookDetailsPropertyBinding::inflate)
            }.also {
                registerClickHandler(it)
            }

        override fun onBindViewHolder(holder: ViewHolderWithBinding<*>, position: Int){
            when (val binding = holder.binding) {
                is BookDetailsCoverBinding -> {
                    binding.imageview.setImageBitmap(book.image)
                }
                is BookDetailsPropertyBinding -> {
                    val d = details[position - 1]
                    binding.label.text = d.name
                    binding.data.text = d.data
                    var c = defaultColor
                    if (trStatus == d.name || trDueDate == d.name) {
                        c = book.getStatusColor()
                        if (c == -1) c = defaultColor
                    }
                    binding.data.setTextColor(c)
                }
                is BookDetailsHoldingBinding -> {
                    val d = details[position - 1]
                    binding.label.text = d.name
                    binding.simpletextview.text = d.data
                    binding.simpletextview.setTextColor(defaultColor)
                    if (d is DetailsHolding
                            && d.orderable
                            && book.account == null
                            && context is SearchResult) {
                        binding.button.text = d.orderLabel
                        binding.button.visibility = View.VISIBLE
                        binding.button.isClickable = holdingOrderClickable
                        binding.button.setOnClickListener { context.orderBookHolding(book, d.holdingId) }
                    } else  binding.button.visibility = View.GONE
                }
            }
        }

        private val holdingStartPosition: Int
        private val defaultColor: Int

        var holdingOrderClickable = true

        fun updateImage() = notifyItemChanged(0)

        init {
            val resources = context.resources
            @Suppress("DEPRECATION")
            defaultColor = resources.getColor(android.R.color.primary_text_dark)
            holdingStartPosition = details.indexOfFirst { it is DetailsHolding }.let { if (it == -1) details.size else it } + 1
        }
    }

    val details = ArrayList<Details>()
    private fun addDetails(label: Int, data: String?){
        data ?: return
        details.add(Details(getString(label), data))
    }
    private fun addDetails(label: String, data: String?){
        data ?: return
        details.add(Details(label, data))
    }

    var book: Bridge.Book = Bridge.Book()
    set(newBook) {
        field = newBook

        trStatus = getString(R.string.book_status)
        trDueDate = getString(R.string.book_duedate)

        /*Log.i("VL",  ""+isInLayout());
        Log.i("VL", ""+getSherlockActivity());
        Log.i("VL", ""+getView());       */

        val isSearchedBook = book.account == null
        details.clear()

        addDetails(R.string.book_titleauthor, arrayOf(book.title, " ", book.author, book.year, book.id).filter { it.isNotEmpty() }.joinToString("\n").takeNonEmpty())

        if (!isSearchedBook && !book.history || book.dueDate != 0)
            addDetails(if (book.hasOrderedStatus()) getString(R.string.book_duedate_order) else trDueDate, formatDateFull(book.dueDate))

        addDetails(trStatus, book.getStatusText())
        addDetails(R.string.book_lenddate, when {
            book.issueDate != 0 -> formatDateFull(book.issueDate)
            book.firstExistsDate != 0 -> activity.getString(R.string.book_lenddate_before_prefixS, formatDateFull(book.firstExistsDate))
            else -> null
        })
        secondaryProperties.forEach { (label, property) -> addDetails(label, book[property].takeNonEmpty()) }
        addDetails(R.string.book_renewCount, book["renewCount"].takeIf { it != "" && it != "0" })
        addDetails(R.string.book_account, book.account?.prettyName)
        book.forEachAdditionalProperty { key, value ->
            if (value.isNotEmpty()) {
                if (!isSearchedBook && !ignorableProperties.contains(key)
                        || isSearchedBook && isAdditionalDisplayProperty(key))
                    details.add(Details(key, value))
                else if ("isbn" == key) details.add(Details("ISBN", value))
            }
        }
        addHoldings(book.holdings)

        listview.adapter = BookDetailsAdapter(activity, details, book)

        val needToLoadImage = book.image == null && (book.hasProperty("image-url") || book.hasProperty("isbn"))
        if (needToLoadImage) {
            loadBookCover(this, book)
        }

        val action: String? = when {
            isSearchedBook && book.isOrderable ->
                book["orderTitle"].takeNonEmpty() ?: getString(R.string.book_order)
            !isSearchedBook && !book.history && activity !is RenewList -> when (book.status) {
                StatusEnum.Unknown, StatusEnum.Normal -> getString(R.string.book_renew)
                StatusEnum.Ordered, StatusEnum.Provided -> getString(R.string.book_cancel).takeIf { book.isCancelable }
                else -> null
            }
            else -> null
        }

        activity.findViewById<Button>(R.id.button)?.let { actionButton ->
            if (action != null) {
                actionButton.text = action
                actionButton.visibility = View.VISIBLE
                actionButton.setOnClickListener { (activity as? BookListActivity)?.onBookActionButtonClicked(book) }
                (activity as? BookListActivity)?.bookActionButton = actionButton
            } else actionButton.visibility = View.GONE
        }
    }

    private inner class HoldingDetailMaker {
        private var builder = SpannableStringBuilder()
        var padding = 0
        var holding: Bridge.Book? = null
        fun addPair(name: String?, value: String?) {
            if (value.isNullOrEmpty()) return
            val len = builder.length
            builder.append(" ")
            builder.setSpan(object : ReplacementSpan() {
                override fun getSize(paint: Paint, text: CharSequence?, start: Int, end: Int, fm: FontMetricsInt?): Int {
                    return -padding //undo the indentation of the leading margin span as leading margin span seems to be broken crap: https://issuetracker.google.com/issues/36956124
                }

                override fun draw(canvas: Canvas, text: CharSequence?, start: Int, end: Int, x: Float, top: Int, y: Int, bottom: Int, paint: Paint) {}
            }, len, len + 1, Spanned.SPAN_INCLUSIVE_EXCLUSIVE)
            builder.append(name).append(": ")
            builder.setSpan(StyleSpan(Typeface.BOLD), len, builder.length, Spanned.SPAN_INCLUSIVE_EXCLUSIVE)
            builder.append(value).append("\n")
            builder.setSpan(LeadingMarginSpan.Standard(padding, padding), len, builder.length, Spanned.SPAN_INCLUSIVE_EXCLUSIVE)
        }


        fun build(): CharSequence =
            builder
        fun clear() {
            builder = SpannableStringBuilder()
        }
    }

    private fun addHoldings(holdings: Array<Bridge.Book>?) {
        if (holdings == null || holdings.isEmpty()) return
        val builder = HoldingDetailMaker()
        builder.padding = (activity.resources.displayMetrics.scaledDensity * 40 + 0.5).toInt()
        val defaultOrderTitle = book["orderTitle"].takeNonEmpty() ?: getString(R.string.book_order)
        for (i in holdings.indices) {
            val holding = holdings[i]
            builder.clear()
            builder.holding = holdings[i]
            builder.addPair(getString(R.string.book_title), holding.title)
            builder.addPair(getString(R.string.book_author), holding.author)
            holdingsProperties.forEach { (label, key) ->
                holding[key].takeNonEmpty()?.let { builder.addPair(getString(label), it) }
            }
            holding.forEachAdditionalProperty {key, value ->
                if (isAdditionalDisplayProperty(key)) builder.addPair(key.substring(0, key.length - 1), value)
            }
            if (holding.dueDate != 0) builder.addPair(trDueDate, formatDate(holding.dueDate))
            val orderTitle = holding["orderTitle"].takeNonEmpty() ?: defaultOrderTitle
            details.add(DetailsHolding(getString(R.string.book_holding_nrD, i + 1), builder.build(), holding, i, orderTitle))
        }
    }

    fun updateImage() = adapter?.updateImage()

    fun setOrderButtonsClickable() {
        val clickable = (activity as? SearchResult)?.searcher?.loadingTaskOrderHolding == false
        adapter?.holdingOrderClickable = clickable
        listview.forEachDescendantView { v -> (v as? Button)?.isClickable = clickable }
    }

    fun exportShare(html: Boolean): String =
        if (html)
            details.joinToString("<br>\n") {
                "<b>${it.name.escapeForHtml().ensureSuffix(":")}</b>\n${it.data.escapeForHtml()}"
            }
        else details.joinToString("\n\n") {
            "${it.name.ensureSuffix(":")}\n${it.data}"
        }

    companion object {
        var trStatus = ""
        var trDueDate = ""
        fun isAdditionalDisplayProperty(s: String) = s.endsWith("!")

        var displayMetrics: DisplayMetrics? = null

        val secondaryProperties = listOf(
                R.string.book_lendat to "libraryBranch",
                R.string.book_libraryLocation to  "libraryLocation",
                R.string.book_category to  "category",
                R.string.book_publisher to  "publisher"
        )
        val ignorableProperties = setOf(
                "status", "orderable", "cancelable", "renewCount", "isbn"
        ) + secondaryProperties.map { it.second }

        val holdingsProperties = listOf(
                R.string.book_id to "id",
                R.string.book_barcode to "barcode",
                R.string.book_category to "category",
                R.string.book_publisher to "publisher",
                R.string.book_libraryBranch to "libraryBranch",
                R.string.book_libraryLocation to "libraryLocation",
                R.string.book_year to "year",
                R.string.book_status to "status",
                R.string.book_pendingOrders to "pendingOrders"
        )

    }


    var coverTargetWidth: Int
    var coverTargetHeight: Int
    init {
        displayMetrics = activity.resources.displayMetrics.also { dm ->
            val longSide = max(dm.widthPixels, dm.heightPixels)
            val shortSide = min(dm.widthPixels, dm.heightPixels)
            //portrait: maxWidth = shortSide, maxHeight = longSide * factor
            //landscape: maxWidth = longSide / 2, maxHeight = shortSide * factor
            coverTargetWidth = max(3, min(shortSide, longSide / 2))
            coverTargetHeight = max(3, shortSide / 2)
        }
    }
}

