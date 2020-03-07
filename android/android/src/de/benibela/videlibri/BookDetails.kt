package de.benibela.videlibri

import android.app.Activity
import android.graphics.Canvas
import android.graphics.Paint
import android.graphics.Paint.FontMetricsInt
import android.graphics.Typeface
import android.graphics.drawable.BitmapDrawable
import android.graphics.drawable.Drawable
import android.text.SpannableStringBuilder
import android.text.Spanned
import android.text.style.LeadingMarginSpan
import android.text.style.ReplacementSpan
import android.text.style.StyleSpan
import android.text.util.Linkify
import android.util.DisplayMetrics
import android.view.View
import android.view.ViewGroup
import android.widget.BaseAdapter
import android.widget.Button
import android.widget.ListView
import android.widget.TextView
import de.benibela.videlibri.BookFormatter.formatDate
import de.benibela.videlibri.BookFormatter.formatDateFull
import de.benibela.videlibri.CoverLoader.loadBookCover
import de.benibela.videlibri.jni.Bridge
import de.benibela.videlibri.jni.Bridge.Book.StatusEnum
import java.util.*
import kotlin.math.max
import kotlin.math.min

class BookDetails internal constructor(activity: BookListActivity) : VideLibriFakeFragment(activity) {
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

    var coverTargetWidth: Int
    var coverTargetHeight: Int

    class BookDetailsAdapter(private val context: Activity, private val details: ArrayList<Details>, private val book: Bridge.Book) : BaseAdapter() {
        private val holdingStartPosition: Int
        private val defaultColor: Int
        private val scale: Float
        private fun toPx(sp: Float): Int {
            return (sp * scale + 0.5f).toInt()
        }

        var holdingOrderClickable = true

        internal open class ViewHolder(var text: TextView)
        internal class ViewHolderHolding(text: TextView, var button: Button): ViewHolder(text)

        override fun getCount(): Int {
            return 2 * details.size + 1
        }

        override fun getItem(i: Int): Any {
            return details[(i - 1) / 2]
        }

        override fun getItemId(i: Int): Long {
            return i.toLong()
        }

        override fun getViewTypeCount(): Int {
            return 3
        }

        override fun getItemViewType(position: Int): Int {
            if (position == 0) return VIEW_VALUE
            if (position and 1 == 1) return VIEW_HEADER
            return if (position < holdingStartPosition) VIEW_VALUE else VIEW_HOLDING_VALUE
        }

        override fun getView(position: Int, convertView: View?, parent: ViewGroup?): View {
            val type = getItemViewType(position)
            var view = convertView
            if (view == null) {
                val viewHolder: ViewHolder
                val inflater = context.layoutInflater
                when (type) {
                    VIEW_HOLDING_VALUE -> {
                        view = inflater.inflate(R.layout.holdingrow, null)
                        viewHolder = ViewHolderHolding(view.findViewById(R.id.simpletextview), view.findViewById(R.id.holdingbutton))
                    }
                    //VIEW_HEADER, VIEW_VALUE,
                    else -> {
                        view = inflater.inflate(R.layout.simpletextview, null) //not setting a root makes the image centered. with root it is left aligned
                        viewHolder = ViewHolder(view.findViewById(R.id.simpletextview))
                    }
                }
                viewHolder.text.autoLinkMask = Linkify.WEB_URLS
                view.tag = viewHolder
                when (type) {
                    VIEW_HEADER -> {
                        viewHolder.text.typeface = Typeface.DEFAULT_BOLD
                        viewHolder.text.setPadding(toPx(10f), toPx(1f), toPx(10f), toPx(1f))
                    }
                    VIEW_VALUE -> {
                        viewHolder.text.typeface = Typeface.DEFAULT
                        viewHolder.text.setPadding(toPx(30f), toPx(1f), toPx(10f), toPx(2f))
                    }
                    VIEW_HOLDING_VALUE -> {
                        viewHolder.text.typeface = Typeface.DEFAULT
                        viewHolder.text.setPadding(toPx(30f), toPx(1f), toPx(10f), toPx(2f))
                    }
                }
            }

            val holder = view?.tag as ViewHolder
            if (position > 0) {
                val d = details[(position - 1) / 2]
                when (type) {
                    VIEW_HEADER -> holder.text.text = d.name
                    VIEW_VALUE -> {
                        holder.text.text = d.data
                        var c = defaultColor
                        if (trStatus == d.name || trDueDate == d.name) {
                            c = book.getStatusColor()
                            if (c == -1) c = defaultColor
                        }
                        holder.text.setTextColor(c)
                        holder.text.setCompoundDrawables(null, null, null, null)
                    }
                    VIEW_HOLDING_VALUE -> {
                        holder.text.text = d.data
                        holder.text.setTextColor(defaultColor)
                        if (d is DetailsHolding
                                && d.orderable
                                && book.account == null && context is SearchResult) {
                            (holder as ViewHolderHolding).button.text = d.orderLabel
                            holder.button.visibility = View.VISIBLE
                            holder.button.isClickable = holdingOrderClickable
                            holder.button.setOnClickListener { context.orderBookHolding(book, d.holdingId) }
                        } else (holder as? ViewHolderHolding)?.button?.visibility = View.GONE
                    }
                }
            } else {
                holder.text.text = ""
                holder.text.setCompoundDrawablesWithIntrinsicBounds(null, null, null, image)
            }
            return view
        }

        var image: Drawable? = null
        fun updateImage() {
            image = BitmapDrawable(context.resources, book.image)
            notifyDataSetChanged()
        }

        companion object {
            private const val VIEW_HEADER = 0
            private const val VIEW_VALUE = 1
            private const val VIEW_HOLDING_VALUE = 2
        }

        init {
            val resources = context.resources
            if (book.image != null) image = BitmapDrawable(resources, book.image)
            @Suppress("DEPRECATION")
            defaultColor = resources.getColor(android.R.color.primary_text_dark)
            scale = displayMetrics?.scaledDensity ?: 1.0f
            var hs = details.size
            while (hs > 0 && details[hs - 1] is DetailsHolding) hs--
            holdingStartPosition = hs * 2 + 1
        }
    }

    val details = ArrayList<Details>()
    private fun addDetails(label: Int, data: String?){
        data ?: return
        details.add(Details(getString(label) , data))
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
        val lv = activity.findViewById<ListView>(R.id.bookdetailsview) ?: return
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

        lv.adapter = BookDetailsAdapter(activity, details, book)

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
            builder.append(name).append(":")
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
        val defaultOrderTitle = book["orderTitle"].takeNonEmpty() ?: getString(R.string.book_order) ?: ""
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

    private fun getAdapter(v: View?): BookDetailsAdapter? = (v as? ListView)?.adapter as? BookDetailsAdapter

    private val adapter: BookDetailsAdapter?
        get() = getAdapter(findViewById(R.id.bookdetailsview))

    fun updateImage() = adapter?.updateImage()

    fun setOrderButtonsClickable() {
        val lv = activity.findViewById<ListView>(R.id.bookdetailsview) ?: return
        val clickable = !activity.isLoading(VideLibriBaseActivityOld.LOADING_SEARCH_ORDER_HOLDING)
        getAdapter(lv)?.holdingOrderClickable = clickable
        lv.forEachDescendantView { v -> (v as? Button)?.isClickable = clickable }
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


    init {
        activity.registerForContextMenu(findViewById(R.id.bookdetailsview))
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

