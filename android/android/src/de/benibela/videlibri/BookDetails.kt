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
import de.benibela.videlibri.BookFormatter.getStatusColor
import de.benibela.videlibri.BookFormatter.getStatusText
import de.benibela.videlibri.CoverLoader.loadBookCover
import de.benibela.videlibri.jni.Bridge
import de.benibela.videlibri.jni.Bridge.Book.StatusEnum
import java.util.*

class BookDetails internal constructor(activity: BookListActivity) : VideLibriFakeFragment(activity) {
    open class Details(var name: String?, var data: CharSequence?) {
        override fun toString(): String {
            return "$name: $data"
        }

        init {
            if (name == null) name = "??"
            if (isAdditionalDisplayProperty(name)) name = name!!.substring(0, name!!.length - 1)
            if (data == null) data = ""
        }
    }

    internal class DetailsHolding(name: String?, data: CharSequence?, holding: Bridge.Book?, holdingId: Int, orderLabel: String) : Details(name, data) {
        val orderable: Boolean
        var orderLabel: String
        var holdingId: Int

        init {
            orderable = holding!!.isOrderableHolding
            this.orderLabel = orderLabel
            this.holdingId = holdingId
        }
    }

    var coverTargetWidth: Int
    var coverTargetHeight: Int

    class BookDetailsAdapter(private val context: Activity, private val details: ArrayList<Details>, book: Bridge.Book) : BaseAdapter() {
        private val holdingStartPosition: Int
        private val book: Bridge.Book?
        val defaultColor: Int
        val scale: Float
        fun toPx(sp: Float): Int {
            return (sp * scale + 0.5f).toInt()
        }

        var holdingOrderClickable = true

        internal open class ViewHolder {
            var text: TextView? = null
        }

        internal class ViewHolderHolding : ViewHolder() {
            var button: Button? = null
        }

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

        override fun getView(position: Int, convertView: View, parent: ViewGroup): View {
            var view = convertView
            val type = getItemViewType(position)
            if (view == null) {
                val viewHolder: ViewHolder
                val inflater = context.layoutInflater
                when (type) {
                    VIEW_HOLDING_VALUE -> {
                        view = inflater.inflate(R.layout.holdingrow, null)
                        viewHolder = ViewHolderHolding()
                        viewHolder.button = view.findViewById<View>(R.id.holdingbutton) as Button
                    }
                    VIEW_HEADER, VIEW_VALUE -> {
                        view = inflater.inflate(R.layout.simpletextview, null)
                        viewHolder = ViewHolder()
                    }
                    else -> {
                        view = inflater.inflate(R.layout.simpletextview, null)
                        viewHolder = ViewHolder()
                    }
                }
                viewHolder.text = view.findViewById<View>(R.id.simpletextview) as TextView
                viewHolder.text!!.autoLinkMask = Linkify.WEB_URLS
                view.tag = viewHolder
                when (type) {
                    VIEW_HEADER -> {
                        viewHolder.text!!.typeface = Typeface.DEFAULT_BOLD
                        viewHolder.text!!.setPadding(toPx(10f), toPx(1f), toPx(10f), toPx(1f))
                    }
                    VIEW_VALUE -> {
                        viewHolder.text!!.typeface = Typeface.DEFAULT
                        viewHolder.text!!.setPadding(toPx(30f), toPx(1f), toPx(10f), toPx(2f))
                    }
                    VIEW_HOLDING_VALUE -> {
                        viewHolder.text!!.typeface = Typeface.DEFAULT
                        viewHolder.text!!.setPadding(toPx(30f), toPx(1f), toPx(10f), toPx(2f))
                    }
                }
            }
            val holder = view.tag as ViewHolder
            if (position > 0) {
                val d = details[(position - 1) / 2]
                when (type) {
                    VIEW_HEADER -> holder.text!!.text = d.name
                    VIEW_VALUE -> {
                        holder.text!!.text = d.data
                        var c = defaultColor
                        if (trStatus == d.name || trDueDate == d.name) {
                            c = getStatusColor(book!!)
                            if (c == -1) c = defaultColor
                        }
                        holder.text!!.setTextColor(c)
                        holder.text!!.setCompoundDrawables(null, null, null, null)
                    }
                    VIEW_HOLDING_VALUE -> {
                        holder.text!!.text = d.data
                        holder.text!!.setTextColor(defaultColor)
                        if (d is DetailsHolding
                                && d.orderable
                                && book!!.account == null && context is SearchResult) {
                            (holder as ViewHolderHolding).button!!.text = d.orderLabel
                            holder.button!!.visibility = View.VISIBLE
                            holder.button!!.isClickable = holdingOrderClickable
                            holder.button!!.setOnClickListener { context.orderBookHolding(book, d.holdingId) }
                        } else (holder as ViewHolderHolding).button!!.visibility = View.GONE
                    }
                }
            } else {
                holder.text!!.text = ""
                holder.text!!.setCompoundDrawablesWithIntrinsicBounds(null, null, null, image)
            }
            return view
        }

        var image: Drawable? = null
        fun updateImage() {
            if (book == null) return
            image = BitmapDrawable(book.image)
            notifyDataSetChanged()
        }

        companion object {
            private const val VIEW_HEADER = 0
            private const val VIEW_VALUE = 1
            private const val VIEW_HOLDING_VALUE = 2
        }

        init {
            this.book = book
            if (book.image != null) image = BitmapDrawable(book.image)
            defaultColor = context.resources.getColor(android.R.color.primary_text_dark)
            scale = displayMetrics?.scaledDensity ?: 1.0f
            var hs = details.size
            while (hs > 0 && details[hs - 1] is DetailsHolding) hs--
            holdingStartPosition = hs * 2 + 1
        }
    }

    var details = ArrayList<Details>()
    fun addIfExists(displayName: String?, propertyName: String?) {
        val value = book!!.getProperty(propertyName!!)
        if (value == null || "" == value) return
        details.add(Details(displayName, value))
    }

    var book: Bridge.Book = Bridge.Book()
    set(newBook) {
        field = newBook
        /*Log.i("VL",  ""+isInLayout());
        Log.i("VL", ""+getSherlockActivity());
        Log.i("VL", ""+getView());       */
        val searchedBook = book.account == null
        val lv = activity.findViewById<ListView>(R.id.bookdetailsview) ?: return
        details.clear()
        var titleData = book.title
        if (book!!.author != null && book!!.author != "") {
            titleData += if (!book!!.author.startsWith("von") && !book!!.author.startsWith("by")) "\n\n" + " " + book!!.author else "\n\n " + book!!.author
        }
        val year = book!!.year
        if (year != null && year != "") titleData += "\n $year"
        val id = book!!.id
        if (id != null && id != "") titleData += "\n $id"
        if (titleData != null && "" != titleData) details.add(Details(tr(R.string.book_titleauthor), titleData))
        trStatus = tr(R.string.book_status)
        trDueDate = tr(R.string.book_duedate)
        if (!searchedBook && !book!!.history || book!!.dueDate != 0) details.add(Details(if (book!!.hasOrderedStatus()) tr(R.string.book_duedate_order) else trDueDate, formatDateFull(book!!.dueDate)))
        val status = getStatusText(book!!)
        if ("" != status) details.add(Details(trStatus, status))
        if (book!!.issueDate != 0 || book!!.firstExistsDate != 0) {
            val s = formatDateFull(if (book!!.issueDate != 0) book!!.issueDate else book!!.firstExistsDate)
            details.add(Details(tr(R.string.book_lenddate), if (book!!.issueDate != 0) s else tr(R.string.book_lenddate_before_prefixS, s)))
        }
        addIfExists(tr(R.string.book_lendat), "libraryBranch")
        addIfExists(tr(R.string.book_libraryLocation), "libraryLocation")
        //addIfExists("ID", "id");
        addIfExists(tr(R.string.book_category), "category")
        //addIfExists("Jahr", "year");
        addIfExists(tr(R.string.book_publisher), "publisher")
        val renewCount = book!!.getProperty("renewCount")
        if ("" != renewCount && "0" != renewCount) details.add(Details(tr(R.string.book_renewCount), renewCount))
        if (book!!.account != null) details.add(Details(tr(R.string.book_account), book!!.account!!.prettyName))
        val above = Arrays.asList("status", "category", "libraryBranch", "publisher", "orderable", "cancelable", "renewCount", "isbn")
        var i = 0
        while (i < book!!.additionalProperties.size) {
            val key = book!!.additionalProperties[i]
            val value = book!!.additionalProperties[i + 1]
            if (!Util.isEmptyString(value)) {
                if (!searchedBook && !above.contains(key)
                        || searchedBook && isAdditionalDisplayProperty(key)) details.add(Details(key, value)) else if ("isbn" == key) details.add(Details("ISBN", value))
            }
            i += 2
        }
        addHoldings(book!!.holdings)
        lv.adapter = BookDetailsAdapter(activity, details, book!!)
        val needToLoadImage = (book!!.hasProperty("image-url") || book!!.hasProperty("isbn")) && book!!.image == null
        if (needToLoadImage) {
            loadBookCover(this, book!!)
        }
        var action: String? = null
        if (searchedBook) {
            if (book!!.isOrderable) {
                action = book!!.getProperty("orderTitle")
                if (action == null || "" == action) action = tr(R.string.book_order)
            }
        } else if (!book!!.history && activity !is RenewList) when (book!!.status) {
            StatusEnum.Unknown -> action = tr(R.string.book_renew)
            StatusEnum.Normal -> action = tr(R.string.book_renew)
            StatusEnum.Ordered, StatusEnum.Provided -> action = if (book!!.isCancelable) tr(R.string.book_cancel) else null
        }
        val actionButton = findButtonById(R.id.button)
        if (action != null) {
            actionButton.text = action
            actionButton.visibility = View.VISIBLE
            actionButton.setOnClickListener { if (activity is BookListActivity && book != null) (activity as BookListActivity).onBookActionButtonClicked(book!!) }
            if (activity is BookListActivity) (activity as BookListActivity).bookActionButton = actionButton
        } else actionButton.visibility = View.GONE
    }

    private inner class HoldingDetailMaker {
        private var builder = SpannableStringBuilder()
        var padding = 0
        var holding: Bridge.Book? = null
        //  String indent;
        fun addPair(name: String?, value: String?) {
            if (Util.isEmptyString(value)) return
            //if (builder.length()>0) builder.append("\n");
//builder.append(indent);
            val len = builder.length
            builder.append(" ")
            builder.setSpan(object : ReplacementSpan() {
                override fun getSize(paint: Paint, text: CharSequence, start: Int, end: Int, fm: FontMetricsInt?): Int {
                    return -padding //undo the indentation of the leadingmarginspan as leadingmarginspan seems to be broken crap: https://issuetracker.google.com/issues/36956124
                }

                override fun draw(canvas: Canvas, text: CharSequence, start: Int, end: Int, x: Float, top: Int, y: Int, bottom: Int, paint: Paint) {}
            }, len, len + 1, Spanned.SPAN_INCLUSIVE_EXCLUSIVE)
            //len++;
            builder.append(name)
            builder.append(": ")
            builder.setSpan(StyleSpan(Typeface.BOLD), len, builder.length, Spanned.SPAN_INCLUSIVE_EXCLUSIVE)
            builder.append(value)
            builder.append("\n")
            builder.setSpan(LeadingMarginSpan.Standard(padding, padding), len, builder.length, Spanned.SPAN_INCLUSIVE_EXCLUSIVE)
        }

        fun addProperty(translation: Int, value: String?) {
            val v = holding!!.getProperty(value!!)
            if (Util.isEmptyString(v)) return
            addPair(tr(translation), v)
        }

        fun build(): CharSequence {
            return builder
        }

        fun clear() {
            builder = SpannableStringBuilder()
        }
    }

    private fun addHoldings(holdings: Array<Bridge.Book>?) {
        if (holdings == null || holdings.size == 0) return
        val builder = HoldingDetailMaker()
        builder.padding = (activity.resources.displayMetrics.scaledDensity * 40 + 0.5).toInt()
        //builder.indent = "      ";
        var defaultOrderTitle: String? = book!!.getProperty("orderTitle")
        if (Util.isEmptyString(defaultOrderTitle)) defaultOrderTitle = tr(R.string.book_order)
        for (i in holdings.indices) {
            builder.clear()
            builder.holding = holdings[i]
            builder.addPair(tr(R.string.book_title), builder.holding!!.title)
            builder.addPair(tr(R.string.book_author), builder.holding!!.author)
            val specialProperties = arrayOf("id", "barcode", "category", "publisher", "libraryBranch", "libraryLocation", "year", "status", "pendingOrders")
            val specialPropertiesLabel = intArrayOf(R.string.book_id, R.string.book_barcode, R.string.book_category, R.string.book_publisher, R.string.book_libraryBranch, R.string.book_libraryLocation, R.string.book_year, R.string.book_status, R.string.book_pendingOrders)
            for (j in specialPropertiesLabel.indices) builder.addProperty(specialPropertiesLabel[j], specialProperties[j])
            var j = 0
            while (j < builder.holding!!.additionalProperties.size) {
                val key = builder.holding!!.additionalProperties[j]
                //Log.i("VIDELIBRIPAIR", pair.first+" : "+pair.second);
                if (isAdditionalDisplayProperty(key)) builder.addPair(key.substring(0, key.length - 1), builder.holding!!.additionalProperties[j + 1])
                j += 2
            }
            if (builder.holding!!.dueDate != 0) builder.addPair(trDueDate, formatDate(builder.holding!!.dueDate))
            val orderTitle = builder.holding!!.getProperty("orderTitle", defaultOrderTitle!!)
            details.add(DetailsHolding(tr(R.string.book_holding_nrD, i + 1), builder.build(), builder.holding, i, orderTitle))
        }
    }

    protected fun getAdapter(v: View?): BookDetailsAdapter? {
        if (v == null) return null
        val lv = v as ListView
        return lv.adapter as BookDetailsAdapter
    }

    protected val adapter: BookDetailsAdapter?
        protected get() = getAdapter(findViewById(R.id.bookdetailsview))

    fun updateImage() {
        val adapter = adapter ?: return
        adapter.updateImage()
    }

    fun setOrderButtonsClickable() {
        val lv = findViewById(R.id.bookdetailsview) as ListView ?: return
        val clickable = !activity.isLoading(VideLibriBaseActivityOld.LOADING_SEARCH_ORDER_HOLDING)
        getAdapter(lv)!!.holdingOrderClickable = clickable
        Util.iterateChildViews(lv) { v -> (v as? Button)?.isClickable = clickable }
    }

    fun exportShare(html: Boolean): String {
        val sb = StringBuilder()
        for (i in details.indices) {
            if (html) sb.append("<b>")
            sb.append(details[i].name)
            if (html) sb.append("</b>")
            if (details[i].name != null && !details[i].name!!.endsWith(":")) sb.append(":")
            sb.append("\n")
            sb.append(details[i].data)
            if (html) sb.append("<br>")
            sb.append("\n\n")
        }
        return sb.toString()
    }

    companion object {
        var trStatus = ""
        var trDueDate = ""
        fun isAdditionalDisplayProperty(s: String?): Boolean {
            return s!!.endsWith("!")
        }

        var displayMetrics: DisplayMetrics? = null
    }

    init {
        val lv = findViewById(R.id.bookdetailsview)
        if (lv != null) activity.registerForContextMenu(lv)
        displayMetrics = activity.resources.displayMetrics.also { dm ->
            val longSide = Math.max(dm.widthPixels, dm.heightPixels)
            val shortSide = Math.min(dm.widthPixels, dm.heightPixels)
            //portrait: maxWidth = shortSide, maxHeight = longSide * factor
            //landscape: maxWidth = longSide / 2, maxHeight = shortSide * factor
            coverTargetWidth = Math.max(3, Math.min(shortSide, longSide / 2))
            coverTargetHeight = Math.max(3, shortSide / 2)
        }
    }
}