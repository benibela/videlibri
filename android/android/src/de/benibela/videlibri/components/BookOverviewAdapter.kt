package de.benibela.videlibri.components

import android.graphics.Color
import android.graphics.Typeface
import android.view.Gravity
import android.view.View
import android.view.ViewGroup
import de.benibela.multilevellistview.ClickableRecyclerView
import de.benibela.videlibri.R
import de.benibela.videlibri.activities.BookListActivity
import de.benibela.videlibri.jni.BookListDisplayOptions
import de.benibela.videlibri.utils.BookFormatter.shortened
import de.benibela.videlibri.databinding.BookOverviewRowBinding
import de.benibela.videlibri.jni.Bridge
import de.benibela.videlibri.jni.isGrouped
import de.benibela.videlibri.utils.BookFormatter
import de.benibela.videlibri.utils.getDateText
import de.benibela.videlibri.utils.getMoreText
import de.benibela.videlibri.utils.getStatusColor
import java.util.*

private const val VIEW_TYPE_DEFAULT = 0
private const val VIEW_TYPE_NODETAILS = 1
private const val VIEW_TYPE_GROUPING = 2


internal class BookOverviewAdapter(private val context: BookListActivity, books: ArrayList<Bridge.Book>, count: Int, private val options: BookListDisplayOptions) : ClickableRecyclerView.Adapter<ViewHolderWithBinding<*>>() {
    private val defaultColor: Int = context.resources.getColor(android.R.color.primary_text_dark)
    private val defaultColorSecondary: Int = context.resources.getColor(android.R.color.secondary_text_dark)
    private val defaultBackgroundColor: Int = context.resources.getColor(android.R.color.background_dark)
    private val placeHolder: Bridge.Book = Bridge.Book()

    var books: ArrayList<Bridge.Book> = books
    set(value){
        field = value
        notifyDataSetChanged()
    }

    private val completeCount = kotlin.math.max(count, books.size)

    override fun getItemCount(): Int = completeCount

    override fun getItemViewType(position: Int): Int = get(position).let { book ->
        if (options.isGrouped() && book.isGroupingHeader) VIEW_TYPE_GROUPING
        else if (options.noBorrowedBookDetails) VIEW_TYPE_NODETAILS
        else VIEW_TYPE_DEFAULT
    }

    override fun onCreateViewHolder(parent: ViewGroup, viewType: Int): ViewHolderWithBinding<*> =
        ViewHolderWithBinding(parent, BookOverviewRowBinding::inflate).also {
            when (viewType) {
                VIEW_TYPE_GROUPING -> {
                    it.binding.caption.apply {
                        setTypeface(typeface, Typeface.BOLD)
                        gravity = Gravity.CENTER_HORIZONTAL
                    }
                    it.binding.more.visibility = View.GONE
                }
                VIEW_TYPE_NODETAILS -> {
                    it.binding.more.visibility = View.GONE
                }
            }
            registerClickHandler(it)
        }

    override fun onBindViewHolder(holder: ViewHolderWithBinding<*>, position: Int) {
        val book = books.getOrNull(position) ?: placeHolder
        val binding = holder.binding as BookOverviewRowBinding
        binding.caption.text = shortened(book.title)

        val isGroupingHeader = options.isGrouped() && book.isGroupingHeader
        if (options.isGrouped()) {
            if (isGroupingHeader) {
                binding.caption.setTextColor(book.getStatusColor())
            } else {
                binding.caption.setTextColor(defaultColorSecondary)
            }
        }
        if (book === placeHolder) {
            context.onPlaceHolderShown(position)
            binding.more.text = book.author //not an author
        } else if (!isGroupingHeader) {
            binding.more.text = book.getMoreText()
        }
        val sb = context.selectedBooks
        if (sb != null) {
            val isSelected = sb.contains(book)
            if (isSelected) holder.itemView.setBackgroundColor(Color.rgb(0, 0, 96))
            else holder.itemView.setBackgroundColor(defaultBackgroundColor)
        }
        binding.date.text = book.getDateText(options)
        var c = book.getStatusColor()
        if (c == -1) c = defaultColor
        binding.date.setTextColor(c)
    }



    operator fun get(i: Int): Bridge.Book = books.getOrElse(i) { placeHolder }

    fun exportShare(html: Boolean): String {
        val newline = if (html) "<br>\n" else "\n"
        val paragraph = newline + newline

        return books.indices.joinToString(paragraph) { pos ->
            val book = books[pos]
            val isGroupingHeader = options.isGrouped() && book.isGroupingHeader
            if (isGroupingHeader) {
                if (pos == 0) book.title
                else "$paragraph${book.title}"
            } else  {
                val more = if (!options.noBorrowedBookDetails) book.getMoreText() else ""
                val date = if (!book.history) ": ${book.getDateText(options)}" else ""
                "${book.title} $more$date"
            }
        }
    }

    init {
        //super(context, R.layout.bookoverview, books);
        placeHolder.author = context.getString(R.string.booklist_loading)
        BookFormatter.tr.init(context)
    }
}