package de.benibela.multilevellistview

import android.content.Context
import android.graphics.drawable.Drawable
import android.graphics.drawable.StateListDrawable
import androidx.recyclerview.widget.LinearLayoutManager
import androidx.recyclerview.widget.RecyclerView
import android.util.AttributeSet
import android.util.TypedValue
import android.view.ViewGroup
import android.widget.TextView


@Suppress("RemoveRedundantQualifierName", "unused", "MemberVisibilityCanBePrivate")
open class ExpandableMultiLevelListView @JvmOverloads constructor(context: Context, attrs: AttributeSet? = null, defStyle: Int = 0): MultiLevelListView(context, attrs, defStyle), PartialMultiLevelListView {
    abstract class ExpandableAdapter<VH: RecyclerView.ViewHolder>: MultiLevelListView.Adapter<VH>() {
        abstract fun isExpanded(position: IntArray): Boolean
        open fun isExpanded(id: Long) = isExpanded(idToPosition(id))

        abstract fun expand(position: IntArray): Int
        open fun expand(id: Long) = expand(idToPosition(id))

        abstract fun collapse(position: IntArray)
        open fun collapse(id: Long) = collapse(idToPosition(id))

        abstract fun getPotentialChildCount(position: IntArray): Int
        open fun getPotentialChildCount(id: Long): Int = getPotentialChildCount(idToPosition(id))

        override fun getChildCount(id: Long): Int =
            if (!isExpanded(id shr bitsPerLevel)) 0
            else getChildCount(idToPosition(id))
    }
    open class DefaultExpandableAdapter<VH: RecyclerView.ViewHolder>(private val wrapped: MultiLevelListView.Adapter<VH>): ExpandableAdapter<VH>() {

        private val expanded = mutableSetOf(0L)

        override val bitsPerLevel = wrapped.bitsPerLevel

        override fun getLevels(): Int = wrapped.getLevels()

        override fun isExpanded(position: IntArray): Boolean = isExpanded(positionToId(position))
        override fun isExpanded(id: Long) = expanded.contains(id)

        override fun getPotentialChildCount(position: IntArray): Int = wrapped.getChildCount(position)
        override fun getPotentialChildCount(id: Long): Int = wrapped.getChildCount(id)

        override fun getChildCount(position: IntArray): Int = getChildCount(positionToId(position))
        override fun getChildCount(id: Long): Int =
                if (!isExpanded(id )) 0
                else wrapped.getChildCount(id)

        override fun getItemViewType(position: IntArray): Int = wrapped.getItemViewType(position)
        override fun getItemViewType(id: Long): Int = wrapped.getItemViewType(id)

        override fun onCreateViewHolder(parent: ViewGroup, viewType: Int): VH =
            wrapped.onCreateViewHolder(parent, viewType).also { registerClickHandler(it) }

        override fun onBindViewHolder(holder: VH, position: IntArray){
            wrapped.onBindViewHolder(holder, position)
            (recyclerView as? ExpandableMultiLevelListView)?.let { rv ->
                val indicator = when {
                    wrapped.getChildCount(position) == 0 -> null
                    isExpanded(holder.itemId) ->            rv.groupIndicatorExpanded
                    else ->                                 rv.groupIndicator
                }
                (holder.itemView as? TextView)?.setCompoundDrawablesWithIntrinsicBounds(indicator, null, null, null)
            }
        }

        override fun expand(position: IntArray) = expand(positionToId(position))
        override fun expand(id: Long): Int {
            if (isExpanded(id) || wrapped.getChildCount(id) == 0) return 0
            val linearPosition = idToLinearPosition(id)
            expanded.add(id)
            val descendants = countDescendants(id)

            //Log.i("LISTVIEW", "EXPAND: $id $linearPosition $descendants")

            if (descendants > 0)
                notifyItemRangeInserted(linearPosition + 1, descendants)
            notifyItemChanged(linearPosition)

            return descendants
        }

        override fun collapse(position: IntArray) = collapse(positionToId(position))
        override fun collapse(id: Long){
            if (!isExpanded(id) || id == 0L) return
            val linearPosition = idToLinearPosition(id)
            val descendants = countDescendants(id)
            expanded.remove(id)

            //Log.i("LISTVIEW", "COLLAPSE: $id $linearPosition $descendants")

            if (descendants > 0)
                notifyItemRangeRemoved(linearPosition + 1, descendants)
            notifyItemChanged(linearPosition)
        }


    }
    override fun setAdapter(adapter: RecyclerView.Adapter<*>?) {
        when (adapter) {
            is ExpandableAdapter -> super.setAdapter(adapter)
            is MultiLevelListView.Adapter -> super.setAdapter(DefaultExpandableAdapter(adapter))
            else -> throw UnsupportedOperationException()
        }
    }

    var expandableAdapter: ExpandableAdapter<*>?
        set(value){ adapter = value }
        get() = adapter as ExpandableAdapter<*>?

    var groupIndicator: StateListDrawable? = null
        set(value) {
            field = value
            value?.state = intArrayOf(android.R.attr.state_expanded)
            groupIndicatorExpanded = value?.current
        }

    private var groupIndicatorExpanded: Drawable? = null

    override fun expand(id: Long): Int =
        expandableAdapter?.let { ea ->
            val descendants = ea.expand(id)

            if (descendants > 0) {
                if (autoExpandSingleChildren && ea.getChildCount(id) == 1 && ea.getPotentialChildCount(ea.childId(ea.bitsPerLevel, id, 0)) > 0)
                    if (expand(ea.childId(ea.bitsPerLevel, id, 0)) > 0)
                        return descendants
                if (autoScrollOnExpansion) {
                    val lm = layoutManager as? LinearLayoutManager
                            ?: return descendants
                    val linearPosition = ea.idToLinearPosition(id)
                    val lastPosition = lm.findLastCompletelyVisibleItemPosition()
                    val firstPosition = lm.findFirstCompletelyVisibleItemPosition()
                    val visibleItemCount = lastPosition - firstPosition + 1
                    val scrollTarget = if (visibleItemCount >= descendants) linearPosition + descendants else linearPosition + visibleItemCount - 1
                    //Log.i("LISTVIEW", "Scroll to: $scrollTarget   $linearPosition $firstPosition $lastPosition ")
                    smoothScrollToPosition(scrollTarget)
                }
            }

            descendants 
        } ?: 0
    
    fun collapse(id: Long){
        expandableAdapter?.collapse(id)
    }


    override fun doItemClick(vh: ViewHolder) {
        super.doItemClick(vh)
        expandableAdapter?.let { ea ->
            if (!ea.isExpanded(vh.itemId)) expand(vh.itemId)
            else ea.collapse(vh.itemId)
        }
    }

    override var autoExpandSingleChildren = true
    var autoScrollOnExpansion = true

    init {
        val typedValue = TypedValue()
        if (context.theme.resolveAttribute(android.R.attr.expandableListViewStyle, typedValue, true)) {
            groupIndicator = context.theme.obtainStyledAttributes(typedValue.resourceId, intArrayOf(android.R.attr.groupIndicator)).getDrawable(0) as? StateListDrawable
        }
    }
}
