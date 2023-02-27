package de.benibela.multilevellistview

import android.annotation.SuppressLint
import android.content.Context
import androidx.recyclerview.widget.LinearLayoutManager
import androidx.recyclerview.widget.RecyclerView
import android.util.AttributeSet
import android.view.Gravity
import android.view.View
import android.view.ViewGroup
import android.widget.LinearLayout

@Suppress("unused", "MemberVisibilityCanBePrivate")
open class MultiColumnListView @JvmOverloads constructor(context: Context, attrs: AttributeSet? = null, defStyleAttr: Int = 0): LinearLayout(context, attrs, defStyleAttr), PartialMultiLevelListView{
    abstract class SingleLevelAdapter<VH: RecyclerView.ViewHolder>: ClickableRecyclerView.Adapter<VH>(){
        open var currentParent: Long = -1L
            set(value) {
                if (field == value) return
                field = value
                notifyDataSetChanged()
            }

    }
    class DefaultSingleLevelAdapter<VH: RecyclerView.ViewHolder>(private val wrapped: MultiLevelListView.Adapter<VH>, currentParent: Long ): SingleLevelAdapter<VH>(){
        override fun onCreateViewHolder(parent: ViewGroup, viewType: Int): VH =
            wrapped.onCreateViewHolder(parent, viewType).also { registerClickHandler(it) }
        override fun getItemCount(): Int =
                if (currentParent == -1L) 0
                else wrapped.getChildCount(currentParent)
        override fun onBindViewHolder(holder: VH, position: Int) =
                wrapped.onBindViewHolder(holder, position)

        fun linearPositionToId(position: Int): Long =
                if (currentParent > -1L && position < wrapped.getChildCount(currentParent))
                    wrapped.childId(wrapped.bitsPerLevel, currentParent, position)
                else View.NO_ID.toLong()

        override fun getItemId(position: Int) = linearPositionToId(position)
        override fun getItemViewType(position: Int) = wrapped.getItemViewType(linearPositionToId(position))


        init {
            setHasStableIds(true)
            this.currentParent = currentParent
        }
    }

    @SuppressLint("ViewConstructor")
    open class SingleColumnListView(val parent: MultiColumnListView, attrs: AttributeSet? = null, defStyleAttr: Int = 0): ClickableRecyclerView(parent.context, attrs, defStyleAttr){
        override fun doItemClick(vh: ViewHolder) {
            super.doItemClick(vh)
            for (l in parent.clickListeners) l(this, vh)
            parent.expand(vh.itemId)
        }
    }

    protected var subviews = emptyList<SingleColumnListView>()
    open var adapter: MultiLevelListView.Adapter<*>? = null
        set(value){
            field = value
            removeAllViews()


            subviews = (0 until (value?.getLevels()?:0)).map {SingleColumnListView(this).also { view ->
                val lp = LayoutParams(LayoutParams.MATCH_PARENT, LayoutParams.MATCH_PARENT, 1.0f)
                lp.gravity = Gravity.END or Gravity.CENTER_VERTICAL

                addView(view, lp)
                value?.let {  view.adapter = DefaultSingleLevelAdapter(value, -1L) }
                view.layoutManager = LinearLayoutManager(context)
            }}
            expand(0)
        }

    override fun expand(id: Long): Int {
        var expandedCount = 0
        adapter?.let { adapter ->
            if (!adapter.isValidId(id)) return 0
            val bits = adapter.bitsPerLevel
            val level = adapter.idToLevel(id)
            val ancestors = (0..level).map { id shr (bits * it) }.reversed()
            for (i in 0 until level)
                (subviews[i].adapter as? SingleLevelAdapter)?.currentParent = ancestors[i]
            var p = id
            for (i in level until subviews.size) {
                (subviews[i].adapter as? SingleLevelAdapter)?.currentParent = p
                val children = adapter.getChildCount(p)
                expandedCount += children
                p = if (p == -1L || children == 0 || !autoExpandSingleChildren) -1
                else adapter.childId(bits, p, 0)
            }
        }
        return expandedCount
    }

    override var autoExpandSingleChildren: Boolean = true

    private val clickListeners = mutableListOf<OnItemClickListener>()
    fun addOnItemClickListener(listener: OnItemClickListener) { clickListeners.add(listener) }
    fun removeOnItemClickListener(listener: OnItemClickListener) { clickListeners.remove(listener) }


    init {
        orientation = HORIZONTAL
    }
}
