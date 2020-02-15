package de.benibela.multilevellistview

import android.content.Context
import androidx.recyclerview.widget.RecyclerView
import android.util.AttributeSet
import android.view.View
import android.view.View.OnClickListener

typealias OnItemClickListener = (RecyclerView, RecyclerView.ViewHolder) -> Unit

open class ClickableRecyclerView @JvmOverloads constructor(context: Context, attrs: AttributeSet? = null, defStyle: Int = 0): RecyclerView(context, attrs, defStyle) {


    abstract class Adapter<VH: ViewHolder>: RecyclerView.Adapter<VH>() {
        open fun registerClickHandler(vh: VH) {
            vh.itemView.setOnClickListener(clickListener)
        }

        private val clickListener = OnClickListener { view ->
            recyclerView?.doItemClick(view)
        }

        var recyclerView: ClickableRecyclerView? = null
        override fun onAttachedToRecyclerView(recyclerView: RecyclerView) {
            super.onAttachedToRecyclerView(recyclerView)
            this.recyclerView = recyclerView as? ClickableRecyclerView
        }

        override fun onDetachedFromRecyclerView(recyclerView: RecyclerView) {
            super.onDetachedFromRecyclerView(recyclerView)
            this.recyclerView = null
        }
    }

    protected fun doItemClick(v: View){
        doItemClick( getChildViewHolder(v) ?: return )
    }
    protected open fun doItemClick(vh: ViewHolder){
        onItemClick(vh)
        for (l in clickListeners) l(this, vh)
    }
    protected open fun onItemClick(vh: ViewHolder){}
    private val clickListeners = mutableListOf<OnItemClickListener>()
    fun addOnItemClickListener(listener: OnItemClickListener) { clickListeners.add(listener) }
    fun removeOnItemClickListener(listener: OnItemClickListener) { clickListeners.remove(listener) }

}
