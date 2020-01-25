package de.benibela.multilevellistview

import android.content.Context
import android.support.v7.widget.RecyclerView
import android.util.AttributeSet
import android.util.Log
import java.lang.UnsupportedOperationException


class MultiLevelListView @JvmOverloads constructor(context: Context, attrs: AttributeSet? = null, defStyle: Int = 0): RecyclerView(context, attrs, defStyle) {
    abstract class Adapter<VH: RecyclerView.ViewHolder>: RecyclerView.Adapter<VH>() {
        abstract fun getLevels(): Int
        abstract fun getChildCount(position: IntArray): Int
        abstract fun getItemViewType(position: IntArray): Int
        //abstract fun onCreateViewHolder(parent: ViewGroup, viewType: Int, level: Int): VH
        abstract fun onBindViewHolder(holder: VH, position: IntArray)

        init {
            setHasStableIds(true)
        }

        open val bitsPerLevel = 16
        private var arrayCache = emptyArray<IntArray>()
        protected fun idToLevel(id: Long): Int{
            val bits = bitsPerLevel
            var level = 0
            var temp = id
            while (temp > 0) {
                temp = temp shr bits
                level++
            }
            return level
        }
        protected fun idToPosition(id: Long): IntArray{
            val bits = bitsPerLevel
            val mask: Long = (1L shl bits) - 1
            val level = idToLevel(id)
            if (level >= arrayCache.size) arrayCache = Array(level + 1) { IntArray(it) }
            val res = arrayCache[level]
            var l = level - 1
            var temp = id
            while (temp > 0) {
                res[l] = (temp and mask).toInt() - 1
                temp = temp shr bits
                l--
            }
            return res
        }
        protected fun positionToId(position: IntArray): Long{
            val bits = bitsPerLevel
            return position.fold(0L) { id, p -> (id shl bits) or (p + 1).toLong()}
        }
        //gets child position. Make sure arrayCache is big enough!
        protected fun positionToChildPosition(position: IntArray, child: Int = 0): IntArray{
            val res = arrayCache[position.size + 1]
            for (i in position.indices) res[i] = position[i]
            res[position.size] = child
            return res
        }
        protected fun positionToParentPosition(position: IntArray): IntArray{
            val res = arrayCache[position.size - 1]
            for (i in res.indices) res[i] = position[i]
            return res
        }

        protected fun nextPosition(pos: IntArray): IntArray{
            val tc = getChildCount(pos)
            if (tc > 0)
                return positionToChildPosition(pos, 0)
            var oldpos = pos
            var parent = positionToParentPosition(pos)
            while (oldpos.size > 0) {
                val nextChild = oldpos[oldpos.size - 1] + 1
                val count = getChildCount(parent)
                if (nextChild < count)
                    return positionToChildPosition(parent, nextChild)
                oldpos = parent
                parent = positionToParentPosition(parent)
            }
            return arrayCache[0]
        }
        protected fun linearPositionToPosition(pos: Int): IntArray{
            var curPos = arrayCache[0]
            var p = pos
            while (p >= 0) {
                curPos = nextPosition(curPos)
                p--
            }
            Log.d("LISTVIEW", pos.toString() + " => "+curPos.joinToString())
            return curPos
        }
        protected fun getItemCountSum(position: IntArray): Int{
            val c = getChildCount(position)
            var res = c
            if (c > 0) {
                val child = positionToChildPosition(position)
                for (i in 0 until c) {
                    child[position.size] = i
                    res += getItemCountSum(child)
                }
            }
            return res
        }

        //override fun onCreateViewHolder(parent: ViewGroup, viewType: Int): VH =
        //        onCreateViewHolder(parent, viewType >> 16)
        override fun onBindViewHolder(holder: VH, position: Int){
            val pos = idToPosition(holder.itemId)
            Log.d("LISTVIEW", pos.joinToString() + ": " + position+ " " + holder.itemId)
            onBindViewHolder(holder, pos)
        }
        override fun getItemCount(): Int{
            val levels = getLevels()
            if (levels >= arrayCache.size) arrayCache = Array(levels + 1) { IntArray(it) }
            return getItemCountSum(arrayCache[0])
        }
        override fun getItemId(position: Int): Long = positionToId(linearPositionToPosition(position))
        override fun getItemViewType(position: Int) = getItemViewType(linearPositionToPosition(position))

    }

    override fun setAdapter(adapter: RecyclerView.Adapter<*>?) {
        if (adapter !is Adapter) throw UnsupportedOperationException()
        super.setAdapter(adapter)
    }
}