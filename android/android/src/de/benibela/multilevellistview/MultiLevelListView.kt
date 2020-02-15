package de.benibela.multilevellistview

import android.content.Context
import androidx.recyclerview.widget.RecyclerView
import android.util.AttributeSet


@Suppress("RedundantVisibilityModifier", "RemoveRedundantQualifierName", "MemberVisibilityCanBePrivate")
open class MultiLevelListView @JvmOverloads constructor(context: Context, attrs: AttributeSet? = null, defStyle: Int = 0): ClickableRecyclerView(context, attrs, defStyle) {
    abstract class Adapter<VH: RecyclerView.ViewHolder>: ClickableRecyclerView.Adapter<VH>() {
        abstract fun getLevels(): Int
        abstract fun getChildCount(position: IntArray): Int
        abstract fun getItemViewType(position: IntArray): Int
        abstract fun onBindViewHolder(holder: VH, position: IntArray)

        open fun getChildCount(id: Long): Int = getChildCount(idToPosition(id))
        open fun getItemViewType(id: Long): Int = getItemViewType(idToPosition(id))

        init {
            @Suppress("LeakingThis")
            setHasStableIds(true)
        }

        open val bitsPerLevel = 16
        private var arrayCache = emptyArray<IntArray>()
        @Suppress("NOTHING_TO_INLINE")
        inline fun childId(bits: Int, id: Long, child: Int) = (id shl bits) or (child + 1).toLong()
        @Suppress("NOTHING_TO_INLINE")
        inline fun parentId(bits: Int, id: Long) = id shr bits
        @Suppress("NOTHING_TO_INLINE")
        inline fun levelMask(bits: Int) = (1 shl bits) - 1L
        public fun idToLevel(id: Long): Int{
            val bits = bitsPerLevel
            var level = 0
            var temp = id
            while (temp > 0) {
                temp = parentId(bits, temp)
                level++
            }
            return level
        }
        public fun idToPosition(id: Long): IntArray{
            val bits = bitsPerLevel
            val mask: Long = levelMask(bits)
            val level = idToLevel(id)
            if (level >= arrayCache.size) arrayCache = Array(level + 1) { IntArray(it) }
            val res = arrayCache[level]
            var l = level - 1
            var temp = id
            while (temp > 0) {
                res[l] = (temp and mask).toInt() - 1
                temp = parentId(bits, temp)
                l--
            }
            return res
        }
        public fun positionToId(position: IntArray): Long{
            val bits = bitsPerLevel
            return position.fold(0L) { id, p -> childId (bits, id, p + 1)}
        }

        protected fun nextId(id: Long): Long{
            val bits = bitsPerLevel
            val tc = getChildCount(id)
            if (tc > 0) return childId(bits, id, 0)
            val mask = levelMask(bits)
            var oldpos = id
            var parent = parentId(bits, id)
            while (oldpos > 0) {
                val nextChild = oldpos and mask // -1 + 1
                val count = getChildCount(parent)
                if (nextChild < count)
                    return childId(bits, parent, nextChild.toInt())
                oldpos = parent
                parent = parentId(bits, parent)
            }
            return 0
        }
        protected fun linearPositionToId(pos: Int): Long{
            var curPos = 0L
            var p = pos
            while (p >= 0) {
                curPos = nextId(curPos)
                p--
            }
            //Log.d("LISTVIEW", pos.toString() + " => "+curPos)
            return curPos
        }
        internal fun idToLinearPosition(id: Long): Int {
            val bits = bitsPerLevel
            val mask = levelMask(bits)
            var pid = id
            var res = 0
            while (pid > 0) {
                when ((pid and mask).toInt()) {
                    0, 1 -> { //0 should not happen
                        pid = parentId(bits, pid)
                        res++
                    }
                    else -> {
                        pid--
                        res += countDescendants(pid) + 1
                    }
                }
            }
            if (res > 0) res-- //the loop above counts self
            return res
        }
        public fun isValidId(id: Long): Boolean{
            val bits = bitsPerLevel
            val pos = idToPosition(id).clone()
            val level = pos.size
            val ancestors = (0..level).map { id shr (bits * it) }.reversed()
            for (i in 0 until level)
                if (pos[i] >= getChildCount(ancestors[i]))
                    return false
            return true
        }
        public fun countDescendants(id: Long): Int {
            val c = getChildCount(id)
            val childId = childId (bitsPerLevel, id, 0)
            var res = c
            for (i in 0 until c) res += countDescendants(childId + i)
            return res
        }
        protected fun getItemCountSum(id: Long): Int{
            val c = getChildCount(id)
            var res = c
            if (c > 0) {
                val child = childId (bitsPerLevel, id, 0)
                for (i in 0 until c)
                    res += getItemCountSum(child + i)
            }
            return res
        }

        //override fun onCreateViewHolder(parent: ViewGroup, viewType: Int): VH =
        //        onCreateViewHolder(parent, viewType >> 16)
        override fun onBindViewHolder(holder: VH, position: Int){
            val pos = idToPosition(holder.itemId)
            //Log.d("LISTVIEW", pos.joinToString() + ": " + position+ " " + holder.itemId)
            onBindViewHolder(holder, pos)
        }
        override fun getItemCount(): Int{
            val levels = getLevels()
            if (levels >= arrayCache.size) arrayCache = Array(levels + 1) { IntArray(it) }
            return getItemCountSum(0)
        }
        override fun getItemId(position: Int): Long = linearPositionToId(position)
        override fun getItemViewType(position: Int) = getItemViewType(linearPositionToId(position))

    }

    override fun setAdapter(adapter: RecyclerView.Adapter<*>?) {
        if (adapter !is Adapter) throw UnsupportedOperationException()
        super.setAdapter(adapter)
    }


}