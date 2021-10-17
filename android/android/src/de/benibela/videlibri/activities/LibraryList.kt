package de.benibela.videlibri.activities

import android.annotation.SuppressLint
import android.content.Intent
import android.os.Bundle
import android.os.Parcelable
import android.view.*
import android.widget.AdapterView
import android.widget.ArrayAdapter
import android.widget.TextView
import androidx.annotation.LayoutRes
import androidx.recyclerview.widget.LinearLayoutManager
import androidx.recyclerview.widget.RecyclerView
import de.benibela.multilevellistview.ExpandableMultiLevelListView
import de.benibela.multilevellistview.MultiColumnListView
import de.benibela.multilevellistview.MultiLevelListView
import de.benibela.multilevellistview.PartialMultiLevelListView
import de.benibela.videlibri.*
import de.benibela.videlibri.databinding.BookOverviewRowBinding
import de.benibela.videlibri.databinding.DialogBookListLikeBinding
import de.benibela.videlibri.jni.Bridge
import de.benibela.videlibri.utils.*
import kotlinx.android.parcel.Parcelize
import java.lang.IndexOutOfBoundsException
import java.util.*

class LibraryListAdapter: MultiLevelListView.Adapter<LibraryListAdapter.Holder>(){
    private val states: MutableList<String> = ArrayList()
    private val cities: MutableList<MutableList<String>> = ArrayList()
    private val localLibs: MutableList<MutableList<MutableList<String>>> = ArrayList()

    class Holder(view: View): RecyclerView.ViewHolder(view){
        val text = view as TextView
    }

    override fun getLevels(): Int = 3
    override fun getChildCount(position: IntArray): Int = try {
        when (position.size) {
            0 -> states.size
            1 -> cities[position[0]].size
            2 -> localLibs[position[0]][position[1]].size
            else -> 0
        }
    } catch (e: IndexOutOfBoundsException) {
        0
    }
    override fun getItemViewType(position: IntArray): Int = position.size
    private fun holder(parent: ViewGroup, @LayoutRes view: Int): Holder {
        val v = LayoutInflater.from(parent.context).inflate(view, parent, false)
        return Holder(v)
    }
    override fun onCreateViewHolder(parent: ViewGroup, viewType: Int): Holder =
        when (viewType) {
            1 -> holder(parent, android.R.layout.simple_expandable_list_item_1).apply {
                itemView.setBackgroundResource(android.R.drawable.list_selector_background)
            }
            2 -> holder(parent, R.layout.librarycityinlistview)
            3 -> holder(parent, R.layout.libraryinlistview)
            else -> Holder(TextView(parent.context))
        }

    override fun onBindViewHolder(holder: Holder, position: IntArray) = try {
        when (position.size) {
            1 -> holder.text.text = states[position[0]]
            2 -> holder.text.text = cities[position[0]][position[1]]
            3 -> holder.text.text = getLibraryText(getLibraryId(position))
            else -> {}
        }
    } catch (e: IndexOutOfBoundsException) {
        @SuppressLint("SetTextI18n")
        holder.text.text = "MISSING"
    }
    private val detailCache = mutableMapOf<String, Bridge.LibraryDetails>()
    internal fun getLibraryDetails(id: String): Bridge.LibraryDetails =
            detailCache.getOrPut(id) { Bridge.VLGetLibraryDetails(id) ?: Bridge.LibraryDetails() }
    internal fun getLibraryId(position: IntArray) =
            localLibs[position[0]][position[1]][position[2]]
    private fun getLibraryText(id: String): String{
        val info = getLibraryDetails(id)
        val name = info.prettyName
        return info.tableComment.takeNonEmpty()?.let { "$name\n\t$it" } ?: name
    }

    val metaCatalogs: Int
    val autoExpand: Int

    init {
        var localMetaCatalogs = -1
        var localAutoExpand = 0

        val libs = Bridge.VLGetLibraryIds()

        if (Accounts.size > 0) {
            localAutoExpand = 1
            states.add(getString(R.string.liblist_withaccounts))
            cities.add(mutableListOf(getString(R.string.liblist_withaccounts)))
            localLibs.add(mutableListOf(Accounts.toArray.map { it.libId }.toSet().toMutableList()))
        }

        var lastIdSplit = listOf("", "", "", "")
        for (id in libs) {
            val split = id.split('_')
            if (split.size < 3) continue
            if (states.isEmpty() || split[0] != lastIdSplit[0] || split[1] != lastIdSplit[1]) {
                if (localMetaCatalogs < 0 && id.contains("ueberregional")) localMetaCatalogs = states.size
                states.add(Bridge.LibraryDetails.decodeIdEscapes(split[0] + " - " + split[1]))
                cities.add(ArrayList())
                localLibs.add(ArrayList())
            }
            val curCities = cities[cities.size - 1]
            if (curCities.isEmpty() || split[2] != lastIdSplit[2]) {
                curCities.add(Bridge.LibraryDetails.decodeIdEscapes(split[2]))
                if ("-" == split[2] && localAutoExpand < 2) localAutoExpand += 1
                localLibs[localLibs.size - 1].add(ArrayList())
            }
            localLibs[localLibs.size - 1][localLibs[localLibs.size - 1].size - 1].add(id)
            lastIdSplit = split
        }

        autoExpand = localAutoExpand
        metaCatalogs = localMetaCatalogs
    }
}

class LibraryList: VideLibriBaseActivity() {
    @Parcelize
    class State(var lastClickedItem: Long = -1L): Parcelable
    private var state = State()

    internal lateinit var adapter: LibraryListAdapter
    private var listView: ExpandableMultiLevelListView? = null
    private var columnView: MultiColumnListView? = null


    override fun onCreateOptionsMenuOverflow(menu: Menu, inflater: MenuInflater) {
        super.onCreateOptionsMenuOverflow(menu, inflater)
        inflater.inflate(R.menu.librarylistmenu, menu)
    }


    private fun onLeafClick(@Suppress("UNUSED_PARAMETER") rv: RecyclerView, vh: RecyclerView.ViewHolder) {
        state.lastClickedItem = vh.itemId
        val p = adapter.idToPosition(vh.itemId)
        if (p.size != 3) return
        val id = adapter.getLibraryId(p)
        lastSelectedLibId = id
        lastSelectedLibName = adapter.getLibraryDetails(id).prettyName
        lastSelectedTime = System.currentTimeMillis()

        finishWithResult()
    }

    public override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        registerState(::state)
        setVideLibriView(R.layout.librarylist)

        intent.getStringExtra("reason")?.takeNonEmpty()?.let { findViewById<TextView>(R.id.textView) }


        val portMode = resources.getBoolean(R.bool.port_mode)
        if (portMode) listView = findViewById<ExpandableMultiLevelListView>(R.id.libListView).also {
            it.layoutManager = LinearLayoutManager(this)
            it.addOnItemClickListener(::onLeafClick)
        }
        else columnView = findViewById<MultiColumnListView>(R.id.libColumnListView).also {
            it.addOnItemClickListener(::onLeafClick)
        }
        createListAdapter()

        findViewById<View>(R.id.textViewLibWhyNot).apply {
            if (accounts.isNotEmpty())
                visibility = View.GONE
            else
                setOnClickListener { showHowToAddLibraryDialog() }
        }

    }

    private fun restoreExpansion(l: PartialMultiLevelListView) {
        val lastClickedItem = state.lastClickedItem
        if (lastClickedItem <= 0) return
        val bits = adapter.bitsPerLevel
        val level = adapter.idToLevel(lastClickedItem)
        val ancestors = (0 until level).map { lastClickedItem shr (bits * it) }.reversed()
        ancestors.take(level - 1).forEach { l.expand(it) }
        (l as? ExpandableMultiLevelListView)?.autoScrollOnExpansion = true
        ancestors.takeLast(1).forEach { l.expand(it) }
    }

    private fun createListAdapter(){

        adapter = LibraryListAdapter()
        listView?.let {
            it.adapter = adapter
            it.autoScrollOnExpansion = false
            for (i in (adapter.autoExpand - 1) downTo 0 )
                it.expand(adapter.childId(adapter.bitsPerLevel, 0, i))
            restoreExpansion(it)
            it.autoScrollOnExpansion = true
        }
        columnView?.let {
            it.adapter = adapter
            restoreExpansion(it)
        }
    }

    public override fun onActivityResult(requestCode: Int, resultCode: Int, data: Intent?) {
        when (requestCode) {
            RETURNED_FROM_NEW_LIBRARY -> createListAdapter()
        }
        super.onActivityResult(requestCode, resultCode, data)
    }


    private fun showHowToAddLibraryDialog(){
        de.benibela.videlibri.utils.showDialog {
            onCreate = onCreate@{ builder ->
                val activity = activity ?: return@onCreate
                val inflater = activity.layoutInflater
                val dialogBinding = DialogBookListLikeBinding.inflate(inflater)

                val items = intArrayOf(R.string.foreignlibrariesnotinthelist_easy, R.string.foreignlibrariesnotinthelist_meta, R.string.foreignlibrariesnotinthelist_install, R.string.foreignlibrariesnotinthelist_diy, R.string.foreignlibrariesnotinthelist_mail).map { getString(it) }
                val itemsSubCaption = intArrayOf(R.string.foreignlibrariesnotinthelist, R.string.foreignlibrariesnotinthelist_easy_req, R.string.foreignlibrariesnotinthelist_meta_req, R.string.foreignlibrariesnotinthelist_install_req, R.string.foreignlibrariesnotinthelist_diy_req, R.string.foreignlibrariesnotinthelist_mail_req).map { getString(it) }

                dialogBinding.listView.adapter = object : ArrayAdapter<String>(activity, R.layout.book_overview_row, R.id.caption, itemsSubCaption) {
                    override fun getView(position: Int, convertView: View?, parent: ViewGroup): View {
                        return super.getView(position, convertView, parent).also {
                            val binding = BookOverviewRowBinding.bind(it)
                            binding.date.visibility = View.GONE
                            binding.more.text = itemsSubCaption.getOrElse(position) { "" }
                            binding.caption.apply {
                                text = items.getOrElse(position - 1) { "" }
                                isVisibleNotGone = position > 0
                            }
                        }
                    }
                }
                dialogBinding.listView.onItemClickListener = AdapterView.OnItemClickListener { _, _, i, _ ->
                    withActivity<LibraryList> {
                        when (i) {
                            1 -> startActivityForResult<NewLibrary>(RETURNED_FROM_NEW_LIBRARY)
                            2 -> if (adapter.metaCatalogs >= 0) {
                                val id = adapter.childId(adapter.bitsPerLevel, 0, adapter.metaCatalogs)
                                listView?.expandableAdapter?.let { ea ->
                                    if (ea.isExpanded(id)) {
                                        val pos = ea.idToLinearPosition(id)
                                        val d = ea.countDescendants(id)
                                        listView?.smoothScrollToPosition(pos + d)
                                    }
                                }
                                listView?.expand(id)
                                columnView?.expand(id)
                            }
                            3 -> LibraryUpdateLoader.askForUpdate()
                            4 -> startActivity<SourceEdit>() //showUriInBrowser("http://www.videlibri.de/help/neuebibliothek.html")
                            5 -> startActivity<Feedback>()
                        }
                    }
                    dismiss()
                }
                builder.setView(dialogBinding.root)
            }
        }
    }

    companion object {
        @JvmField internal var lastSelectedLibId: String? = null
        @JvmField internal var lastSelectedLibName: String? = null       //result of the activity
        internal var lastSelectedTime: Long = 0       //(passing as intent did not work on every device (perhaps the caller is killed?)
        private const val SELECTION_REUSE_TIME = (10 * 1000).toLong()

        fun lastSelectedFallbackLibraryId(): String? =
                if (System.currentTimeMillis() - lastSelectedTime < SELECTION_REUSE_TIME)
                    lastSelectedLibId
                else
                    null
    }

}