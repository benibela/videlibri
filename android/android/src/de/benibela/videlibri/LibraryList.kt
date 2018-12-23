package de.benibela.videlibri

import android.content.Context
import android.content.Intent
import android.graphics.Rect
import android.graphics.drawable.Drawable
import android.graphics.drawable.StateListDrawable
import android.os.Bundle
import android.util.TypedValue
import android.view.*
import android.view.View.OnClickListener
import android.widget.*
import de.benibela.videlibri.jni.Bridge
import java.util.*

class LibraryList: VideLibriBaseActivity() {
    internal val states: MutableList<String> = ArrayList()
    internal val cities: MutableList<MutableList<String>> = ArrayList()
    internal val localLibs: MutableList<MutableList<MutableList<MutableMap<String, String>>>> = ArrayList()

    internal lateinit var scrollView: ScrollView
    private var libView: LibraryListView? = null
    private var metaCat: Int = 0

    override fun onCreateOptionsMenuOverflow(menu: Menu, inflater: MenuInflater) {
        super.onCreateOptionsMenuOverflow(menu, inflater)
        inflater.inflate(R.menu.librarylistmenu, menu)
    }

    private fun makeLibView(cityView: ScrollView? = null, libView: ScrollView? = null): LibraryListView {
        metaCat = -1
        val libs = Bridge.VLGetLibraryIds()

        states.clear()
        cities.clear()
        localLibs.clear()

        var autoExpand = 0
        if (Accounts.size > 0) {
            autoExpand = 1
            val used = ArrayList<String>()
            states.add(tr(R.string.liblist_withaccounts))
            cities.add(ArrayList())
            cities[0].add(tr(R.string.liblist_withaccounts))
            localLibs.add(ArrayList())
            localLibs[0].add(ArrayList())
            for (account in Accounts.toArray) {
                if (used.contains(account.libId)) continue
                used.add(account.libId)

                val map = TreeMap<String, String>()
                localLibs[0][localLibs.size - 1].add(map)

                map["ID"] = account.libId
            }
        }

        var lastIdSplit = listOf("", "", "", "")
        for (id in libs) {
            val split = id.split("_".toRegex())
            if (states.isEmpty() || split[0] != lastIdSplit[0] || split[1] != lastIdSplit[1]) {
                if (metaCat < 0 && id.contains("ueberregional")) metaCat = states.size
                states.add(Bridge.LibraryDetails.decodeIdEscapes(split[0] + " - " + split[1]))
                cities.add(ArrayList())
                localLibs.add(ArrayList())
            }
            val curCities = cities[cities.size - 1]
            if (curCities.isEmpty() || split[2] != lastIdSplit[2]) {
                curCities.add(Bridge.LibraryDetails.decodeIdEscapes(split[2]))
                if ("-" == split[2] && autoExpand < 2) autoExpand += 1
                localLibs[localLibs.size - 1].add(ArrayList())
            }
            val map = TreeMap<String, String>()
            localLibs[localLibs.size - 1][localLibs[localLibs.size - 1].size - 1].add(map)
            map["ID"] = id
            lastIdSplit = split
        }

        val lv = LibraryListView(this, cityView, libView)

        for (i in 0 until autoExpand) {
            lv.expand(i, false)
            if (cities[i].size == 1) lv.expand(i, 0, false)
            if (cityView != null) break
        }

        return lv
    }

    private fun onLeafClick(state: Int, city: Int, lib: Int) {
        val leaf = localLibs[state][city][lib]
        lastSelectedLibId = leaf["ID"]
        lastSelectedLibName = leaf["NAME"]
        lastSelectedTime = System.currentTimeMillis()

        finishWithResult()
    }

    public override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setVideLibriView(R.layout.chooselib)

        intent.getStringExtra("reason")?.takeNonEmpty()?.let { findViewById<TextView>(R.id.textView) }

        createListView()

        findViewById<View>(R.id.textViewLibWhyNot).apply {
            if (//false &&
                    accounts.isNotEmpty())
                visibility = View.GONE
            else
                setOnClickListener { showHowToAddLibraryDialog() }
        }

    }

    override fun onSaveInstanceState(outState: Bundle?) {
        super.onSaveInstanceState(outState)
        outState ?: return
        val libView = libView ?: return
        outState.putString("lastExpandedState", libView.lastExpandedState)
        outState.putString("lastExpandedCity", libView.lastExpandedCity)
    }

    override fun onRestoreInstanceState(savedInstanceState: Bundle?) {
        super.onRestoreInstanceState(savedInstanceState)
        savedInstanceState ?: return
        val libView = libView ?: return
        val lastExpandedState = savedInstanceState.getString("lastExpandedState")
        val lastExpandedCity = savedInstanceState.getString("lastExpandedCity")
        val state = states.indexOf(lastExpandedState)
        val city = cities.getOrNull(state)?.indexOf(lastExpandedCity) ?: return
        if (city < 0) libView.expand(state, true)
        else libView.expand(state, city, true)
    }

    private fun createListView() {
        val portMode = resources.getBoolean(R.bool.port_mode)
        scrollView = findViewById(R.id.libListView)
        scrollView.let {
            it.removeAllViews()
            libView = if (portMode) makeLibView()
                      else makeLibView(findViewById(R.id.libListViewCities), findViewById(R.id.libListViewLibs))
            it.addView(libView)
        }
    }

    internal class ViewIdState( var a: Int )
    internal class ViewIdCity(var a: Int, var b: Int)
    internal class ViewIdLibrary(var a: Int, var b: Int, var c: Int)


    inner class LibraryListView internal constructor(
            context: Context,
            private var cityView: ScrollView?,
            private var libView: ScrollView?) : LinearLayout(context) {
        private var stateViews: List<TextView>
        private var stateChildViews: List<LinearLayout>
        private var cityViews: List<Array<TextView?>>
        private var cityChildViews: List<Array<LinearLayout?>>
        private var portMode: Boolean = false
        //View libViews[][][];

        internal var lastExpandedState: String? = null
        internal var lastExpandedCity: String? = null

        private var groupIndicator: Drawable? = null
        private var groupIndicatorExpanded: Drawable? = null

        private var combinedListener: View.OnClickListener = OnClickListener { view ->
            view.tag.let {
                when (it) {
                    is ViewIdState -> if (!isExpanded(it.a)) openState(it.a, portMode) else collapse(it.a)
                    is ViewIdCity -> if (!isExpanded(it.a, it.b)) expand(it.a, it.b, true) else collapse(it.a, it.b)
                    is ViewIdLibrary -> onLeafClick(it.a, it.b, it.c)
                }
            }
        }

        init {
            portMode = cityView == null
            orientation = LinearLayout.VERTICAL
            if (portMode) {
                val typedValue = TypedValue()
                if (theme.resolveAttribute(android.R.attr.expandableListViewStyle, typedValue, true)) {
                    val typedArray = theme.obtainStyledAttributes(typedValue.resourceId, intArrayOf(android.R.attr.groupIndicator))
                    (typedArray.getDrawable(0) as? StateListDrawable)?.let {
                        groupIndicator = it
                    }
                    (typedArray.getDrawable(0) as? StateListDrawable)?.let {
                        it.state = intArrayOf(android.R.attr.state_expanded)
                        groupIndicatorExpanded = it.current
                    }
                }
            }

            //create all state views
            stateViews = states.mapIndexed { i, s ->
                (layoutInflater.inflate(android.R.layout.simple_expandable_list_item_1, this, false) as TextView).apply {
                    setBackgroundResource(android.R.drawable.list_selector_background)
                    text = s
                    setCompoundDrawablesWithIntrinsicBounds(groupIndicator, null, null, null)
                    tag = ViewIdState(i)
                    setOnClickListener(combinedListener)
                }
            }
            stateChildViews = states.map {
                LinearLayout(context).apply {
                    layoutParams = LayoutParams(LinearLayout.LayoutParams.MATCH_PARENT, LinearLayout.LayoutParams.WRAP_CONTENT)
                    orientation = VERTICAL
                }
            }
            for (i in states.indices) {
                addView(stateViews[i])
                if (portMode)
                    addView(stateChildViews[i])
            }


            //create null arrays for city views
            cityViews = states.mapIndexed { i, _ -> arrayOfNulls<TextView>(cities[i].size) }
            cityChildViews = states.mapIndexed { i, _ -> arrayOfNulls<LinearLayout>(cities[i].size) }
        }

        private fun isExpanded(state: Int): Boolean {
            return stateChildViews[state].childCount > 0 && stateChildViews[state].isShown
        }

        private fun isExpanded(state: Int, city: Int): Boolean {
            return cityChildViews[state][city]?.run { childCount > 0 && isShown } ?: false
        }


        internal fun expand(state: Int, scroll: Boolean) {
            if (stateChildViews[state].childCount == 0) {
                for (b in cities[state].indices) {
                    (layoutInflater.inflate(R.layout.librarycityinlistview, this, false) as TextView).apply {
                        text = cities[state][b]
                        setCompoundDrawablesWithIntrinsicBounds(groupIndicator, null, null, null)
                        //l.setPadding((int)(60 * getResources().getDisplayMetrics().density), l.getPaddingTop(), l.getPaddingRight(), l.getPaddingBottom());
                        tag = ViewIdCity(state, b)
                        setOnClickListener(combinedListener)
                        stateChildViews[state].addView(this)
                        cityViews[state][b] = this
                    }

                    LinearLayout(context).apply {
                        layoutParams = LayoutParams(LinearLayout.LayoutParams.MATCH_PARENT, LinearLayout.LayoutParams.WRAP_CONTENT)
                        orientation = VERTICAL
                        cityChildViews[state][b] = this
                        if (portMode)
                            stateChildViews[state].addView(this)
                    }
                }
            }
            if (!portMode) cityView?.apply {
                removeAllViews()
                addView(stateChildViews[state])
            }
            stateChildViews[state].visibility = View.VISIBLE

            if (portMode)
                setIndicator(stateViews[state], true)

            if (scroll)
                smoothScrollTo(
                        if (portMode)
                            if (cityChildViews[state].isNotEmpty() && isExpanded(state, cityChildViews[state].size - 1))
                                cityChildViews[state][cityChildViews[state].size - 1]?:return
                            else
                                stateChildViews[state]
                        else
                            stateViews[state],
                        stateViews[state])

            if (state < states.size) {
                lastExpandedState = states[state]
                lastExpandedCity = null
            }
        }

        internal fun expand(state: Int, city: Int, scroll: Boolean) {
            cityChildViews[state][city] ?: expand(state, scroll)
            cityChildViews[state][city]?.let { libraryHolder ->
                if (libraryHolder.childCount == 0) {
                    localLibs[state][city].forEachIndexed { index, libraryMap ->
                        if (!libraryMap.containsKey("NAME")) {
                            val id = libraryMap["ID"] ?: return@forEachIndexed
                            val details = Bridge.VLGetLibraryDetails(id)
                            libraryMap["NAME"] = details?.prettyName ?: "Failed to load: $id"
                            details?.tableComment?.takeNonEmpty()?.let { libraryMap["TABLECOMMENT"] = it }
                        }
                        libraryHolder.addView((layoutInflater.inflate(R.layout.libraryinlistview, this, false) as TextView).apply {
                            text = libraryMap["NAME"]
                            tag = ViewIdLibrary(state, city, index)
                            setOnClickListener(combinedListener)
                        })
                        libraryMap["TABLECOMMENT"]?.takeNonEmpty()?.let {
                            libraryHolder.addView((layoutInflater.inflate(R.layout.simpletextview, this, false) as TextView).apply {
                                text = it
                                setPadding(30, 0, 10, 10)
                            })
                        }
                    }
                }
                if (!portMode) libView?.apply{
                    removeAllViews()
                    addView(libraryHolder)
                }
                libraryHolder.visibility = View.VISIBLE
                if (portMode) cityViews[state][city]?.let{
                    setIndicator(it, true)
                    smoothScrollTo(libraryHolder, it)
                }
                if (state < Math.min(states.size, cities.size) && city < cities[state].size) {
                    lastExpandedState = states[state]
                    lastExpandedCity = cities[state][city]
                }
            }
        }

        private fun collapse(state: Int) {
            if (portMode) {
                stateChildViews[state].visibility = View.GONE
                setIndicator(stateViews[state], false)
            }
        }

        private fun collapse(state: Int, city: Int) {
            if (portMode) {
                cityChildViews[state][city]?.visibility = View.GONE
                cityViews[state][city]?.let { setIndicator(it, false) }
            }
        }

        private fun setIndicator(view: TextView, expanded: Boolean) {
            playSoundEffect(SoundEffectConstants.CLICK)
            if (groupIndicator != null || groupIndicatorExpanded != null)
                view.postDelayed({ view.setCompoundDrawablesWithIntrinsicBounds(if (expanded) groupIndicatorExpanded else groupIndicator, null, null, null) }, 150)

            /*TransitionDrawable trans = new TransitionDrawable(new ColorDrawable[]{new ColorDrawable(Color.BLACK), new ColorDrawable(Color.BLUE), new ColorDrawable(Color.BLACK)});
            view.setBackgroundDrawable(trans);
            trans.startTransition(500);                             */
        }

        internal fun openState(i: Int, scroll: Boolean) {
            expand(i, scroll)
            if (cities[i].size == 1 && !isExpanded(i, 0)) expand(i, 0, portMode)
        }

        private fun offsetParent(r: Rect, view: View) {
            if (view.parent !== this@LibraryListView && view.parent is View) {
                val temp = Rect()
                (view.parent as View).getHitRect(temp)
                r.top += temp.top
                r.bottom += temp.top
            }
        }

        private fun smoothScrollTo(view: View, header: View) {
            view.postDelayed(Runnable {
                val viewRect = Rect()
                val scrollRect = Rect()
                view.getHitRect(viewRect)
                offsetParent(viewRect, view)
                scrollView.getDrawingRect(scrollRect)

                //Log.i("VL", "center: "+center+ " temp "+temp.top+" "+temp.bottom);

                if (viewRect.bottom >= scrollRect.top && viewRect.bottom <= scrollRect.bottom) return@Runnable
                val headerRect = Rect()
                header.getHitRect(headerRect)
                offsetParent(headerRect, header)

                val target = if (headerRect.height() + viewRect.height() + 20 /* arbitrary offset */ >= scrollRect.height())
                    headerRect.top
                else
                    viewRect.bottom - scrollRect.height()

                scrollView.smoothScrollTo(measuredWidth / 2, target)
            }, 150) //wait for post onMeasure
        }

    }


    override fun onActivityResult(requestCode: Int, resultCode: Int, data: Intent?) {
        when (requestCode) {
            VideLibriBaseActivityOld.RETURNED_FROM_NEW_LIBRARY -> createListView()
        }
        super.onActivityResult(requestCode, resultCode, data)
    }


    private fun showHowToAddLibraryDialog(){
        showDialog {
            onCreate =  onCreate@ {  builder ->
                val activity = activity ?: return@onCreate
                val inflater = activity.layoutInflater
                val v = inflater.inflate(R.layout.dialogbooklistlike, null)

                val items = intArrayOf(R.string.foreignlibrariesnotinthelist_easy, R.string.foreignlibrariesnotinthelist_meta, R.string.foreignlibrariesnotinthelist_install, R.string.foreignlibrariesnotinthelist_diy, R.string.foreignlibrariesnotinthelist_mail).map { getString(it) }
                val itemsSubCaption = intArrayOf(R.string.foreignlibrariesnotinthelist, R.string.foreignlibrariesnotinthelist_easy_req, R.string.foreignlibrariesnotinthelist_meta_req, R.string.foreignlibrariesnotinthelist_install_req, R.string.foreignlibrariesnotinthelist_diy_req, R.string.foreignlibrariesnotinthelist_mail_req).map { getString(it) }

                val lv = v.findViewById<ListView>(R.id.listView)
                lv.adapter = object : ArrayAdapter<String>(activity, R.layout.bookoverview, R.id.bookoverviewCaption, itemsSubCaption) {
                    override fun getView(position: Int, convertView: View?, parent: ViewGroup?): View? {
                        return super.getView(position, convertView, parent)?.apply {
                            findViewById<View>(R.id.bookoverviewDate).visibility = View.GONE
                            findViewById<TextView>(R.id.bookoverviewMore).text = itemsSubCaption.getOrElse(position, {""})
                            findViewById<TextView>(R.id.bookoverviewCaption).apply {
                                text = items.getOrElse(position - 1, {""})
                                isVisibleNotGone = position > 0
                            }
                        }
                    }
                }
                lv.onItemClickListener = AdapterView.OnItemClickListener { _, _, i, _ ->
                    withActivity<LibraryList> {
                        when (i) {
                            1, 3 -> startActivity<NewLibrary>(
                                    "mode" to if (i == 3) 0 else NewLibrary.MODE_LIBRARY_ENTER_NEW_DATA
                            )
                            2 -> if (metaCat >= 0) libView?.openState(metaCat, true)
                            4 -> showUriInBrowser("http://www.videlibri.de/help/neuebibliothek.html")
                            5 -> startActivity<Feedback> ()
                        }
                    }
                    dismiss()
                }
                builder.setView(v)
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