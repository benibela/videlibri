package de.benibela.videlibri

import android.Manifest
import android.app.ActivityManager
import android.app.AlertDialog
import android.app.Dialog
import android.content.DialogInterface
import android.content.pm.PackageManager
import android.graphics.BitmapFactory
import android.os.Build
import android.os.Bundle
import android.os.Handler
import android.preference.PreferenceManager
import androidx.core.app.ActivityCompat
import androidx.core.content.ContextCompat
import android.text.Editable
import android.view.*
import android.widget.Button
import android.widget.CompoundButton
import android.widget.EditText
import android.widget.LinearLayout
import androidx.fragment.app.DialogFragment
import de.benibela.videlibri.jni.Bridge
import org.json.JSONArray
import org.json.JSONException
import java.util.*


class LendingList: BookListActivity(){

    private var searchPanel: View? = null

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        createDrawerToggle()

        searchPanel = findViewById<View>(R.id.searchFilterPanel)

        savedInstanceState?.apply{
            filterActually = getString("filterActually") ?: ""
            setFilterMultiLine(getBoolean("filterIsMultiLine", false))
        }
        updateViewFilters()


        findViewById<EditText>(R.id.searchFilter).let { et ->
            registerForContextMenu(et)
            et.addTextChangedListener(object : EmptyTextWatcher() {
                override fun afterTextChanged(editable: Editable) {
                    setFilter(editable.toString())
                }
            })
        }

        if (accounts.isNotEmpty()) {
            displayAccounts()
            VideLibriApp.updateAccount(null, true, false)
        }


        endLoadingAll(LOADING_COVER_IMAGE)
    }

    internal fun setFilter(s: String) {
        val oldFilterActually = filterActually
        filterActually = s
        if (alwaysFilterOnHistory && oldFilterActually.isEmpty() != filterActually.isEmpty()) {
            displayAccounts()
        } else
            refreshBookCache()
    }


    override fun onResume() {
        super.onResume()

        updateAccountView()

        //setTitle("Ausleihen");  //does not work in onCreate (why? makes the title invisible) No. it just works sometimes?

        if (accounts.filterWithRunningUpdate().isNotEmpty()) beginLoading(LOADING_ACCOUNT_UPDATE)
        if (!cacheShown)
            displayBookCache()

        if (ContextCompat.checkSelfPermission(this, Manifest.permission.INTERNET) != PackageManager.PERMISSION_GRANTED)
        //unnecessary, because not dangerous??
            ActivityCompat.requestPermissions(this, arrayOf(Manifest.permission.INTERNET), 0)
    }

    override fun onPostResume() {
        super.onPostResume()

        if (accounts.isEmpty())
            Handler().postDelayed({
                if (VideLibriApp.currentActivity is LendingList) //do not open the list, if the user navigated away
                    startActivity<AccountInfo>("mode" to AccountInfo.MODE_ACCOUNT_CREATION_INITIAL)
            }, 200) //without delay it has crashed on old android (android 3 or something) in onresume
    }


    override fun onSaveInstanceState(outState: Bundle) {
        super.onSaveInstanceState(outState)
        outState.apply {
            putString("filterActually", filterActually)
            putBoolean("filterIsMultiLine", filterIsMultiLine)
        }
    }


    private fun updateViewFilters() {
        val sp = PreferenceManager.getDefaultSharedPreferences(this)
        displayOptions.readFromPreferences(sp)
        searchPanel?.isVisibleNotGone = !("__disabled" == displayOptions.filterKey)
        displayHistory = sp.getBoolean("displayHistory", false)
        alwaysFilterOnHistory = sp.getBoolean("alwaysFilterOnHistory", true)
    }

    private fun updateAccountView() {
        updateViewFilters()
        if (displayHistoryActually != (displayHistory || alwaysFilterOnHistory && filterActually.isNotEmpty())
                || displayOptions != displayOptionsActually
                || displayForcedCounterActually != displayForcedCounter
        ) {
            displayAccounts()
        }
    }


    var primaryBookCache = ArrayList<Bridge.Book>()
    fun displayAccounts() {
        displayHistoryActually = displayHistory || alwaysFilterOnHistory && filterActually.isNotEmpty()
        displayOptionsActually = displayOptions.copy()
        displayForcedCounterActually = displayForcedCounter

        primaryBookCache = makePrimaryBookCache(displayHistoryActually, false)
        refreshBookCache()

        if (VideLibriApp.mainIcon != currentMainIcon) {
            checkMainIcon()
            if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.LOLLIPOP) {
                setTaskDescription(ActivityManager.TaskDescription(null, BitmapFactory.decodeResource(resources, VideLibriApp.mainIcon, null)))
            }
        }
    }


    fun refreshBookCache() {
        var xquery = (filterActually.startsWith("xquery version") || filterActually.startsWith("for $") || filterActually.contains("\$book"))
        if (!xquery) {
            var controlchars = 0
            for (c in filterActually)
                when (c) {
                    '!', '$', '(', ')', '*', '+', '"', ',', '/', '<', '=', '>', '[', ']' -> controlchars++
                }
            xquery = controlchars >= 4
        }
        if (!xquery)
            bookCache = displayOptions.let { filterToSecondaryBookCache(primaryBookCache, it.groupingKey, it.sortingKey, filterActually, it.filterKey) }
        else {
            bookCache = ArrayList<Bridge.Book>()
            bookCache.addAll(Bridge.VLXQuery(filterActually))
        }

        displayBookCache()

        var bookCountPrimary = 0 //number of shown books (after filtering), number of books (before filtering)
        var bookCountPrimaryNoHistory = 0
        if (displayHistoryActually) {
            for (b in primaryBookCache)
                if (b.account != null) {
                    bookCountPrimary++
                    if (!b.history) bookCountPrimaryNoHistory++
                }
        } else {
            bookCountPrimary += primaryBookCache.count { it.account != null }
        }

        val bookCount =
            if (primaryBookCache.size == bookCache.size)
                bookCountPrimary
            else
                bookCache.count { it.account != null }

        var title: String =
            if (displayHistoryActually) {
                if (bookCountPrimary == bookCount)
                    tr(R.string.main_bookcounthistoryDD, bookCountPrimaryNoHistory, bookCountPrimary)
                else
                    tr(R.string.main_bookcounthistoryDDD, bookCount, bookCountPrimaryNoHistory, bookCountPrimary)
            } else {
                if (bookCountPrimary != bookCount)
                    tr(R.string.main_bookcountDD, bookCount, bookCountPrimary)
                else
                    resources.getQuantityString(R.plurals.main_bookcountPluralD, bookCount, bookCount)
            }
        val hiddenCount = accounts.filterHidden().size
        if (hiddenCount > 0) {
            val shownAccounts = accounts.size - hiddenCount
            if (shownAccounts == 1) {
                for (account in accounts)
                    if (account.isShown) {
                        title += ", " + account.prettyName
                        break
                    }
            } else {
                title += ", " + tr(R.string.main_accountcountDD, shownAccounts, accounts.size)
            }
        }
        setTitle(title)
    }


    override fun onCreateOptionsMenuPrimary(menu: Menu, inflater: MenuInflater) {
        super.onCreateOptionsMenuPrimary(menu, inflater)
        inflater.inflate(R.menu.filtermenu, menu)
    }

    override fun onCreateOptionsMenuOverflow(menu: Menu, inflater: MenuInflater) {
        inflater.inflate(R.menu.lendinglistmenu, menu)
        super.onCreateOptionsMenuOverflow(menu, inflater)
    }

    override fun setOptionMenuVisibility(menu: Menu?) {
        super.setOptionMenuVisibility(menu)
        val hasAccounts = accounts.isNotEmpty()

        menu?.forItems {
            it.isVisible = when (it.itemId) {
                R.id.refresh -> accounts.filterWithRunningUpdate().isEmpty()
                R.id.filter, R.id.renew, R.id.renewlist -> hasAccounts
                R.id.research_menu -> hasAccounts && detailsVisible()
                R.id.delete -> hasAccounts && detailsVisible() && details.book?.history == true
                else -> return@forItems
            }
        }
    }

    override fun onOptionsItemIdSelected(id: Int): Boolean {
        when (id) {
            R.id.account_information -> {
                accounts.refreshAccounts()
                val info = if (accounts.isEmpty()) getString(R.string.main_no_accounts)
                           else accounts.joinToString (
                              separator = "\n\n",
                              transform = { "${it.prettyName}:\n${
                                    if (it.isReal)
                                        getString(R.string.main_last_refreshS, BookFormatter.formatDateFull(it.lastCheckDate)) +
                                        if (it.expiration.isNotEmpty()) "\n" + getString(R.string.main_account_expirationS, it.expiration) else ""
                                    else getString(R.string.main_account_search_only)}" }
                           )
                showMessage(message = info, title = getString(R.string.menu_account_information))
            }
            R.id.research_same, R.id.research_author, R.id.research_title -> {
                currentBook()?.let {
                    it.account?.let { account ->
                        val query = mapOf(
                                "title" to if (id == R.id.research_same || id == R.id.research_title) it.title else "",
                                "author" to if (id == R.id.research_same || id == R.id.research_author) it.author else "")
                        startActivity<Search>(
                                "libId" to account.libId,
                                "query" to query)
                    }
                }
            }
            R.id.research_filter_author -> currentBook()?.let {
                val filter = "\$books[author = \"${it.author.replace("\"", "\"\"")}\"]"
                findViewById<EditText>(R.id.searchFilter).setText(filter)
                showList()
            }
            R.id.filter -> {
                ViewOptionsDialog().show(supportFragmentManager, null)
            }
            R.id.delete -> {
                val book = details.book ?: return false
                showMessageYesNo(getString(R.string.delete_book_confirmS, book.title)) {
                    Bridge.VLChangeBook(book, null)
                    showToast(R.string.delete_book_confirmed)
                    refreshDisplayedLendBooks()
                }
            }
            else -> return super.onOptionsItemIdSelected(id)
        }
        return true
    }

    var displayOptionsActually = BookListDisplayOptions()

    var alwaysFilterOnHistory = true
    private var displayHistoryActually = false
    private var filterActually: String = ""
    private var filterIsMultiLine: Boolean = false

    override fun onBookActionButtonClicked(book: Bridge.Book) {
        when (book.status) {
            Bridge.Book.StatusEnum.Normal -> showDialog{
                message(R.string.renew_single_confirm)
                negativeButton(R.string.cancel)
                positiveButton(R.string.renew) {
                    VideLibriApp.renewBooks(arrayOf(book))
                    withActivity<LendingList> { showList() }
                }
            }
            Bridge.Book.StatusEnum.Ordered, Bridge.Book.StatusEnum.Provided -> showMessageYesNo(R.string.main_cancelconfirm){
                Bridge.VLBookOperation(arrayOf(book), Bridge.BOOK_OPERATION_CANCEL) //cancel
                withActivity<LendingList> {
                    beginLoading(LOADING_ACCOUNT_UPDATE)
                    showList()
                }
            }
            else -> return
        }
    }

    internal var displayForcedCounterActually: Int = 0


    class ViewOptionsDialog : DialogFragment(), DialogInterface.OnCancelListener {
        internal lateinit var view: View
        override fun onCreateDialog(savedInstanceState: Bundle?): Dialog {
            val builder = AlertDialog.Builder(activity)
            builder.setOnCancelListener(this)
            activity?.let { activity ->
                val inflater = activity.layoutInflater
                view = inflater.inflate(R.layout.options_lendings, null)
                Options.showLendingOptionsInView(activity, view)


                val linearLayout = view.findViewById<LinearLayout>(R.id.viewaccounts)
                linearLayout.removeAllViews()
                val switchboxes = ArrayList<CompoundButton>()
                val accountCheckListener = View.OnClickListener { v ->
                    val acc = v.tag as Bridge.Account?
                    val b = (v as? CompoundButton)?.isChecked ?: return@OnClickListener
                    if (acc == null) {
                        if (b) accounts.showAll()
                        else accounts.hideAll()
                        for (cb in switchboxes) cb.isChecked = b
                    } else acc.isHidden = !b
                }
                val accountClickListener = object : View.OnClickListener {
                    override fun onClick(v: View) {
                        val acc = v.tag as Bridge.Account?
                        if (acc == null) accounts.showAll()
                        else acc.showAlone()
                        val dialog = this@ViewOptionsDialog.dialog ?: return
                        dialog.findViewById<CompoundButton>(R.id.viewHistory).isChecked = v.id == R.id.buttonforhistory
                        dialog.cancel()
                    }
                }

                for (i in -1 until accounts.size) {
                    val acc = if (i == -1) null else accounts[i]
                    val group = inflater.inflate(R.layout.options_lendings_accountrow, null)
                    val sb: CompoundButton = group.findViewById<CompoundButton>(R.id.switchbox).apply {
                        isChecked = if (acc == null) accounts.filterHidden().size <= accounts.size / 2 else !acc.isHidden
                        tag = acc
                        isSaveEnabled = false
                        setOnClickListener(accountCheckListener)
                    }
                    if (acc != null) switchboxes.add(sb)
                    group.findViewById<Button>(R.id.button).apply {
                        text = acc?.prettyName ?: getText(R.string.main_allaccounts)
                        tag = acc
                        setOnClickListener(accountClickListener)
                    }
                    group.findViewById<Button>(R.id.buttonforhistory).apply {
                        tag = acc
                        setOnClickListener(accountClickListener)
                    }
                    linearLayout.addView(group)
                }

                builder.setView(view)
            }
            return builder.create().apply {
                window?.setGravity(Gravity.END or Gravity.TOP)
            }
        }

        override fun onCancel(dialog: DialogInterface) {
            super.onCancel(dialog)
            val activity = activity ?: return
            Options.putLendingOptionsFromView(activity, view)
            if (activity is LendingList)
                activity.updateAccountView()
        }
    }

    override fun onCreateContextMenu(menu: ContextMenu?, v: View?, menuInfo: ContextMenu.ContextMenuInfo?) {
        if (v?.id == R.id.searchFilter) {
            menuInflater.inflate(R.menu.searchfiltercontextmenu, menu)
            menu?.findItem(R.id.toggle_singleline)?.setTitle(if (filterIsMultiLine) R.string.menu_context_filter_singleline else R.string.menu_context_filter_multiline)
            contextMenuSelectedItem = filterActually
        } else
            super.onCreateContextMenu(menu, v, menuInfo)
    }

    private fun getFilterHistory(): ArrayList<String> {
        val filters = ArrayList<String>()
        try {
            val filtersJson = JSONArray(PreferenceManager.getDefaultSharedPreferences(this).getString("filterHistory", tr(R.string.config_example_filters_json)))
            for (i in 0 until filtersJson.length())
                filters.add(filtersJson.getString(i))
        } catch (ignored: JSONException) {
        }

        return filters
    }

    override fun onContextItemSelected(item: MenuItem): Boolean {
        when (item.itemId) {
            R.id.load_filter -> {
                showChooseDialog(R.string.menu_context_load_filter, getFilterHistory()) {
                    currentActivity<LendingList>()?.findViewById<EditText>(R.id.searchFilter)?.setText(getFilterHistory()[it])
                }
            }
            R.id.save_filter -> {
                val filters = getFilterHistory()
                if (filters.contains(filterActually)) filters.remove(filterActually)
                filters.add(0, filterActually)
                val filtersJson = JSONArray()
                for (s in filters) filtersJson.put(s)
                PreferenceManager.getDefaultSharedPreferences(this).edit().let {
                    it.putString("filterHistory", filtersJson.toString())
                    it.apply()
                }
            }
            R.id.clear -> {
                findViewById<EditText>(R.id.searchFilter).setText("")
            }
            R.id.paste, R.id.pastereplace -> {
                Util.Clipboard.getText(this)?.let {
                    findViewById<EditText>(R.id.searchFilter).setText(
                            (if (item.itemId == R.id.pastereplace) "" else filterActually) + it
                    )
                }
            }
            R.id.toggle_singleline -> {
                setFilterMultiLine(!filterIsMultiLine)
            }
            else -> return super.onContextItemSelected(item)
        }
        return true

    }

    override fun onBackPressed() {
        if (listVisible() && filterActually != "") findViewById<EditText>(R.id.searchFilter).setText("")
        else super.onBackPressed()
    }

    internal fun setFilterMultiLine(ml: Boolean) {
        if (ml == filterIsMultiLine) return
        filterIsMultiLine = ml
        findViewById<EditText>(R.id.searchFilter).apply {
            if (filterIsMultiLine) {
                setSingleLine(false)
                maxLines = 10
            } else {
                maxLines = 1
                setSingleLine(true)
            }
        }
    }


    companion object {
        internal var displayForcedCounter = 1
        @JvmField var displayHistory = false
        @JvmStatic fun refreshDisplayedLendBooks() {
            displayForcedCounter += 1
            withActivity<LendingList> { displayAccounts() }
        }

    }

}