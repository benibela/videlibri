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
import android.preference.PreferenceManager
import android.support.v4.app.ActivityCompat
import android.support.v4.content.ContextCompat
import android.text.Editable
import android.text.TextWatcher
import android.view.*
import android.widget.Button
import android.widget.CompoundButton
import android.widget.EditText
import android.widget.LinearLayout
import de.benibela.videlibri.jni.Bridge
import org.json.JSONArray
import org.json.JSONException
import java.util.*


class LendingList: BookListActivity(){

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        createDrawerToggle()

        // Log.i("VideLibri", "onCreate")               ;

        if (savedInstanceState != null) {
            filterActually = savedInstanceState.getString("filterActually")
            setFilterMultiLine(savedInstanceState.getBoolean("filterIsMultiLine", false))
        }
        updateViewFilters()


        findViewById<View>(R.id.searchFilterPanel).visibility = View.VISIBLE
        val et = findViewById<EditText>(R.id.searchFilter)
        registerForContextMenu(et)
        et.addTextChangedListener(object : TextWatcher {
            override fun beforeTextChanged(charSequence: CharSequence, i: Int, i2: Int, i3: Int) {}

            override fun onTextChanged(charSequence: CharSequence, i: Int, i2: Int, i3: Int) {}

            override fun afterTextChanged(editable: Editable) {
                setFilter(editable.toString())
            }
        })


        if (VideLibriApp.accounts == null || VideLibriApp.accounts.size == 0)
        else {
            displayAccounts()
            VideLibriApp.updateAccount(null, true, false)
        }//startActivity(new Intent(this, NewAccountWizard.class));


        endLoadingAll(VideLibriBaseActivityOld.LOADING_COVER_IMAGE)
    }

    internal fun setFilter(s: String) {
        val oldFilterActually = filterActually
        filterActually = s
        if (alwaysFilterOnHistory && Util.isEmptyString(oldFilterActually) != Util.isEmptyString(filterActually)) {
            displayAccounts()
        } else
            refreshBookCache()
    }


    override fun onResume() {
        super.onResume()

        updateAccountView()

        //setTitle("Ausleihen");  //does not work in onCreate (why? makes the title invisible) No. it just works sometimes?

        if (VideLibriApp.accounts == null || VideLibriApp.accounts.size == 0) {
            var v: View? = findViewById(R.id.layout) //need an arbitrary view. Depends on landscape/portrait, which is there
            if (v == null) v = findViewById(R.id.booklistview)

            v?.postDelayed({
                startActivity<AccountInfo>("mode" to AccountInfo.MODE_ACCOUNT_CREATION_INITIAL)
            }, 400)
        }

        if (!VideLibriApp.runningUpdates.isEmpty()) beginLoading(VideLibriBaseActivityOld.LOADING_ACCOUNT_UPDATE)
        if (!cacheShown)
            displayBookCache()

        if (ContextCompat.checkSelfPermission(this, Manifest.permission.INTERNET) != PackageManager.PERMISSION_GRANTED)
        //unnecessary, because not dangerous??
            ActivityCompat.requestPermissions(this, arrayOf(Manifest.permission.INTERNET), 0)
    }


    override fun onSaveInstanceState(outState: Bundle?) {
        super.onSaveInstanceState(outState)
        outState?.apply {
            putString("filterActually", filterActually)
            putBoolean("filterIsMultiLine", filterIsMultiLine)
        }
    }


    private fun updateViewFilters() {
        val sp = PreferenceManager.getDefaultSharedPreferences(this)
        displayOptions.readFromPreferences(sp)
        displayHistory = sp.getBoolean("displayHistory", false)
        alwaysFilterOnHistory = sp.getBoolean("alwaysFilterOnHistory", true)
    }

    private fun updateAccountView() {
        updateViewFilters()
        if (displayHistoryActually != (displayHistory || alwaysFilterOnHistory && !Util.isEmptyString(filterActually))
                || hiddenAccounts != hiddenAccountsActually
                || displayOptions != displayOptionsActually
                || displayForcedCounterActually != displayForcedCounter
        ) {
            val searchPanel = findViewById<View>(R.id.searchFilterPanel)
            searchPanel?.visibility = if ("__disabled" == displayOptions.filterKey) View.GONE else View.VISIBLE
            displayAccounts()
        }
    }


    var primaryBookCache = ArrayList<Bridge.Book>()
    fun displayAccounts() {
        displayHistoryActually = displayHistory || alwaysFilterOnHistory && !Util.isEmptyString(filterActually)
        displayOptionsActually = displayOptions.copy()
        displayForcedCounterActually = displayForcedCounter
        hiddenAccountsActually.clear()
        hiddenAccountsActually.addAll(hiddenAccounts)


        primaryBookCache = makePrimaryBookCache(displayHistoryActually, false, hiddenAccounts)
        refreshBookCache()

        if (VideLibriApp.getMainIcon() != currentMainIcon) {
            checkMainIcon()
            if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.LOLLIPOP) {
                setTaskDescription(ActivityManager.TaskDescription(null, BitmapFactory.decodeResource(resources, VideLibriApp.getMainIcon(), null)))
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
        if (!hiddenAccounts.isEmpty()) {
            val shownAccounts = VideLibriApp.accounts.size - hiddenAccounts.size
            if (shownAccounts == 1) {
                for (account in VideLibriApp.accounts)
                    if (!hiddenAccounts.contains(account)) {
                        title += ", " + account.prettyName
                        break
                    }
            } else {
                title += ", " + tr(R.string.main_accountcountDD, shownAccounts, VideLibriApp.accounts.size)
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
        val hasAccounts = VideLibriApp.accounts.size > 0
        menu?.apply {
            findItem(R.id.refresh)?.isVisible = VideLibriApp.runningUpdates.isEmpty()
            findItem(R.id.filter)?.isVisible = hasAccounts
            findItem(R.id.renew)?.isVisible = hasAccounts
            findItem(R.id.renewlist)?.isVisible = hasAccounts
            findItem(R.id.research_menu)?.isVisible = hasAccounts
        }
    }

    override fun onOptionsItemIdSelected(id: Int): Boolean {
        when (id) {
            R.id.account_information -> {
                val info = if (VideLibriApp.accounts.isEmpty()) getString(R.string.main_no_accounts)
                           else VideLibriApp.accounts.joinToString (
                              separator = "\n\n",
                              transform = { "${it.prettyName}:\n${tr(R.string.main_last_refreshS, BookFormatter.formatDateFull(it.lastCheckDate))}" }
                           )
                showMessage(message = info, title = getString(R.string.menu_account_information))
            }
            R.id.research_same, R.id.research_author, R.id.research_title -> {
                currentBook()?.let {
                    if (it.account != null) {
                        val query = mapOf(
                                "title" to if (id == R.id.research_same || id == R.id.research_title) it.title else "",
                                "author" to if (id == R.id.research_same || id == R.id.research_author) it.author else "")
                        startActivity<Search>(
                                "libId" to it.account.libId,
                                "query" to query)
                    }
                }
            }
            R.id.filter -> {
                ViewOptionsDialog().show(supportFragmentManager, null)
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
    private val hiddenAccountsActually = ArrayList<Bridge.Account>()


    private var lastSelectedBookForDialog: Bridge.Book? = null

    override fun onBookActionButtonClicked(book: Bridge.Book) {
        when (book.status) {
            Bridge.Book.StatusEnum.Normal -> {
                lastSelectedBookForDialog = book
                Util.showMessageNegPos(DialogId.RENEW_SINGLE_CONFIRM, tr(R.string.renew_single_confirm), R.string.cancel, R.string.renew)
            }
            Bridge.Book.StatusEnum.Ordered, Bridge.Book.StatusEnum.Provided -> {
                lastSelectedBookForDialog = book
                Util.showMessageYesNo(DialogId.CANCEL_CONFIRM, tr(R.string.main_cancelconfirm))
            }
            else -> return
        }
    }

    internal var displayForcedCounterActually: Int = 0

    internal override fun onDialogResult(dialogId: Int, buttonId: Int, more: Bundle?): Boolean {
        when (dialogId) {
            DialogId.RENEW_SINGLE_CONFIRM -> lastSelectedBookForDialog?.let {
                if (buttonId == DialogInterface.BUTTON_POSITIVE) {
                    VideLibriApp.renewBooks(arrayOf(it))
                    showList()
                }
                return true
            }
            DialogId.CANCEL_CONFIRM -> lastSelectedBookForDialog?.let {
                if (buttonId == DialogInterface.BUTTON_POSITIVE) {
                    Bridge.VLBookOperation(arrayOf(it), Bridge.BOOK_OPERATION_CANCEL) //cancel
                    beginLoading(VideLibriBaseActivityOld.LOADING_ACCOUNT_UPDATE)
                    showList()
                }
                lastSelectedBookForDialog = null
                return true
            }
            DialogId.FILTER_LOAD_LIST -> {
                if (buttonId >= 0)
                    findViewById<EditText>(R.id.searchFilter).setText(getFilterHistory()[buttonId])
                return true
            }
            DialogId.SPECIAL_LEND_LIST_OPTIONS -> {
                updateAccountView()
                return true
            }
        }
        return super.onDialogResult(dialogId, buttonId, more)
    }


    class ViewOptionsDialog : android.support.v4.app.DialogFragment(), DialogInterface.OnCancelListener {
        internal lateinit var view: View
        override fun onCreateDialog(savedInstanceState: Bundle?): Dialog {
            val activity = activity ?: return Dialog(VideLibriApp.currentContext())
            val builder = AlertDialog.Builder(activity)
            val inflater = activity.layoutInflater
            view = inflater.inflate(R.layout.options_lendings, null)
            Options.showLendingOptionsInView(activity, view)

            val linearLayout = view.findViewById<LinearLayout>(R.id.viewaccounts)
            linearLayout.removeAllViews()
            val switchboxes = ArrayList<CompoundButton>()
            val accountCheckListener = CompoundButton.OnCheckedChangeListener { compoundButton, b ->
                val acc = compoundButton.tag as Bridge.Account?
                if (acc == null) {
                    if (b)
                        LendingList.hiddenAccounts.clear()
                    else
                        LendingList.hiddenAccounts = ArrayList<Bridge.Account>(Arrays.asList<Bridge.Account>(*VideLibriApp.accounts))
                    for (cb in switchboxes) cb.isChecked = b
                } else {
                    if (!b == LendingList.hiddenAccounts.contains(acc)) return@OnCheckedChangeListener
                    if (!b)
                        LendingList.hiddenAccounts.add(acc)
                    else
                        LendingList.hiddenAccounts.remove(acc)
                }
            }
            val accountClickListener = object : View.OnClickListener {
                override fun onClick(v: View) {
                    val acc = v.tag as Bridge.Account?
                    if (acc == null) {
                        LendingList.hiddenAccounts.clear()
                    } else {
                        LendingList.hiddenAccounts.clear()
                        for (acc2 in VideLibriApp.accounts)
                            if (acc != acc2) LendingList.hiddenAccounts.add(acc2)
                    }
                    dialog.findViewById<CompoundButton>(R.id.viewHistory).isChecked = v.id == R.id.buttonforhistory
                    dialog.cancel()
                }
            }

            for (i in -1 until VideLibriApp.accounts.size) {
                val acc = if (i == -1) null else VideLibriApp.accounts[i]
                val group = inflater.inflate(R.layout.options_lendings_accountrow, null)
                val sb: CompoundButton = group.findViewById<CompoundButton>(R.id.switchbox).apply {
                    isChecked = if (acc == null) LendingList.hiddenAccounts.size <= VideLibriApp.accounts.size / 2 else !LendingList.hiddenAccounts.contains(acc)
                    tag = acc
                    setOnCheckedChangeListener(accountCheckListener)
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
            builder.setOnCancelListener(this)
            val d = builder.create()
            d.window?.setGravity(Gravity.RIGHT or Gravity.TOP)
            return d
        }

        override fun onCancel(dialog: DialogInterface?) {
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
                Util.chooseDialog(DialogId.FILTER_LOAD_LIST, tr(R.string.menu_context_load_filter), getFilterHistory().toTypedArray())
            }
            R.id.save_filter -> {
                val filters = getFilterHistory()
                if (filters.contains(filterActually)) filters.remove(filterActually)
                filters.add(0, filterActually)
                val filtersJson = JSONArray()
                for (s in filters) filtersJson.put(s)
                val sp = PreferenceManager.getDefaultSharedPreferences(this)
                val editor = sp.edit()
                editor.putString("filterHistory", filtersJson.toString())
                editor.commit()
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
        @JvmField var hiddenAccounts = ArrayList<Bridge.Account>()
        internal var displayForcedCounter = 1
        @JvmField var displayHistory = false
        @JvmStatic fun refreshDisplayedLendBooks() {
            displayForcedCounter += 1
            if (VideLibriApp.currentActivity is LendingList)
                (VideLibriApp.currentActivity as LendingList).displayAccounts()
        }

    }

}