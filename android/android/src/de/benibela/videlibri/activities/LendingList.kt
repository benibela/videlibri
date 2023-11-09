package de.benibela.videlibri.activities

import android.Manifest
import android.app.ActivityManager
import android.app.AlertDialog
import android.app.Dialog
import android.content.DialogInterface
import android.content.pm.PackageManager
import android.graphics.BitmapFactory
import android.os.*
import android.text.Editable
import android.view.*
import android.widget.CompoundButton
import android.widget.EditText
import androidx.core.app.ActivityCompat
import androidx.core.content.ContextCompat
import androidx.fragment.app.DialogFragment
import de.benibela.videlibri.*
import de.benibela.videlibri.databinding.OptionsLendingsAccountrowBinding
import de.benibela.videlibri.databinding.OptionsLendingsBinding
import de.benibela.videlibri.jni.*
import de.benibela.videlibri.notifications.checkForRequiredNotificationPermission
import de.benibela.videlibri.utils.*
import kotlinx.parcelize.Parcelize
import java.util.*


class LendingList: BookListActivity(){
    @Parcelize
    class FilterState(
            var actually: String = "",
            var isMultiLine: Boolean = false
    ): Parcelable
    var filter = FilterState()

    private var searchPanel: View? = null

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        registerState(::filter)
        createDrawerToggle()

        searchPanel = findViewById(R.id.searchFilterPanel)

        updateFilterMultiLine()
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

    }

    internal fun setFilter(s: String) {
        val oldFilterActually = filter.actually
        filter.actually = s
        if (displayOptions.alwaysFilterOnHistory && oldFilterActually.isEmpty() != filter.actually.isEmpty()) {
            displayAccounts()
        } else
            refreshBookCache()
    }


    override fun onResume() {
        super.onResume()

        updateAccountView()

        //setTitle("Ausleihen");  //does not work in onCreate (why? makes the title invisible) No. it just works sometimes?
        if (!cacheShown)
            displayBookCache()

        if (ContextCompat.checkSelfPermission(this, Manifest.permission.INTERNET) != PackageManager.PERMISSION_GRANTED)
        //unnecessary, because not dangerous??
            ActivityCompat.requestPermissions(this, arrayOf(Manifest.permission.INTERNET), 0)
    }

    override fun onPostResume() {
        super.onPostResume()

        if (Accounts.isEmpty()) {
            Handler(Looper.getMainLooper()).postDelayed({
                if (VideLibriApp.currentActivity is LendingList) //do not open the list, if the user navigated away
                    startActivity<AccountInfo>("mode" to AccountInfo.MODE_ACCOUNT_CREATION_INITIAL)
            }, 200) //without delay it has crashed on old android (android 3 or something) in onresume
        } else if (globalOptionsAndroid.notifications.lastAskedForPermission < Bridge.currentPascalDate)
            checkForRequiredNotificationPermission(this)
    }

    private fun updateViewFilters() {
        displayOptions = globalOptionsAndroid.bookListDisplayOptions.copy()
        displayOptions.showHistory = displayOptions.showHistory || (displayOptions.alwaysFilterOnHistory && filter.actually.isNotEmpty())
        searchPanel?.isVisibleNotGone = "__disabled" != displayOptions.filterKey
    }

    private fun updateAccountView() {
        updateViewFilters()
        displayOptionsActually.alwaysFilterOnHistory = displayOptions.alwaysFilterOnHistory
        if (displayOptions != displayOptionsActually || displayForcedCounterActually != displayForcedCounter)
            displayAccounts()
    }


    private var primaryBookCache = ArrayList<Bridge.Book>()
    fun displayAccounts() {
        displayOptions.showHistory = globalOptionsAndroid.bookListDisplayOptions.showHistory || (displayOptions.alwaysFilterOnHistory && filter.actually.isNotEmpty())
        displayOptionsActually = displayOptions.copy()
        displayForcedCounterActually = displayForcedCounter

        primaryBookCache = makePrimaryBookCache(displayOptionsActually.showHistory, false)
        refreshBookCache()

        if (VideLibriApp.mainIcon != currentMainIcon) {
            checkMainIcon()
            if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.LOLLIPOP) {
                setTaskDescription(ActivityManager.TaskDescription(null, BitmapFactory.decodeResource(resources, VideLibriApp.mainIcon, null)))
            }
        }
    }


    private fun refreshBookCache() {
        val filterActually = filter.actually
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
        if (displayOptionsActually.showHistory) {
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
            if (displayOptionsActually.showHistory) {
                if (bookCountPrimary == bookCount)
                    getString(R.string.main_bookcounthistoryDD, bookCountPrimaryNoHistory, bookCountPrimary)
                else
                    getString(R.string.main_bookcounthistoryDDD, bookCount, bookCountPrimaryNoHistory, bookCountPrimary)
            } else {
                if (bookCountPrimary != bookCount)
                    getString(R.string.main_bookcountDD, bookCount, bookCountPrimary)
                else
                    resources.getQuantityString(R.plurals.main_bookcountPluralD, bookCount, bookCount)
            }
        val hiddenCount = Accounts.filterHidden().size
        if (hiddenCount > 0) {
            val shownAccounts = Accounts.size - hiddenCount
            if (shownAccounts == 1) {
                for (account in accounts)
                    if (account.isShown) {
                        title += ", " + account.prettyName
                        break
                    }
            } else {
                title += ", " + getString(R.string.main_accountcountDD, shownAccounts, Accounts.size)
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
                R.id.refresh -> Accounts.filterWithRunningUpdate().isEmpty()
                R.id.filter, R.id.renew, R.id.renewlist -> hasAccounts
                R.id.research_menu -> hasAccounts && detailsVisible()
                R.id.delete -> hasAccounts && detailsVisible() && details.book.history
                else -> return@forItems
            }
        }
    }

    private fun escapeXQueryQuotes(s: String) = s.replace("\"", "\"\"")
    private fun setXQueryFilter(query: String){
        if (query.contains('\n')) {
            filter.isMultiLine = true
            updateFilterMultiLine()
        }
        findViewById<EditText>(R.id.searchFilter).setText(query)
        showList()
    }
    private fun setXQuerySearch(query: String){
        showInputDialog(R.string.xquery_book_tracking_example_regex_dialog) { message ->
            setXQueryFilter(query.replace("%s", message ))
        }
    }

    override fun onOptionsItemIdSelected(id: Int): Boolean {
        when (id) {
            R.id.account_information -> {
                Accounts.refreshAccounts()
                val info = if (Accounts.isEmpty()) getString(R.string.main_no_accounts)
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
                setXQueryFilter("\$books[author = \"${escapeXQueryQuotes(it.author)}\"]")
            }
            R.id.filter -> {
                ViewOptionsDialog().show(supportFragmentManager, null)
            }
            R.id.delete -> {
                val book = details.book
                showMessageYesNo(getString(R.string.delete_book_confirmS, book.title)) {
                    Bridge.VLChangeBook(book, null)
                    showToast(R.string.delete_book_confirmed)
                    refreshDisplayedLendBooks()
                }
            }
            R.id.xquery_example_most_common_authors -> setXQueryFilter("for \$book in \$books\ngroup by \$author := \$book.author\norder by count(\$book) descending\nreturn \$author[1] || ': ' || count(\$book) ")
            R.id.xquery_example_most_common_author_books -> setXQueryFilter("let \$author := (for \$book in \$books group by \$author := \$book.author order by count(\$book) descending return \$author)[1]\nreturn \$books[author = \$author]")
            R.id.xquery_example_borrowed_repeatedly -> setXQueryFilter("for \$book in \$books\ngroup by \$temp := \$book.author || ':' || \$book.title\nwhere count(\$book) > 1\nreturn \$book[1]")
            R.id.xquery_example_longest_title -> setXQueryFilter("(for \$book in \$books\norder by string-length(\$book.title) descending\nreturn \$book)[1]")
            R.id.xquery_example_author_regex -> setXQuerySearch("\$books[matches(author, '%s', 'i')]")
            R.id.xquery_example_title_regex -> setXQuerySearch("\$books[matches(title, '%s', 'i')]")
            R.id.xquery_example_primes -> setXQueryFilter("for \$i in 2 to 100\nwhere empty((2 to \$i - 1)[\$i mod . = 0])\nreturn \$i")
            else -> return super.onOptionsItemIdSelected(id)
        }
        return true
    }

    private var displayOptionsActually = BookListDisplayOptions()

    override fun onBookActionButtonClicked(book: Bridge.Book) {
        when (book.status) {
            BookStatus.Normal -> showDialog {
                message(R.string.renew_single_confirm)
                negativeButton(android.R.string.cancel)
                positiveButton(R.string.renew) {
                    VideLibriApp.renewBooks(arrayOf(book))
                    withActivity<LendingList> { showList() }
                }
            }
            BookStatus.Ordered, BookStatus.Provided, BookStatus.Reserved -> showMessageYesNo(R.string.main_cancelconfirm) {
                book.account?.isUpdating = true
                Bridge.VLBookOperation(arrayOf(book), Bridge.BOOK_OPERATION_CANCEL) //cancel
                withActivity<LendingList> { showList() }
            }
            else -> return
        }
    }

    private var displayForcedCounterActually: Int = 0


    class ViewOptionsDialog : DialogFragment(), DialogInterface.OnCancelListener {
        //internal lateinit var view: View
        private lateinit var binding: OptionsLendingsBinding
        override fun onCreateDialog(savedInstanceState: Bundle?): Dialog {
            val builder = AlertDialog.Builder(activity)
            builder.setOnCancelListener(this)
            activity?.let { activity ->
                val inflater = activity.layoutInflater
                binding = OptionsLendingsBinding.inflate(inflater)
                //@SuppressLint("InflateParams")
                //view = inflater.inflate(R.layout.options_lendings, null)
                Options.showLendingOptionsInView(activity, binding)


                val linearLayout = binding.viewaccounts
                linearLayout.removeAllViews()
                val switchboxes = ArrayList<CompoundButton>()
                val accountCheckListener = View.OnClickListener { v ->
                    val acc = v.tag as Bridge.Account?
                    val b = (v as? CompoundButton)?.isChecked ?: return@OnClickListener
                    if (acc == null) {
                        if (b) Accounts.showAll()
                        else Accounts.hideAll()
                        for (cb in switchboxes) cb.isChecked = b
                    } else acc.isHidden = !b
                }
                val accountClickListener = object : View.OnClickListener {
                    override fun onClick(v: View) {
                        val acc = v.tag as Bridge.Account?
                        if (acc == null) Accounts.showAll()
                        else acc.showAlone()
                        val dialog = this@ViewOptionsDialog.dialog ?: return
                        dialog.findViewById<CompoundButton>(R.id.viewHistory).isChecked = v.id == R.id.buttonforhistory
                        dialog.cancel()
                    }
                }

                for (i in -1 until Accounts.size) {
                    val acc = if (i == -1) null else accounts[i]
                    val row = OptionsLendingsAccountrowBinding.inflate(inflater, linearLayout, true)
                    val sb: CompoundButton = row.switchbox.apply {
                        isChecked = if (acc == null) Accounts.filterHidden().size <= Accounts.size / 2 else !acc.isHidden
                        tag = acc
                        isSaveEnabled = false
                        setOnClickListener(accountCheckListener)
                    }
                    if (acc != null) switchboxes.add(sb)
                    row.button.apply {
                        text = acc?.prettyName ?: getText(R.string.main_allaccounts)
                        tag = acc
                        setOnClickListener(accountClickListener)
                    }
                    row.buttonforhistory.apply {
                        tag = acc
                        setOnClickListener(accountClickListener)
                    }
                }

                builder.setView(binding.root)
            }
            return builder.create().apply {
                window?.setGravity(Gravity.END or Gravity.TOP)
            }
        }

        override fun onCancel(dialog: DialogInterface) {
            super.onCancel(dialog)
            val activity = activity ?: return
            Options.putLendingOptionsFromView(activity, binding)
            if (activity is LendingList)
                activity.updateAccountView()
        }
    }

    override fun onCreateContextMenu(menu: ContextMenu?, v: View?, menuInfo: ContextMenu.ContextMenuInfo?) {
        if (v?.id == R.id.searchFilter) {
            menuInflater.inflate(R.menu.searchfiltercontextmenu, menu)
            menu?.findItem(R.id.toggle_singleline)?.setTitle(if (filter.isMultiLine) R.string.menu_context_filter_singleline else R.string.menu_context_filter_multiline)
            contextMenuSelectedItem = filter.actually
        } else
            super.onCreateContextMenu(menu, v, menuInfo)
    }

    override fun onContextItemSelected(item: MenuItem): Boolean {
        when (item.itemId) {
            R.id.load_filter -> {
                showChooseDialog(R.string.menu_context_load_filter, globalOptionsAndroid.filterHistory.toList()) {
                    currentActivity<LendingList>()?.findViewById<EditText>(R.id.searchFilter)?.setText(globalOptionsAndroid.filterHistory[it])
                }
            }
            R.id.save_filter -> {
                val filters = globalOptionsAndroid.filterHistory.toMutableList()
                if (filters.contains(filter.actually)) filters.remove(filter.actually)
                filters.add(0, filter.actually)
                globalOptionsAndroid.filterHistory = filters.toTypedArray()
                globalOptionsAndroid.save()
            }
            R.id.clear -> {
                findViewById<EditText>(R.id.searchFilter).setText("")
            }
            R.id.paste, R.id.pastereplace -> {
                Clipboard.text?.let {
                    findViewById<EditText>(R.id.searchFilter).setText(
                            (if (item.itemId == R.id.pastereplace) "" else filter.actually) + it
                    )
                }
            }
            R.id.toggle_singleline -> {
                filter.apply {
                    isMultiLine = !isMultiLine
                    updateFilterMultiLine()
                }
            }
            else -> return super.onContextItemSelected(item)
        }
        return true

    }

    override fun onBackPressed() {
        if (listVisible() && filter.actually != "") findViewById<EditText>(R.id.searchFilter).setText("")
        else super.onBackPressed()
    }

    private fun updateFilterMultiLine() {
        findViewById<EditText>(R.id.searchFilter).apply {
            if (filter.isMultiLine) {
                isSingleLine = false
                maxLines = 10
            } else {
                maxLines = 1
                isSingleLine = true
            }
        }
    }


    companion object {
        internal var displayForcedCounter = 1
        @JvmStatic fun refreshDisplayedLendBooks() {
            displayForcedCounter += 1
            withActivity<LendingList> { displayAccounts() }
        }

    }

}