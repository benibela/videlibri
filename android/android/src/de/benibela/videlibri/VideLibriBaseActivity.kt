package de.benibela.videlibri

import android.annotation.SuppressLint
import android.app.Activity
import android.content.Intent
import android.net.Uri
import android.os.Bundle
import android.view.Menu
import android.view.MenuInflater
import android.view.MenuItem
import de.benibela.videlibri.jni.Bridge

@SuppressLint("Registered")
open class VideLibriBaseActivity: VideLibriBaseActivityOld(){


    private val loadingTasks = ArrayList<Int>()
    private var loadingItem: MenuItem? = null

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        VideLibriApp.initializeBridge()
        if (savedInstanceState != null) {
            loadingTasks.clear()
            loadingTasks += savedInstanceState.getIntegerArrayList("activeLoadingTasks")
        }
    }

    override fun onResume() {
        super.onResume()

        VideLibriApp.currentActivity = this

        refreshLoadingIcon()
        checkMainIcon()

        for (b in VideLibriApp.pendingDialogs)
            showPreparedDialog(this, b)
        VideLibriApp.pendingDialogs.clear()
    }

    override fun onPause() {
        if (VideLibriApp.currentActivity === this) VideLibriApp.currentActivity = null
        super.onPause()
    }

    override fun onSaveInstanceState(outState: Bundle?) {
        super.onSaveInstanceState(outState)
        outState?.putIntegerArrayList("activeLoadingTasks", loadingTasks)
    }

    override fun onDestroy() {
        if (VideLibriApp.currentActivity === this) VideLibriApp.currentActivity = null
        super.onDestroy()
    }


    override fun onCreateOptionsMenu(menu: Menu): Boolean {
        val inflater = menuInflater
        inflater.inflate(R.menu.basemenu, menu)
        onCreateOptionsMenuPrimary(menu, inflater)
        inflater.inflate(R.menu.loadingmenu, menu)
        onCreateOptionsMenuOverflow(menu, inflater)
        loadingItem = menu.findItem(R.id.loading)?.apply {
            setActionView(R.layout.actionbar_loading)
            actionView?.setOnClickListener { showLoadingInfo() }
            isVisible = loadingTasks.size > 0
        }
        return super.onCreateOptionsMenu(menu)
    }
    protected open fun onCreateOptionsMenuPrimary(menu: Menu, inflater: MenuInflater) = Unit
    protected open fun onCreateOptionsMenuOverflow(menu: Menu, inflater: MenuInflater) = Unit
    protected open fun setOptionMenuVisibility(menu: Menu?) = Unit

    override fun onPrepareOptionsMenu(menu: Menu?): Boolean {
        val x = super.onPrepareOptionsMenu(menu)
        menu ?: return false
        loadingItem = menu.findItem(R.id.loading)
        refreshLoadingIcon()
        setOptionMenuVisibility(menu)

        return x
    }

    private fun refreshLoadingIcon(){
        loadingItem?.isVisible = loadingTasks.size > 0
    }
    fun beginLoading(loadingId: Int) {
        loadingTasks += loadingId
        refreshLoadingIcon()
    }

    fun endLoading(loadingId: Int) {
        loadingTasks -= loadingId
        refreshLoadingIcon()
    }

    fun endLoadingAll(loadingId: Int) {
        loadingTasks.removeAll{ it == loadingId }
        refreshLoadingIcon()
    }

    fun endLoadingAll(loadingId: IntArray) {
        loadingTasks.removeAll{ loadingId.contains(it) }
        refreshLoadingIcon()
    }

    fun isLoading(loadingId: Int): Boolean {
        return loadingTasks.contains(loadingId)
    }

    private fun showLoadingInfo() {
        showMessage(loadingTasks.joinToString(separator = "\n") {
            getString(when (it) {
                LOADING_ACCOUNT_UPDATE -> R.string.loading_account_update
                LOADING_COVER_IMAGE -> R.string.loading_cover
                LOADING_SEARCH_CONNECTING -> R.string.loading_search_connecting
                LOADING_SEARCH_SEARCHING -> R.string.loading_search_searching
                LOADING_SEARCH_DETAILS -> R.string.loading_search_details
                LOADING_SEARCH_ORDER -> R.string.loading_search_order
                LOADING_SEARCH_ORDER_HOLDING -> R.string.loading_search_order_holding
                LOADING_SEARCH_MESSAGE -> R.string.loading_search_message
                LOADING_INSTALL_LIBRARY -> R.string.loading_search_install_library
                else -> return@joinToString ""
            })
        })
    }

    private val REQUESTED_LIBRARY_HOMEPAGE = 29324
    private val REQUESTED_LIBRARY_CATALOGUE = 29325

    override fun onOptionsItemIdSelected(id: Int): Boolean {
        when (id) {
            android.R.id.home -> onBackPressed()
            R.id.search -> VideLibriApp.newSearchActivity()
            R.id.accounts -> startActivity<LendingList>()
            R.id.options -> startActivity<Options>()
            R.id.refresh -> VideLibriApp.updateAccount(null, false, false)
            R.id.renew -> showMessageYesNo(R.string.base_renewallconfirm) {
                VideLibriApp.updateAccount(null, false, true)
                withActivity<RenewList> { onBackPressed() }
            }
            R.id.renewlist -> startActivity<RenewList>()
            R.id.import_ -> startActivity<ImportExport>("mode" to ImportExport.MODE_IMPORT)
            R.id.export -> startActivity<ImportExport>("mode" to ImportExport.MODE_EXPORT)
            R.id.libinfo -> startActivityForResult<LibraryList>(REQUESTED_LIBRARY_CATALOGUE,"reason" to getString(R.string.base_chooselibhomepage), "search" to true)
            R.id.libcatalogue -> startActivityForResult<LibraryList>(REQUESTED_LIBRARY_CATALOGUE,"reason" to getString(R.string.base_chooselibcat), "search" to true)
            R.id.newlib -> startActivityForResult<NewLibrary>(RETURNED_FROM_NEW_LIBRARY)
            R.id.feedback -> startActivity<Feedback>()
            R.id.debuglog -> startActivity<DebugLogViewer>()
            R.id.about -> startActivity<About>()
            else -> return false
        }
        return true
    }

    override fun onActivityResult(requestCode: Int, resultCode: Int, data: Intent?) {
        if ((requestCode == REQUESTED_LIBRARY_CATALOGUE || requestCode == REQUESTED_LIBRARY_HOMEPAGE) && resultCode == RESULT_OK) {
            Bridge.VLGetLibraryDetails(LibraryList.lastSelectedLibId ?: return)?.apply {
                showUriInBrowser(if (requestCode == REQUESTED_LIBRARY_HOMEPAGE) homepageBase else homepageCatalogue)
            }
            return
        }

        super.onActivityResult(requestCode, resultCode, data)
    }

    fun showUriInBrowser(uri: String) {
        try {
            startActivity(Intent(Intent.ACTION_VIEW, Uri.parse(uri)))
        } catch (e: android.content.ActivityNotFoundException) {
            showMessage(tr(R.string.err_uri_open_failed, uri))
        }
    }

    protected fun finishWithResult(){
        setResult(Activity.RESULT_OK, Intent())
        finish()
    }

}