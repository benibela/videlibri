package de.benibela.videlibri

import android.annotation.SuppressLint
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
        Bridge.initialize(this)
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
            Util.showDialog(this, b)
        VideLibriApp.pendingDialogs.clear()
    }

    override fun onPause() {
        if (VideLibriApp.currentActivity === this) VideLibriApp.currentActivity = null
        super.onPause()
    }

    override fun onSaveInstanceState(outState: Bundle) {
        super.onSaveInstanceState(outState)
        outState.putIntegerArrayList("activeLoadingTasks", loadingTasks)
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
        loadingItem = menu.findItem(R.id.loading)
        loadingItem?.let {
            it.setActionView(R.layout.actionbar_loading)
            it.actionView?.setOnClickListener { showLoadingInfo() }
            it.isVisible = loadingTasks.size > 0
        }
        return super.onCreateOptionsMenu(menu)
    }
    open protected fun onCreateOptionsMenuPrimary(menu: Menu, inflater: MenuInflater) = Unit
    open protected fun onCreateOptionsMenuOverflow(menu: Menu, inflater: MenuInflater) = Unit
    open protected fun setOptionMenuVisibility(menu: Menu?) = Unit

    override fun onPrepareOptionsMenu(menu: Menu?): Boolean {
        val x = super.onPrepareOptionsMenu(menu)
        if (menu == null) return false
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

    internal fun showLoadingInfo() {
        val sb = StringBuilder()
        sb.append(tr(R.string.loading_tasks_info))
        for (id in loadingTasks) {
            val code = when (id) {
                LOADING_ACCOUNT_UPDATE -> R.string.loading_account_update
                LOADING_COVER_IMAGE -> R.string.loading_cover
                LOADING_SEARCH_CONNECTING -> R.string.loading_search_connecting
                LOADING_SEARCH_SEARCHING -> R.string.loading_search_searching
                LOADING_SEARCH_DETAILS -> R.string.loading_search_details
                LOADING_SEARCH_ORDER -> R.string.loading_search_order
                LOADING_SEARCH_ORDER_HOLDING -> R.string.loading_search_order_holding
                LOADING_SEARCH_MESSAGE -> R.string.loading_search_message
                LOADING_INSTALL_LIBRARY -> R.string.loading_search_install_library
                else -> 0
            }
            if (code != 0) sb.append(tr(code))
            sb.append("\n")
        }
        Util.showMessage(sb.toString())
    }


}