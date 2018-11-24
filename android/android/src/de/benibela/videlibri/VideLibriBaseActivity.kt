package de.benibela.videlibri

import android.annotation.SuppressLint
import android.os.Bundle
import android.view.Menu
import android.view.MenuInflater
import android.view.MenuItem
import de.benibela.videlibri.jni.Bridge
import java.util.ArrayList

@SuppressLint("Registered")
open class VideLibriBaseActivity: VideLibriBaseActivityOld(){


    protected var loadingTasks: ArrayList<Int> = ArrayList<Int>();
    private var loadingItem: MenuItem? = null

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        Bridge.initialize(this)
        if (savedInstanceState != null) loadingTasks = savedInstanceState.getIntegerArrayList("activeLoadingTasks")
    }

    override fun onResume() {
        super.onResume()

        VideLibriApp.currentActivity = this

        loadingItem?.isVisible = loadingTasks.size > 0
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
        loadingItem?.isVisible = loadingTasks.size > 0
        setOptionMenuVisibility(menu)

        return x
    }


    fun beginLoading(loadingId: Int) {
        loadingTasks.add(loadingId)
        loadingItem?.isVisible = loadingTasks.size > 0
    }

    fun endLoading(loadingId: Int) {
        val pos = loadingTasks.indexOf(loadingId)
        if (pos >= 0) loadingTasks.removeAt(pos)
        loadingItem?.isVisible = loadingTasks.size > 0
    }

    fun endLoadingAll(loadingId: Int) {
        while (true) {
            val pos = loadingTasks.indexOf(loadingId)
            if (pos < 0) break
            loadingTasks.removeAt(pos)
        }
        loadingItem?.isVisible = loadingTasks.size > 0
    }

    fun endLoadingAll(loadingId: IntArray) {
        for (id in loadingId) endLoadingAll(id)
    }

    fun isLoading(loadingId: Int): Boolean {
        return loadingTasks.indexOf(loadingId) >= 0
    }

    internal fun showLoadingInfo() {
        val sb = StringBuilder()
        sb.append(tr(R.string.loading_tasks_info))
        for (id in loadingTasks) {
            var code = 0
            when (id) {
                LOADING_ACCOUNT_UPDATE -> code = R.string.loading_account_update
                LOADING_COVER_IMAGE -> code = R.string.loading_cover
                LOADING_SEARCH_CONNECTING -> code = R.string.loading_search_connecting
                LOADING_SEARCH_SEARCHING -> code = R.string.loading_search_searching
                LOADING_SEARCH_DETAILS -> code = R.string.loading_search_details
                LOADING_SEARCH_ORDER -> code = R.string.loading_search_order
                LOADING_SEARCH_ORDER_HOLDING -> code = R.string.loading_search_order_holding
                LOADING_SEARCH_MESSAGE -> code = R.string.loading_search_message
                LOADING_INSTALL_LIBRARY -> code = R.string.loading_search_install_library
            }
            if (code != 0) sb.append(tr(code))
            sb.append("\n")
        }
        Util.showMessage(sb.toString())
    }


}