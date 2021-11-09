package de.benibela.videlibri.activities

import android.annotation.SuppressLint
import android.app.Activity
import android.content.Context
import android.content.Intent
import android.content.res.Configuration
import android.net.Uri
import android.os.Build
import android.os.Bundle
import android.os.Parcelable
import android.view.*
import android.widget.ImageView
import androidx.annotation.LayoutRes
import androidx.appcompat.app.ActionBarDrawerToggle
import androidx.core.view.GravityCompat
import androidx.drawerlayout.widget.DrawerLayout
import androidx.viewbinding.ViewBinding
import com.google.android.material.navigation.NavigationView
import de.benibela.videlibri.*
import de.benibela.videlibri.jni.Bridge
import de.benibela.videlibri.utils.startActivityForResult
import kotlin.reflect.KMutableProperty0
import de.benibela.videlibri.utils.*

@SuppressLint("Registered")
open class VideLibriBaseActivity: VideLibriBaseActivityOld(){

    private var mDrawerLayout: DrawerLayout? = null
    private var mDrawerToggle: ActionBarDrawerToggle? = null

    private val loadingTasks = ArrayList<Int>()
    private var loadingItem: MenuItem? = null

    private var savedInstanceState: Bundle? = null

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        this.savedInstanceState = savedInstanceState
        VideLibriApp.initializeAll(this)
        if (savedInstanceState != null) {
            loadingTasks.clear()
            savedInstanceState.getIntegerArrayList("activeLoadingTasks")?.let { loadingTasks += it }
        }
        if (Build.VERSION.SDK_INT < Build.VERSION_CODES.N)
            VideLibriApp.overrideResourcesLocale(this)
    }

    override fun attachBaseContext(base: Context?) {
        val newBase = base?.let { VideLibriApp.overrideResourcesLocale(it) } ?: base
        super.attachBaseContext(newBase)
    }

    override fun onRestoreInstanceState(savedInstanceState: Bundle) {
        super.onRestoreInstanceState(savedInstanceState)
        this.savedInstanceState = null
    }

    override fun onResume() {
        super.onResume()

        refreshLoadingIcon()
        checkMainIcon()
    }

    override fun onPostResume() {
        super.onPostResume()
        VideLibriApp.currentActivity = this
        for (b in VideLibriApp.pendingDialogs)
            showPreparedDialog(this, b)
        VideLibriApp.pendingDialogs.clear()
    }

    override fun onPause() {
        if (VideLibriApp.currentActivity === this) VideLibriApp.currentActivity = null
        super.onPause()
    }

    override fun onSaveInstanceState(outState: Bundle) {
        super.onSaveInstanceState(outState)
        outState.putIntegerArrayList("activeLoadingTasks", loadingTasks)
        registeredStates.forEach { outState.putParcelable(it.key, it.value()) }
        if (VideLibriApp.currentActivity === this) VideLibriApp.currentActivity = null
    }

    override fun onDestroy() {
        if (VideLibriApp.currentActivity === this) VideLibriApp.currentActivity = null
        super.onDestroy()
    }




    override fun setContentView(@LayoutRes layoutResID: Int) {
        setContentView(layoutInflater.inflate(layoutResID, null))
    }

    override fun setContentView(view: View) {
        super.setContentView(view)
        // initActionBar
        setSupportActionBar(findViewById(R.id.actionbar))
        mDrawerLayout = findViewById(R.id.drawer_layout)
        supportActionBar?.apply {
            setDisplayHomeAsUpEnabled(true)
            setHomeButtonEnabled(true)
        }
        findViewById<NavigationView>(R.id.navigation)?.let { navi ->
            navi.inflateHeaderView(R.layout.naviheader)
            navi.setNavigationItemSelectedListener { item ->
                mDrawerLayout?.closeDrawer(GravityCompat.START)
                onOptionsItemIdSelected(item.itemId)
            }
        }
    }

    fun setVideLibriView(@LayoutRes layoutResID: Int) {
        val inflater = layoutInflater
        val layout = inflater.inflate(R.layout.videlibribaselayout, null)
        inflater.inflate(layoutResID, layout.findViewById(R.id.content_holder), true)
        setContentView(layout)
    }

    fun <T: ViewBinding> setVideLibriView(viewBindingInflate:  (LayoutInflater, ViewGroup, Boolean) -> T): T = run {
        val inflater = layoutInflater
        val layout = inflater.inflate(R.layout.videlibribaselayout, null)
        val result = viewBindingInflate(inflater, layout.findViewById(R.id.content_holder), true)
        setContentView(layout)
        result
    }



    private val registeredStates = mutableMapOf<String, () -> Parcelable>()
    fun <T: Parcelable> registerState(name: String, property: KMutableProperty0<T>){
        if (!registeredStates.containsKey(name))
            registeredStates[name] = property::get
        if (savedInstanceState?.containsKey(name) == true) {
            val value = savedInstanceState?.getParcelable<T>(name)
            if (value != null) {
                property.set(value)
                return
            }
        }
    }
    inline fun <reified T: Parcelable> registerState(property: KMutableProperty0<T>){
        //combine property name and class name, so the name is unique
        //(although the class name is not necessarily necessary, since subclasses cannot add a property of the same name)
        //this works without the kotlin reflect library, although it looks like reflection (kotlin class name was reflection)
        val name = "${property.name}:${T::class.java.name}"
        //Log.i("STATE", name)
        registerState(name, property)
    }

    protected fun createDrawerToggle() {
        val layout = mDrawerLayout ?: return
        if (mDrawerToggle == null)
            mDrawerToggle = ActionBarDrawerToggle(
                    this,
                    layout,
                    R.string.drawer_open,
                    R.string.drawer_close
            ).also { toggle ->
                layout.addDrawerListener(toggle)
            }
    }

    override fun onPostCreate(savedInstanceState: Bundle?) {
        super.onPostCreate(savedInstanceState)
        mDrawerToggle?.syncState()
    }

    override fun onConfigurationChanged(newConfig: Configuration) {
        super.onConfigurationChanged(newConfig)
        mDrawerToggle?.onConfigurationChanged(newConfig)
    }

    override fun onOptionsItemSelected(item: MenuItem): Boolean =
        (mDrawerToggle?.onOptionsItemSelected(item) ?: false) ||
                onOptionsItemIdSelected(item.itemId) ||
                super.onOptionsItemSelected(item)


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


    internal var currentMainIcon: Int = 0
    fun checkMainIcon() {
        if (VideLibriApp.mainIcon != currentMainIcon) {
            /*ActionBar ab = getSupportActionBar();
            if (ab != null) {
                currentMainIcon = VideLibriApp.getMainIcon();
                ab.setHomeAsUpIndicator(currentMainIcon);
            }*/
            currentMainIcon = VideLibriApp.mainIcon

            val iconView = findViewById<NavigationView>(R.id.navigation)?.getHeaderView(0)?.findViewById<ImageView>(R.id.icon)
            iconView?.setImageResource(currentMainIcon)
        }
    }


    internal fun refreshLoadingIcon(){
        loadingItem?.isVisible = loadingTasks.size > 0 || CoverLoader.isActive()
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
        val tasks = loadingTasks.map {
            when (it) {
                LOADING_ACCOUNT_UPDATE -> R.string.loading_account_update
                LOADING_SEARCH_CONNECTING -> R.string.loading_search_connecting
                LOADING_SEARCH_SEARCHING -> R.string.loading_search_searching
                LOADING_SEARCH_DETAILS -> R.string.loading_search_details
                LOADING_SEARCH_ORDER -> R.string.loading_search_order
                LOADING_SEARCH_ORDER_HOLDING -> R.string.loading_search_order_holding
                LOADING_SEARCH_MESSAGE -> R.string.loading_search_message
                LOADING_INSTALL_LIBRARY -> R.string.loading_search_install_library
                else -> 0
            }
        }.filter { it != 0 }.toMutableList()
        if (CoverLoader.isActive()) tasks += R.string.loading_cover

        showMessage(tasks.joinToString("\n") { getString(it) })
    }

    private val REQUESTED_LIBRARY_HOMEPAGE = 29324
    private val REQUESTED_LIBRARY_CATALOGUE = 29325

    open fun onOptionsItemIdSelected(id: Int): Boolean {
        when (id) {
            android.R.id.home -> onBackPressed()
            R.id.search -> VideLibriApp.newSearchActivity()
            R.id.accounts -> de.benibela.videlibri.utils.startActivity<LendingList>()
            R.id.options -> de.benibela.videlibri.utils.startActivity<Options>()
            R.id.refresh -> VideLibriApp.updateAccount(null, false, false)
            R.id.renew -> showMessageYesNo(R.string.base_renewallconfirm) {
                VideLibriApp.updateAccount(null, false, true)
                withActivity<RenewList> { onBackPressed() }
            }
            R.id.renewlist -> de.benibela.videlibri.utils.startActivity<RenewList>()
            R.id.import_ -> de.benibela.videlibri.utils.startActivity<Import>()
            R.id.export -> de.benibela.videlibri.utils.startActivity<Export>()
            R.id.libinfo -> startActivityForResult<LibraryList>(REQUESTED_LIBRARY_CATALOGUE,"reason" to getString(R.string.base_chooselibhomepage), "search" to true)
            R.id.libcatalogue -> startActivityForResult<LibraryList>(REQUESTED_LIBRARY_CATALOGUE,"reason" to getString(R.string.base_chooselibcat), "search" to true)
            R.id.newlib -> startActivityForResult<NewLibrary>(RETURNED_FROM_NEW_LIBRARY)
            R.id.feedback -> de.benibela.videlibri.utils.startActivity<Feedback>()
            R.id.debuglog -> de.benibela.videlibri.utils.startActivity<DebugLogViewer>()
            R.id.about -> de.benibela.videlibri.utils.startActivity<About>()
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
            showMessage(getString(R.string.err_uri_open_failed, uri))
        }
    }

    protected fun finishWithResult(){
        setResult(Activity.RESULT_OK, Intent())
        finish()
    }

}