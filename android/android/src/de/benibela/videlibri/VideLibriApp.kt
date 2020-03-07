package de.benibela.videlibri

import android.annotation.SuppressLint
import android.app.Activity
import android.app.Application
import android.content.Context
import android.content.Intent
import android.content.res.Configuration
import android.graphics.Color
import android.os.*
import android.preference.PreferenceManager
import android.util.Log
import de.benibela.internettools.Config
import de.benibela.internettools.X509TrustManagerWithAdditionalKeystores
import de.benibela.videlibri.internet.UserKeyStore
import de.benibela.videlibri.internet.VideLibriKeyStore
import de.benibela.videlibri.jni.Bridge
import de.benibela.videlibri.notifications.NotificationScheduling
import de.benibela.videlibri.notifications.Notifier
import org.acra.ACRA
import org.acra.BuildConfig
import org.acra.annotation.AcraCore
import org.acra.annotation.AcraDialog
import org.acra.annotation.AcraHttpSender
import org.acra.config.HttpSenderConfigurationBuilder
import org.acra.data.StringFormat
import org.acra.sender.HttpSender
import java.util.*

@AcraCore(buildConfigClass = BuildConfig::class,
        reportFormat = StringFormat.KEY_VALUE_LIST,
        resReportSendSuccessToast = R.string.crash_dialog_ok_toast,
        resReportSendFailureToast = R.string.crash_dialog_fail_toast
        )
@AcraHttpSender(uri = "http://www.benibela.de/autoFeedback.php?app=VideLibriACRA",
        httpMethod = HttpSender.Method.POST,
        socketTimeout = 5*60*1000,
        connectionTimeout = 2*60*1000
        )
@AcraDialog(resText = R.string.crash_dialog_text,
        resCommentPrompt = R.string.crash_dialog_comment_prompt
        )
class VideLibriApp : Application() {
    override fun onCreate() {
        super.onCreate()

        instance = this
        staticApplicationContext = applicationContext

        val builder = org.acra.config.CoreConfigurationBuilder(this)
        builder.getPluginConfigurationBuilder(HttpSenderConfigurationBuilder::class.java).setUri("http://www.benibela.de/autoFeedback.php?app=VideLibriA"+ Build.VERSION.SDK_INT)
        ACRA.init(this, builder)

        initializeAll(null)

    }

    override fun onConfigurationChanged(newConfig: Configuration?) {
        super.onConfigurationChanged(newConfig)
        staticApplicationContext ?.let { checkLanguageOverride(it) }
    }

    companion object {

        internal var mainIconCache: Int = 0
        @JvmStatic val mainIcon: Int
            get() {
                //Log.i("VIDELIBRI MAINICON", "Cached: " + mainIconCache);
                if (mainIconCache != 0) return mainIconCache
                if (accounts.isNullOrEmpty()) return R.drawable.icon
                val book = Bridge.VLGetCriticalBook()
                mainIconCache = R.drawable.icong
                if (book != null) {
                    //Log.i("VIDELIBRI MAINICON", "Critical book: " + book.title);
                    when (book.getStatusColor()) {
                        Color.RED -> mainIconCache = R.drawable.iconr
                        Color.YELLOW -> mainIconCache = R.drawable.icon
                    }
                }
                //Log.i("VIDELIBRI MAINICON", "New Icon: " + mainIconCache);
                //Log.i("VIDELIBRI MAINICON", "RYG: " + R.drawable.iconr + " "+R.drawable.icon+" "+R.drawable.icong)
                return mainIconCache
            }


        @SuppressLint("StaticFieldLeak")
        @JvmStatic private var instance: VideLibriApp? = null
        @SuppressLint("StaticFieldLeak")
        @JvmStatic var staticApplicationContext: Context? = null
        @SuppressLint("StaticFieldLeak")
        @JvmField var currentActivity: Activity? = null

        @JvmStatic fun currentContext(): Context? {
            return currentActivity ?: staticApplicationContext ?: instance?.applicationContext
        }


        var uiHandler: Handler? = null


        @JvmField var errors = ArrayList<Bridge.PendingException>()

        @JvmField internal var pendingDialogs = ArrayList<Bundle>()



        internal var updateWakeLock: PowerManager.WakeLock? = null
        @JvmStatic fun updateAccount(acc: Bridge.Account?, autoUpdate: Boolean, forceExtend: Boolean) {
            if (acc == null) {
                if (updateWakeLock == null)
                    (currentContext()?.getSystemService(Context.POWER_SERVICE) as PowerManager?)?.apply {
                        newWakeLock(PowerManager.PARTIAL_WAKE_LOCK, "videlibri:updateLock").apply {
                            acquire((10 * 60 * 1000).toLong())
                            setReferenceCounted(false)
                            updateWakeLock = this
                        }
                        Log.i("VideLibri", "Acquired wakelock")
                    }
                accounts.forEach { updateAccount(it, autoUpdate, forceExtend) }
            } else if (acc.isReal) { //not search only account
                if (Bridge.VLUpdateAccount(acc, autoUpdate, forceExtend)) {
                    currentActivity<LendingList>()?.beginLoading(VideLibriBaseActivityOld.LOADING_ACCOUNT_UPDATE)
                    acc.isUpdating = true
                }
            }
        }


        @JvmStatic fun renewBooks(books: Array<Bridge.Book>) {
            for (book in books) book.account?.isUpdating = true
            if (accounts.filterWithRunningUpdate().isNotEmpty())
                currentActivity<LendingList>()?.beginLoading(VideLibriBaseActivityOld.LOADING_ACCOUNT_UPDATE)
            Bridge.VLBookOperation(books, Bridge.BOOK_OPERATION_RENEW)
        }


        @JvmStatic fun newSearchActivity() {
            val intent = Intent(instance, Search::class.java)
            intent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK)
            accounts[0]?.libId?.let { libId ->
                intent.putExtra("libId", libId)
                val tempLib = Bridge.VLGetLibraryDetails(libId)
                intent.putExtra("libName", tempLib?.prettyName)
                if (accounts.any { libId != it.libId})
                    intent.putExtra("showLibList", true)
            }
            staticApplicationContext?.startActivity(intent)
        }


        @JvmStatic fun showPendingExceptions() {
            val exceptions = Bridge.VLTakePendingExceptions() ?: return
            if (errors.size > 3) { //errors eat a lot of memory
                while (errors.size > 3)
                    errors.removeAt(0)
                System.gc()
            }
            errors.addAll(Arrays.asList<Bridge.PendingException>(*exceptions))

            for (i in exceptions.indices) {
                val ex = exceptions[i]
                if (i != 0)
                    showMessage(ex.accountPrettyNames + ": " + ex.error)
                else showDialog {
                    val msg = ex.accountPrettyNames + ": " + ex.error
                    val sendErrorReport: DialogEvent = {
                        val queries = errors.map { it.searchQuery }.filterNot { it.isNullOrEmpty() }.joinToString (separator = "\n") {
                            q -> getString(R.string.app_error_searchedfor) + q
                        }
                        startActivity<Feedback>(
                                "message" to getString(R.string.app_error_anerror) + "\n" + queries + getString(R.string.app_error_needcontact)
                        )
                    }

                    when (ex.kind) {
                        Bridge.PendingException.KIND_LOGIN, Bridge.PendingException.KIND_INTERNET -> {
                            message(ex.accountPrettyNames + ": " + ex.error)
                            negativeButton(R.string.app_error_report_btn, sendErrorReport)
                            neutralButton(R.string.ok)
                            when (ex.kind) {
                                Bridge.PendingException.KIND_LOGIN -> {
                                    positiveButton(R.string.app_error_check_passwd_btn) {
                                        startActivity<AccountInfo>(
                                                "mode" to AccountInfo.MODE_ACCOUNT_MODIFY,
                                                "account" to accounts.get(ex.firstAccountLib, ex.firstAccountUser)
                                        )
                                    }
                                }
                                Bridge.PendingException.KIND_INTERNET -> {
                                    positiveButton(R.string.app_error_check_internet_btn) {
                                        currentContext()?.startActivity(Intent(android.provider.Settings.ACTION_WIRELESS_SETTINGS))
                                    }
                                }
                            }
                        }
                        else -> {
                            message(msg + "\n\n" + getString(R.string.app_error_report))
                            yesButton(sendErrorReport)
                            noButton()
                        }
                    }
                }
            }
        }


        private var defaultLocale: Locale? = null
        private var overrideLocale: Locale? = null
        private var languageOverride: String? = null
        private fun getCurrentLocale(context: Context): Locale =
            if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.N)
                context.resources.configuration.locales.get(0)
            else @Suppress("DEPRECATION")
                context.resources.configuration.locale


        fun setLanguageOverride(context: Context, langOverride: String) {
            languageOverride = langOverride
            //Log.i("VIDELIBRI LANG", langOverride);
            if (defaultLocale == null) defaultLocale = getCurrentLocale(context)
            overrideLocale = if (langOverride.isEmpty()) defaultLocale else Locale(langOverride)
            Locale.setDefault(overrideLocale)
            overrideResourcesLocale(context)
            staticApplicationContext = staticApplicationContext?.let { overrideResourcesLocale(it) }
        }

        fun checkLanguageOverride(context: Context) {
            languageOverride?.takeNonEmpty()?.let {
                setLanguageOverride(context, it)
            }
        }

        fun overrideResourcesLocale(context: Context): Context =
                when {
                    overrideLocale == null -> context
                    Build.VERSION.SDK_INT >= Build.VERSION_CODES.N -> {
                        val configuration = context.resources.configuration
                        configuration.setLocale(overrideLocale)
                        context.createConfigurationContext(configuration)
                    }
                    else -> @Suppress("DEPRECATION") {
                        val resources = context.resources
                        val configuration = context.resources.configuration
                        configuration.locale = overrideLocale
                        resources.updateConfiguration(configuration, resources.displayMetrics)
                        context
                    }
                }


        @JvmStatic fun initializeAll(alternativeContext: Context?) {
            if (Bridge.initialized) return
            if (staticApplicationContext == null ) {
                staticApplicationContext = instance?.applicationContext ?: alternativeContext?.applicationContext
                //Class.forName("android.app.ActivityThread")
                //            .getMethod("currentApplication").invoke(null, (Object[]) null); ??
            }
            uiHandler = Handler(Looper.getMainLooper())

            val context = staticApplicationContext ?: instance?.applicationContext ?: alternativeContext?.applicationContext  ?: alternativeContext ?: return

            val prefs = PreferenceManager.getDefaultSharedPreferences(context)

            Config.defaultCustomTrustManagerFactory = UserKeyStore.makeFactory()
            UserKeyStore.loadUserCertificates(prefs)
            Config.defaultKeystoreFactory = X509TrustManagerWithAdditionalKeystores.LazyLoadKeyStoreFactory { VideLibriKeyStore() }
            Config.invalidCerticateMessage = context.getString(R.string.internet_invalid_certificateS)

            languageOverride = prefs.getString("languageOverride", null)?.takeNonEmpty()
            checkLanguageOverride(context)

            if (instance != null && ACRA.isACRASenderServiceProcess()) {
                Bridge.initialized = true
                return
            }

            Bridge.userPath = context.filesDir.absolutePath

            Bridge.allThreadsDoneHandler = @SuppressLint("HandlerLeak")
            object : Handler() {
                override fun handleMessage(msg: Message) {
                    mainIconCache = 0
                    accounts.allUpdatesComplete()
                    NotificationScheduling.onUpdateComplete()

                    withActivity<LendingList> {endLoadingAll(VideLibriBaseActivityOld.LOADING_ACCOUNT_UPDATE)}

                    LendingList.refreshDisplayedLendBooks()

                    //displayed account has an icon cache, so displayAccount needs to be called before updateNotification
                    Notifier.updateNotification(currentActivity)
                    showPendingExceptions()
                    updateWakeLock?.apply {
                        if (isHeld) {
                            System.gc() //some devices crash when sleep starts during gc run
                            release()
                            updateWakeLock = null
                            Log.i("VideLibri", "Released wakelock")
                        } else
                            Log.i("VideLibri", "Released wakelock (timeout)")

                    }
                }
            }

            Bridge.installationDoneHandler = @SuppressLint("HandlerLeak")
            object : Handler() {
                override fun handleMessage(msg: Message) {
                    withActivity<NewLibrary> { endLoading(VideLibriBaseActivityOld.LOADING_INSTALL_LIBRARY) }
                    val status = msg.what
                    if (status == 1) {
                        showDialog {
                            message(R.string.app_libregistered)
                            onDismiss = { withActivity<NewLibrary> { finish() } }
                        }
                    } else
                        showMessage(R.string.app_libregisterfailed)
                }
            }

            Bridge.searchEventHandler = @SuppressLint("HandlerLeak")
            object : Handler() {
                override fun handleMessage(msg: Message) {
                    val event = msg.obj as Bridge.SearchEvent
                    if (currentActivity<SearchEventHandler>()?.onSearchEvent(event) == true)
                        return
                    event.searcherAccess!!.pendingEvents.add(event)
                }
            }




            Bridge.initialize(context)
            accounts.refreshAll()

            NotificationScheduling.rescheduleDailyIfNecessary(context, false)
        }
    }

}
