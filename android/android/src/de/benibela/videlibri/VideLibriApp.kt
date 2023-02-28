package de.benibela.videlibri

import android.annotation.SuppressLint
import android.app.Activity
import android.app.Application
import android.content.Context
import android.content.Intent
import android.content.SharedPreferences
import android.content.res.Configuration
import android.graphics.Color
import android.os.*
import android.preference.PreferenceManager
import android.provider.Settings
import android.util.Log
import de.benibela.internettools.Config
import de.benibela.internettools.X509TrustManagerWithAdditionalKeystores
import de.benibela.videlibri.activities.*
import de.benibela.videlibri.internet.UserKeyStore
import de.benibela.videlibri.internet.VideLibriKeyStore
import de.benibela.videlibri.jni.*
import de.benibela.videlibri.notifications.NotificationScheduling
import de.benibela.videlibri.notifications.Notifier
import de.benibela.videlibri.utils.*
import org.acra.ACRA
import org.acra.config.dialog
import org.acra.config.httpSender
import org.acra.data.StringFormat
import org.acra.ktx.initAcra
import org.acra.sender.HttpSender
import org.json.JSONArray
import org.json.JSONException
import java.util.*

class VideLibriApp : Application() {
    override fun onCreate() {
        super.onCreate()

        instance = this
        staticApplicationContext = applicationContext

        //or attachBaseContext?
        initAcra {
            buildConfigClass = de.benibela.videlibri.BuildConfig::class.java
            reportFormat = StringFormat.KEY_VALUE_LIST
            httpSender {
                uri = "https://www.benibela.de/autoFeedback.php?app=VideLibriA${Build.VERSION.SDK_INT}"
                httpMethod = HttpSender.Method.POST
                socketTimeout = 5*60*1000
                connectionTimeout = 2*60*1000
                reportSendSuccessToast = getString(R.string.crash_dialog_ok_toast)
                reportSendFailureToast = getString(R.string.crash_dialog_fail_toast)
            }
            dialog {
                text = getString(R.string.crash_dialog_text)
                commentPrompt = getString(R.string.crash_dialog_comment_prompt)
            }
        }

        initializeAll(null)

    }

    override fun onConfigurationChanged(newConfig: Configuration) {
        super.onConfigurationChanged(newConfig)
        staticApplicationContext ?.let(::checkLanguageOverride)
    }

    companion object {

        internal var mainIconCache: Int = 0
        @JvmStatic val mainIcon: Int
            get() {
                //Log.i("VIDELIBRI MAINICON", "Cached: " + mainIconCache);
                if (mainIconCache != 0) return mainIconCache
                if (accounts.isEmpty()) return R.drawable.icon
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
                acc.isUpdating = true
                if (!Bridge.VLUpdateAccount(acc, autoUpdate, forceExtend))
                    acc.isUpdating = false
            }
        }


        @JvmStatic fun renewBooks(books: Array<Bridge.Book>) {
            for (book in books) book.account?.isUpdating = true
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
            errors.addAll(listOf(*exceptions))

            for (i in exceptions.indices) {
                val ex = exceptions[i]
                if (i != 0)
                    showMessage(ex.accountPrettyNames + ": " + ex.error)
                else showDialog {
                    val msg = ex.accountPrettyNames + ": " + ex.error
                    val sendErrorReport: DialogEvent = {
                        val queries = errors.map { it.searchQuery }.filterNot { it.isNullOrEmpty() }.joinToString(separator = "\n") { q ->
                            getString(R.string.app_error_searchedfor) + q
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
                                        currentContext()?.startActivity(Intent(Settings.ACTION_WIRELESS_SETTINGS))
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
            Locale.setDefault(overrideLocale ?: return)
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
                        val configuration = Configuration() //context.resources.configuration
                        configuration.setLocale(overrideLocale)
                        context.createConfigurationContext(configuration)
                    }
                    else -> @Suppress("DEPRECATION") {
                        val resources = context.resources
                        val configuration = Configuration() // context.resources.configuration
                        configuration.locale = overrideLocale
                        resources.updateConfiguration(configuration, context.resources.displayMetrics)
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

            @Suppress("DEPRECATION") val prefs = PreferenceManager.getDefaultSharedPreferences(context)

            Config.defaultCustomTrustManagerFactory = UserKeyStore.makeFactory()
            Config.defaultKeystoreFactory = X509TrustManagerWithAdditionalKeystores.LazyLoadKeyStoreFactory { VideLibriKeyStore() }
            Config.invalidCerticateMessage = context.getString(R.string.internet_invalid_certificateS)

            languageOverride = prefs.getString("languageOverride", null)?.takeNonEmpty()
            checkLanguageOverride(context)

            if (instance != null && ACRA.isACRASenderServiceProcess()) {
                Bridge.initialized = true
                return
            }

            Bridge.userPath = context.filesDir.absolutePath

            Bridge.allThreadsDoneHandler =
            object : Handler(Looper.getMainLooper()) {
                override fun handleMessage(msg: Message) {
                    mainIconCache = 0
                    accounts.allUpdatesComplete()
                    NotificationScheduling.onUpdateComplete()
                    currentActivity<VideLibriBaseActivity>()?.refreshLoadingIcon()

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

            LibraryUpdateLoader.registerInstallationDoneHandler()

            Bridge.searchEventHandler =
            object : Handler(Looper.getMainLooper()) {
                override fun handleMessage(msg: Message) {
                    val event = msg.obj as SearchEvent
                    if (currentActivity<SearchEventHandler>()?.onSearchEvent(event) == true)
                        return
                    event.searcherAccess?.pendingEvents?.add(event)
                }
            }




            Bridge.initialize(context)
            globalOptionsShared = Bridge.VLGetOptions()
            globalOptionsAndroid = Bridge.VLGetOptionsAndroidOnly()
            ACRA.errorReporter.putCustomData("VL-VERSION", Bridge.VLGetVersion().platform + " " + Bridge.VLGetVersion().buildId)
            importDeprecatedPreferences(prefs)
            UserKeyStore.loadUserCertificates(globalOptionsAndroid.additionalCertificatesBase64)
            accounts.refreshAll()

            NotificationScheduling.rescheduleDailyIfNecessary(context, false)
        }


        private fun importDeprecatedPreferences(prefs: SharedPreferences){
            if (globalOptionsAndroid.hasBeenStartedAtLeastOnce) return
            globalOptionsAndroid.bookListDisplayOptions.apply {
                showHistory = prefs.getBoolean("displayHistory", false)
                noBorrowedBookDetails = prefs.getBoolean("noLendBookDetails", false)
                showRenewCount = prefs.getBoolean("showRenewCount", true)
                groupingKey = prefs.getString("grouping", "_dueWeek") ?: "_dueWeek"
                sortingKey = prefs.getString("sorting", "dueDate") ?: "dueDate"
                filterKey = prefs.getString("filtering", "") ?: ""
                alwaysFilterOnHistory = prefs.getBoolean("alwaysFilterOnHistory", true)
            }
            globalOptionsAndroid.notifications.apply {
                enabled = prefs.getBoolean("notifications", true)
                serviceDelay = prefs.getInt("notificationsServiceDelay", 15)
                lastTime = prefs.getLong("lastNotificationTime", 0)
                lastTitle = prefs.getString("lastNotificationTitle", "") ?: ""
                lastText = prefs.getString("lastNotificationText", "") ?: ""
            }
            globalOptionsAndroid.apply {
                try {
                    val temp = JSONArray(prefs.getString("filterHistory", "[]"))
                    filterHistory = (0 until temp.length()).map { temp.getString(it) }.toTypedArray()
                } catch (e: JSONException) {}
                importExportFileName = prefs.getString("importExportFileName", "") ?: ""
                val certs = prefs.getString("additionalCertificatesBase64", "")
                if (!certs.isNullOrEmpty())
                    additionalCertificatesBase64 = certs.split('|').toTypedArray()
                hasBeenStartedAtLeastOnce = true
                accountCountBackup = prefs.getInt("accountCountBackup", -1)
            }
            globalOptionsAndroid.save()
        }
    }

}
