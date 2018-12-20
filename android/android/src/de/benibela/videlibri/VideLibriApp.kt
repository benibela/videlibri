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
import de.benibela.internettools.X509TrustManagerWithAdditionalKeystores
import de.benibela.internettools.X509TrustManagerWrapper
import de.benibela.videlibri.internet.UserKeyStore
import de.benibela.videlibri.internet.VideLibriKeyStore
import de.benibela.videlibri.jni.Bridge
import de.benibela.videlibri.notifications.NotificationScheduling
import de.benibela.videlibri.notifications.Notifier
import org.acra.ACRA
import org.acra.ReportField.*
import org.acra.ReportingInteractionMode
import org.acra.annotation.ReportsCrashes
import java.util.*

@ReportsCrashes(formUri = "http://www.benibela.de/autoFeedback.php?app=VideLibri",
        logcatArguments = arrayOf("-t", "2500", "-v", "threadtime"),
        mode = ReportingInteractionMode.DIALOG,
        resToastText = R.string.crash_toast_text,
        resDialogText = R.string.crash_dialog_text,
        resDialogIcon = android.R.drawable.ic_dialog_info,
        resDialogCommentPrompt = R.string.crash_dialog_comment_prompt,
        resDialogOkToast = R.string.crash_dialog_ok_toast,
        customReportContent = arrayOf(REPORT_ID, APP_VERSION_CODE, APP_VERSION_NAME, PACKAGE_NAME, FILE_PATH, PHONE_MODEL, BRAND, PRODUCT, ANDROID_VERSION, BUILD, TOTAL_MEM_SIZE, AVAILABLE_MEM_SIZE, BUILD_CONFIG, CUSTOM_DATA, IS_SILENT, STACK_TRACE, INITIAL_CONFIGURATION, CRASH_CONFIGURATION, DISPLAY, USER_COMMENT, USER_EMAIL, USER_APP_START_DATE, USER_CRASH_DATE, DUMPSYS_MEMINFO, /*LOGCAT,*/
        INSTALLATION_ID, DEVICE_FEATURES, ENVIRONMENT, SHARED_PREFERENCES))
class VideLibriApp : Application(), Bridge.VideLibriContext {
    override fun onCreate() {
        super.onCreate()

        ACRA.init(this)

        instance = this
        val prefs = PreferenceManager.getDefaultSharedPreferences(this)

        X509TrustManagerWrapper.defaultCustomTrustManagerFactory = UserKeyStore.makeFactory()
        UserKeyStore.loadUserCertificates(prefs)
        X509TrustManagerWithAdditionalKeystores.defaultKeystoreFactory = X509TrustManagerWithAdditionalKeystores.LazyLoadKeyStoreFactory { VideLibriKeyStore() }


        if (ACRA.isACRASenderServiceProcess()) {
            Bridge.initialized = true
            return
        }

        val langOverride = prefs.getString("languageOverride", null)
        if (!Util.isEmptyString(langOverride))
            setLanguageOverride(this, langOverride)


        Bridge.initialize(this)
        refreshAccountList()
        //ACRA.getErrorReporter().putCustomData("app", "VideLibri");
        //ACRA.getErrorReporter().putCustomData("ver", getVersion()+" (android)");

        Bridge.allThreadsDoneHandler = @SuppressLint("HandlerLeak")
        object : Handler() {
            override fun handleMessage(msg: Message) {
                mainIconCache = 0
                VideLibriApp.runningUpdates.clear()
                NotificationScheduling.onUpdateComplete()

                (currentActivity as? LendingList)?.endLoadingAll(VideLibriBaseActivityOld.LOADING_ACCOUNT_UPDATE)

                VideLibriApp.refreshDisplayedLendBooks()

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
                (currentActivity as? NewLibrary)?.endLoading(VideLibriBaseActivityOld.LOADING_INSTALL_LIBRARY)
                val status = msg.what
                val more = Bundle()
                more.putInt("status", status)
                Util.showMessage(DialogId.INSTALLATION_DONE, Util.tr(if (status == 1) R.string.app_libregistered else R.string.app_libregisterfailed), more)
            }
        }

        Bridge.searchEventHandler = @SuppressLint("HandlerLeak")
        object : Handler() {
            override fun handleMessage(msg: Message) {
                val event = msg.obj as Bridge.SearchEvent
                if ((currentActivity as? SearchEventHandler)?.onSearchEvent(event) == true)
                    return
                event.searcherAccess!!.pendingEvents.add(event)
            }
        }


        NotificationScheduling.rescheduleDailyIfNecessary(this, false)
    }

    override fun userPath(): String {
        return userPath(this)
    }

    companion object {

        internal var mainIconCache: Int = 0
        @JvmStatic val mainIcon: Int
            get() {
                if (mainIconCache != 0) return mainIconCache
                if (accounts.isNullOrEmpty()) return R.drawable.icon
                val book = Bridge.VLGetCriticalBook()
                mainIconCache = R.drawable.icong
                if (book != null) {
                    when (BookFormatter.getStatusColor(book)) {
                        Color.RED -> mainIconCache = R.drawable.iconr
                        Color.YELLOW -> mainIconCache = R.drawable.icon
                    }
                }
                return mainIconCache
            }


        @SuppressLint("StaticFieldLeak")
        @JvmStatic lateinit var instance: VideLibriApp
        @SuppressLint("StaticFieldLeak")
        @JvmField var currentActivity: Activity? = null

        @JvmStatic fun currentContext(): Context? {
            return currentActivity ?: instance.applicationContext
        }

        @JvmField var accounts: Array<Bridge.Account>? = null
        internal var accountUpdateCounter = 0

        @JvmField var errors = ArrayList<Bridge.PendingException>()

        @JvmField internal var pendingDialogs = ArrayList<Bundle>()

        @JvmStatic fun addAccount(acc: Bridge.Account) {
            Bridge.VLAddAccount(acc)
            refreshAccountList()
            updateAccount(acc, false, false)
        }

        @JvmStatic fun deleteAccount(acc: Bridge.Account?) {
            if (acc == null) return
            Bridge.VLDeleteAccount(acc)
            if (LendingList.hiddenAccounts.contains(acc)) LendingList.hiddenAccounts.remove(acc)
            refreshAccountList()
            refreshDisplayedLendBooks()
        }

        @JvmStatic fun changeAccount(old: Bridge.Account, newacc: Bridge.Account) {
            Bridge.VLChangeAccount(old, newacc)
            if (LendingList.hiddenAccounts.contains(old)) {
                LendingList.hiddenAccounts.remove(old)
                LendingList.hiddenAccounts.add(newacc)
            }
            refreshAccountList()
            updateAccount(newacc, false, false)
        }

        @JvmStatic fun refreshAccountList() {
            accountUpdateCounter++
            accounts = Bridge.VLGetAccounts()
        }

        @JvmStatic fun refreshDisplayedLendBooks() {
            LendingList.refreshDisplayedLendBooks()
        }


        @JvmStatic fun getAccount(libId: String, userName: String): Bridge.Account? {
            accounts?.forEach { acc ->
                if (Util.equalStrings(acc.libId, libId) && Util.equalStrings(acc.name, userName))
                    return acc
            }
            return null
        }


        @JvmField var runningUpdates: MutableList<Bridge.Account> = ArrayList()

        internal var updateWakeLock: PowerManager.WakeLock? = null
        @JvmStatic fun updateAccount(acc: Bridge.Account?, autoUpdate: Boolean, forceExtend: Boolean) {
            if (acc == null) {
                if (accounts == null) refreshAccountList()
                if (updateWakeLock == null && currentContext() != null) {
                    val pm = currentContext()!!.getSystemService(Context.POWER_SERVICE) as PowerManager?
                    if (pm != null) {
                        val wl = pm.newWakeLock(PowerManager.PARTIAL_WAKE_LOCK, "videlibri:updateLock")
                        wl.acquire((10 * 60 * 1000).toLong())
                        wl.setReferenceCounted(false)
                        updateWakeLock = wl
                    }
                    Log.i("VideLibri", "Acquired wakelock")
                }
                accounts?.forEach { updateAccount(it, autoUpdate, forceExtend) }
                return
            }
            if (acc.name.isEmpty() && acc.pass.isEmpty())
                return  //search only account
            if (Bridge.VLUpdateAccount(acc, autoUpdate, forceExtend)) {
                (currentActivity as? LendingList)?.beginLoading(VideLibriBaseActivityOld.LOADING_ACCOUNT_UPDATE)
                if (!runningUpdates.contains(acc))
                    runningUpdates.add(acc)
            }
        }


        @JvmStatic fun renewBooks(books: Array<Bridge.Book>) {
            for (book in books) book.account?.let {
                if (!runningUpdates.contains(it))
                    runningUpdates.add(it)
            }
            if (!runningUpdates.isEmpty())
                (currentActivity as? LendingList)?.beginLoading(VideLibriBaseActivityOld.LOADING_ACCOUNT_UPDATE)
            Bridge.VLBookOperation(books, Bridge.BOOK_OPERATION_RENEW)
        }


        @JvmStatic fun newSearchActivity() {
            val intent = Intent(instance, Search::class.java)
            intent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK)
            accounts?.let { accounts ->
                val libId = accounts[0].libId
                intent.putExtra("libId", libId)
                val tempLib = Bridge.VLGetLibraryDetails(accounts[0].libId)
                intent.putExtra("libName", tempLib?.prettyName)
                if (accounts.any { it -> libId != it.libId})
                    intent.putExtra("showLibList", true)
            }
            instance.startActivity(intent)
        }


        @JvmStatic fun showPendingExceptions() {
            val exceptions = Bridge.VLTakePendingExceptions()
            if (exceptions == null) return
            if (VideLibriApp.errors.size > 3) { //errors eat a lot of memory
                while (VideLibriApp.errors.size > 3)
                    VideLibriApp.errors.removeAt(0)
                System.gc()
            }
            VideLibriApp.errors.addAll(Arrays.asList<Bridge.PendingException>(*exceptions))

            for (i in exceptions.indices) {
                val ex = exceptions[i]
                if (i != 0)
                    Util.showMessage(ex.accountPrettyNames + ": " + ex.error)
                else {
                    when (ex.kind) {
                        Bridge.PendingException.KIND_LOGIN -> {
                            val more = Bundle()
                            more.putString("lib", ex.firstAccountLib)
                            more.putString("user", ex.firstAccountUser)
                            Util.showMessage(DialogId.ERROR_LOGIN, ex.accountPrettyNames + ": " + ex.error, R.string.app_error_report_btn, R.string.ok, R.string.app_error_check_passwd_btn, more)
                        }
                        Bridge.PendingException.KIND_INTERNET -> Util.showMessage(DialogId.ERROR_INTERNET, ex.accountPrettyNames + ": " + ex.error, R.string.app_error_report_btn, R.string.ok, R.string.app_error_check_internet_btn)
                        Bridge.PendingException.KIND_UNKNOWN -> Util.showMessageYesNo(DialogId.ERROR_CONFIRM, ex.accountPrettyNames + ": " + ex.error + "\n\n" + Util.tr(R.string.app_error_report))
                        else -> Util.showMessageYesNo(DialogId.ERROR_CONFIRM, ex.accountPrettyNames + ": " + ex.error + "\n\n" + Util.tr(R.string.app_error_report))
                    }
                }
            }
        }


        @JvmStatic fun userPath(context: Context): String {
            return context.filesDir.absolutePath
        }

        internal var defaultLocale: Locale? = null
        internal fun getCurrentLocale(context: Context): Locale {
            if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.N) {
                val ls = context.resources.configuration.locales
                return ls.get(0)
            } else {

                return context.resources.configuration.locale
            }
        }

        @JvmStatic fun setLanguageOverride(context: Context, langOverride: String) {
            //Log.i("VIDELIBRI LANG", langOverride);
            if (defaultLocale == null) defaultLocale = getCurrentLocale(context)
            val locale = if (Util.isEmptyString(langOverride)) defaultLocale else Locale(langOverride)
            Locale.setDefault(locale)
            val config = Configuration()
            config.locale = locale
            context.applicationContext.resources.updateConfiguration(config, null)
        }

        @JvmStatic fun initializeBridge() {
            Bridge.initialize(instance)
        }
    }

}
