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
import org.acra.BuildConfig
import org.acra.annotation.AcraCore
import org.acra.annotation.AcraDialog
import org.acra.annotation.AcraHttpSender
import org.acra.data.StringFormat
import org.acra.sender.HttpSender
import java.util.*

@AcraCore(buildConfigClass = BuildConfig::class,
        reportFormat = StringFormat.KEY_VALUE_LIST,
        resReportSendSuccessToast = R.string.crash_dialog_ok_toast,
        resReportSendFailureToast = R.string.crash_dialog_fail_toast
        )
@AcraHttpSender(uri = "http://www.benibela.de/autoFeedback.php?app=VideLibri",
        httpMethod = HttpSender.Method.POST,
        socketTimeout = 60*1000)
@AcraDialog(resText = R.string.crash_dialog_text,
        resCommentPrompt = R.string.crash_dialog_comment_prompt
        )
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

        prefs.getString("languageOverride", null)?.takeIf { it.isNotEmpty() }?.let {
            setLanguageOverride(this, it)
        }



        Bridge.initialize(this)
        refreshAccountList()

        Bridge.allThreadsDoneHandler = @SuppressLint("HandlerLeak")
        object : Handler() {
            override fun handleMessage(msg: Message) {
                mainIconCache = 0
                VideLibriApp.runningUpdates.clear()
                NotificationScheduling.onUpdateComplete()

                withActivity<LendingList> {endLoadingAll(VideLibriBaseActivityOld.LOADING_ACCOUNT_UPDATE)}

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
                if (acc.libId == libId && acc.name == userName)
                    return acc
            }
            return null
        }


        @JvmField var runningUpdates: MutableList<Bridge.Account> = ArrayList()

        internal var updateWakeLock: PowerManager.WakeLock? = null
        @JvmStatic fun updateAccount(acc: Bridge.Account?, autoUpdate: Boolean, forceExtend: Boolean) {
            if (acc == null) {
                if (accounts == null) refreshAccountList()
                if (updateWakeLock == null)
                    (currentContext()?.getSystemService(Context.POWER_SERVICE) as PowerManager?)?.apply {
                        newWakeLock(PowerManager.PARTIAL_WAKE_LOCK, "videlibri:updateLock").apply {
                            acquire((10 * 60 * 1000).toLong())
                            setReferenceCounted(false)
                            updateWakeLock = this
                        }
                        Log.i("VideLibri", "Acquired wakelock")
                    }
                accounts?.forEach { updateAccount(it, autoUpdate, forceExtend) }
            } else if (acc.name.isNotEmpty() || acc.pass.isNotEmpty()) { //not search only account
                if (Bridge.VLUpdateAccount(acc, autoUpdate, forceExtend)) {
                    currentActivity<LendingList>()?.beginLoading(VideLibriBaseActivityOld.LOADING_ACCOUNT_UPDATE)
                    if (!runningUpdates.contains(acc))
                        runningUpdates.add(acc)
                }
            }
        }


        @JvmStatic fun renewBooks(books: Array<Bridge.Book>) {
            for (book in books) book.account?.let {
                if (!runningUpdates.contains(it))
                    runningUpdates.add(it)
            }
            if (!runningUpdates.isEmpty())
                currentActivity<LendingList>()?.beginLoading(VideLibriBaseActivityOld.LOADING_ACCOUNT_UPDATE)
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
            val exceptions = Bridge.VLTakePendingExceptions() ?: return
            if (VideLibriApp.errors.size > 3) { //errors eat a lot of memory
                while (VideLibriApp.errors.size > 3)
                    VideLibriApp.errors.removeAt(0)
                System.gc()
            }
            VideLibriApp.errors.addAll(Arrays.asList<Bridge.PendingException>(*exceptions))

            for (i in exceptions.indices) {
                val ex = exceptions[i]
                if (i != 0)
                    showMessage(ex.accountPrettyNames + ": " + ex.error)
                else showDialog {
                    val msg = ex.accountPrettyNames + ": " + ex.error
                    val sendErrorReport: DialogEvent = {
                        val queries = VideLibriApp.errors.map { it.searchQuery }.filterNot { it.isNullOrEmpty() }.joinToString (separator = "\n") {
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
                                                "account" to VideLibriApp.getAccount(ex.firstAccountLib, ex.firstAccountUser)
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


        @JvmStatic fun userPath(context: Context): String {
            return context.filesDir.absolutePath
        }

        internal var defaultLocale: Locale? = null
        internal fun getCurrentLocale(context: Context): Locale =
            if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.N)
                context.resources.configuration.locales.get(0)
            else
                context.resources.configuration.locale


        @JvmStatic fun setLanguageOverride(context: Context, langOverride: String) {
            //Log.i("VIDELIBRI LANG", langOverride);
            if (defaultLocale == null) defaultLocale = getCurrentLocale(context)
            val locale = if (langOverride.isEmpty()) defaultLocale else Locale(langOverride)
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
