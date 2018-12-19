package de.benibela.videlibri;

import android.app.Activity;
import android.app.Application;
import android.content.Context;
import android.content.Intent;
import android.content.SharedPreferences;
import android.content.res.Configuration;
import android.graphics.Color;
import android.os.Build;
import android.os.Bundle;
import android.os.Handler;
import android.os.LocaleList;
import android.os.Message;
import android.os.PowerManager;
import android.preference.PreferenceManager;
import android.util.Log;

import org.acra.ACRA;
import org.acra.ReportingInteractionMode;
import org.acra.annotation.ReportsCrashes;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Locale;

import de.benibela.internettools.LazyLoadKeystore;
import de.benibela.internettools.X509TrustManagerWithAdditionalKeystores;
import de.benibela.internettools.X509TrustManagerWrapper;
import de.benibela.videlibri.notifications.NotificationScheduling;
import de.benibela.videlibri.notifications.Notifier;
import de.benibela.videlibri.internet.UserKeyStore;
import de.benibela.videlibri.internet.VideLibriKeyStore;
import de.benibela.videlibri.jni.Bridge;

import static org.acra.ReportField.ANDROID_VERSION;
import static org.acra.ReportField.APP_VERSION_CODE;
import static org.acra.ReportField.APP_VERSION_NAME;
import static org.acra.ReportField.AVAILABLE_MEM_SIZE;
import static org.acra.ReportField.BRAND;
import static org.acra.ReportField.BUILD;
import static org.acra.ReportField.BUILD_CONFIG;
import static org.acra.ReportField.CRASH_CONFIGURATION;
import static org.acra.ReportField.CUSTOM_DATA;
import static org.acra.ReportField.DEVICE_FEATURES;
import static org.acra.ReportField.DISPLAY;
import static org.acra.ReportField.DUMPSYS_MEMINFO;
import static org.acra.ReportField.ENVIRONMENT;
import static org.acra.ReportField.FILE_PATH;
import static org.acra.ReportField.INITIAL_CONFIGURATION;
import static org.acra.ReportField.INSTALLATION_ID;
import static org.acra.ReportField.IS_SILENT;
import static org.acra.ReportField.PACKAGE_NAME;
import static org.acra.ReportField.PHONE_MODEL;
import static org.acra.ReportField.PRODUCT;
import static org.acra.ReportField.REPORT_ID;
import static org.acra.ReportField.SHARED_PREFERENCES;
import static org.acra.ReportField.STACK_TRACE;
import static org.acra.ReportField.TOTAL_MEM_SIZE;
import static org.acra.ReportField.USER_APP_START_DATE;
import static org.acra.ReportField.USER_COMMENT;
import static org.acra.ReportField.USER_CRASH_DATE;
import static org.acra.ReportField.USER_EMAIL;

@ReportsCrashes(formUri = "http://www.benibela.de/autoFeedback.php?app=VideLibri",
                logcatArguments = { "-t", "2500", "-v", "threadtime"},
                mode = ReportingInteractionMode.DIALOG,
                resToastText = R.string.crash_toast_text, // optional, displayed as soon as the crash occurs, before collecting data which can take a few seconds
                resDialogText = R.string.crash_dialog_text,
                resDialogIcon = android.R.drawable.ic_dialog_info, //optional. default is a warning sign
                resDialogCommentPrompt = R.string.crash_dialog_comment_prompt, // optional. when defined, adds a user text field input with this text resource as a label
                resDialogOkToast = R.string.crash_dialog_ok_toast, // optional. displays a Toast message when the user accepts to send a report.
                customReportContent = { REPORT_ID, APP_VERSION_CODE, APP_VERSION_NAME,
                        PACKAGE_NAME, FILE_PATH, PHONE_MODEL, BRAND, PRODUCT, ANDROID_VERSION, BUILD, TOTAL_MEM_SIZE,
                        AVAILABLE_MEM_SIZE, BUILD_CONFIG, CUSTOM_DATA, IS_SILENT, STACK_TRACE, INITIAL_CONFIGURATION, CRASH_CONFIGURATION,
                        DISPLAY, USER_COMMENT, USER_EMAIL, USER_APP_START_DATE, USER_CRASH_DATE, DUMPSYS_MEMINFO, /*LOGCAT,*/
                        INSTALLATION_ID, DEVICE_FEATURES, ENVIRONMENT, SHARED_PREFERENCES }
)
public class VideLibriApp extends Application implements Bridge.VideLibriContext {
    @Override
    public void onCreate() {
        super.onCreate();


        ACRA.init(this);

        applicationContext = getApplicationContext();
        instance = this;
        SharedPreferences prefs = PreferenceManager.getDefaultSharedPreferences(this);

        X509TrustManagerWrapper.defaultCustomTrustManagerFactory = UserKeyStore.makeFactory();
        UserKeyStore.loadUserCertificates(prefs);
        X509TrustManagerWithAdditionalKeystores.defaultKeystoreFactory = new X509TrustManagerWithAdditionalKeystores.LazyLoadKeyStoreFactory() {
            @Override
            public LazyLoadKeystore factor() {
                return new VideLibriKeyStore();
            }
        };


        if (ACRA.isACRASenderServiceProcess()) return;

        String langOverride = prefs.getString("languageOverride", null);
        if (!Util.isEmptyString(langOverride))
            setLanguageOverride(this, langOverride);


        Bridge.initialize(this);
        refreshAccountList();
        //ACRA.getErrorReporter().putCustomData("app", "VideLibri");
        //ACRA.getErrorReporter().putCustomData("ver", getVersion()+" (android)");

        Bridge.allThreadsDoneHandler = new Handler(){
            @Override
            public void handleMessage(Message msg) {
                mainIconCache = 0;
                VideLibriApp.runningUpdates.clear();
                NotificationScheduling.onUpdateComplete();

                if (currentActivity != null) {
                    if (currentActivity instanceof LendingList)
                        ((LendingList)currentActivity).endLoadingAll(VideLibriBaseActivity.LOADING_ACCOUNT_UPDATE);

                    VideLibriApp.refreshDisplayedLendBooks();
                    //displayed account has an icon cache, so displayAccount needs to be called before updateNotification
                }
                Notifier.updateNotification(currentActivity);
                showPendingExceptions();
                if (updateWakeLock != null) {
                    if (updateWakeLock.isHeld()) {
                        System.gc(); //some devices crash when sleep starts during gc run
                        updateWakeLock.release();
                        updateWakeLock = null;
                        Log.i("VideLibri", "Released wakelock");
                    } else
                        Log.i("VideLibri", "Released wakelock (timeout)");

                }
            }
        };

        Bridge.installationDoneHandler = new Handler(){
            @Override
            public void handleMessage(Message msg) {
                if (currentActivity instanceof NewLibrary)
                    ((NewLibrary)currentActivity).endLoading(VideLibriBaseActivity.LOADING_INSTALL_LIBRARY);
                final int status = msg.what;
                Bundle more = new Bundle();
                more.putInt("status", status);
                Util.showMessage(DialogId.INSTALLATION_DONE, Util.tr(status == 1 ? R.string.app_libregistered : R.string.app_libregisterfailed), more);
             }
        };

        Bridge.searchEventHandler = new Handler(){
            @Override
            public void handleMessage(Message msg) {
                Bridge.SearchEvent event = (Bridge.SearchEvent)(msg.obj);
                if (VideLibriApp.currentActivity instanceof SearchEventHandler) {
                    SearchEventHandler handleAct = (SearchEventHandler) VideLibriApp.currentActivity;
                    if (handleAct.onSearchEvent(event)) return;
                }
                event.searcherAccess.pendingEvents.add(event);
            }
        };



        NotificationScheduling.rescheduleDailyIfNecessary(this, false);
    }

    static int mainIconCache;
    static public int getMainIcon(){
        if (mainIconCache != 0) return mainIconCache;
        if (accounts == null || accounts.length == 0) return R.drawable.icon;
        Bridge.Book book = Bridge.VLGetCriticalBook();
        mainIconCache = R.drawable.icong;
        if (book != null) {
            switch (BookFormatter.getStatusColor(book)) {
                case Color.RED: mainIconCache = R.drawable.iconr; break;
                case Color.YELLOW: mainIconCache = R.drawable.icon; break;
            }
        }
        return mainIconCache;
    }




    public static VideLibriApp instance;
    @Nullable
    static Activity currentActivity;
    static Context applicationContext;

    static public Context currentContext(){
        if (currentActivity != null) return currentActivity;
        return applicationContext;
    }

    @Nullable
    static public Bridge.Account accounts[] = null;
    static int accountUpdateCounter = 0;

    static ArrayList<Bridge.PendingException> errors = new ArrayList<>();

    static ArrayList<Bundle> pendingDialogs = new ArrayList<>();

    static void addAccount(Bridge.Account acc){
        Bridge.VLAddAccount(acc);
        refreshAccountList();
        updateAccount(acc, false, false);
    }
    static void deleteAccount(Bridge.Account acc){
        if (acc == null) return;
        Bridge.VLDeleteAccount(acc);
        if (LendingList.hiddenAccounts.contains(acc)) LendingList.hiddenAccounts.remove(acc);
        refreshAccountList();
        refreshDisplayedLendBooks();
    }
    static void changeAccount(Bridge.Account old, Bridge.Account newacc){
        Bridge.VLChangeAccount(old, newacc);
        if (LendingList.hiddenAccounts.contains(old)) {
            LendingList.hiddenAccounts.remove(old);
            LendingList.hiddenAccounts.add(newacc);
        }
        refreshAccountList();
        updateAccount(newacc, false, false);
    }

    public static void refreshAccountList(){
        accountUpdateCounter++;
        accounts = Bridge.VLGetAccounts();
    }

    public static void refreshDisplayedLendBooks() {
        LendingList.refreshDisplayedLendBooks();
    }


    static Bridge.Account getAccount(String libId, String userName) {
        for (Bridge.Account acc: accounts)
            if (Util.equalStrings(acc.libId, libId) && Util.equalStrings(acc.name, userName))
                return acc;
        return null;
    }


    static public List<Bridge.Account> runningUpdates = new ArrayList<>();

    static PowerManager.WakeLock updateWakeLock;
    static public void updateAccount(Bridge.Account acc, final boolean autoUpdate, final boolean forceExtend){
        if (acc == null ) {
            if (accounts == null) refreshAccountList();
            if (updateWakeLock == null && currentContext() != null) {
                PowerManager pm = (PowerManager)currentContext().getSystemService(Context.POWER_SERVICE);
                if (pm != null) {
                    PowerManager.WakeLock wl = pm.newWakeLock(PowerManager.PARTIAL_WAKE_LOCK, "updateLock");
                    wl.acquire(10*60*1000);
                    wl.setReferenceCounted(false);
                    updateWakeLock = wl;
                }
                Log.i("VideLibri", "Acquired wakelock");
            }
            for (Bridge.Account a: accounts)
                updateAccount(a, autoUpdate, forceExtend);
            return;
        }
        if (Util.isEmptyString(acc.name) && Util.isEmptyString(acc.pass))
            return; //search only account
        if (Bridge.VLUpdateAccount(acc, autoUpdate, forceExtend)) {
            if (currentActivity instanceof LendingList)
                ((LendingList)currentActivity).beginLoading(VideLibriBaseActivity.LOADING_ACCOUNT_UPDATE);
            if (!runningUpdates.contains(acc))
                runningUpdates.add(acc);
        }
       /* final Bridge.Account facc = acc;
        Thread t = new Thread(new Runnable() {
            @Override
            public void run() {
                Bridge.VLUpdateAccount(facc, autoUpdate, forceExtend);
               // runningUpdates.remove(facc);
               // instance.displayAccount(facc);
            }
        });
        t.start();*/
    }



    static public void renewBooks(Bridge.Book[] books){
        for (Bridge.Book book: books)
            if (book.account != null && !runningUpdates.contains(book.account))
                runningUpdates.add(book.account);
        if (!runningUpdates.isEmpty() && currentActivity instanceof LendingList)
            ((LendingList)currentActivity).beginLoading(VideLibriBaseActivity.LOADING_ACCOUNT_UPDATE);
        Bridge.VLBookOperation(books, Bridge.BOOK_OPERATION_RENEW); //renew
    }


    static void newSearchActivity(){
        Intent intent;
      //  if (VideLibri.instance != null)
      //      intent = new Intent(VideLibri.instance, Search.class);
      //  else {
            intent = new Intent(instance, Search.class);
            intent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
      //  }
        if (accounts.length > 0){
            String libId = accounts[0].libId;
            intent.putExtra("libId", libId);
            Bridge.LibraryDetails tempLib = Bridge.VLGetLibraryDetails(accounts[0].libId);
            intent.putExtra("libName", tempLib.prettyName);

            boolean sure = true;
            for (int i=1;i< accounts.length;i++)
                if (!libId.equals(accounts[i].libId)) {
                    sure = false;
                    break;
                }

            if (!sure) intent.putExtra("showLibList", true);
        }
        //if (VideLibri.instance != null) VideLibri.instance.startActivity(intent);
        //else
        instance.startActivity(intent);
    }


    public static void showPendingExceptions(){
        Bridge.PendingException[] exceptions = Bridge.VLTakePendingExceptions();
        if (VideLibriApp.errors.size() > 3) { //errors eat a lot of memory
            while (VideLibriApp.errors.size() > 3)
                VideLibriApp.errors.remove(0);
            System.gc();
        }
        VideLibriApp.errors.addAll(Arrays.asList(exceptions));

        for (int i=0;i<exceptions.length;i++){
            Bridge.PendingException ex = exceptions[i];
            if (i != 0) Util.showMessage(ex.accountPrettyNames + ": " + ex.error);
            else {
                switch (ex.kind) {
                    case Bridge.PendingException.KIND_LOGIN:
                        Bundle more = new Bundle();
                        more.putString("lib", ex.firstAccountLib);
                        more.putString("user", ex.firstAccountUser);
                        Util.showMessage(DialogId.ERROR_LOGIN, ex.accountPrettyNames + ": " + ex.error, R.string.app_error_report_btn, R.string.ok, R.string.app_error_check_passwd_btn, more);
                        break;
                    case Bridge.PendingException.KIND_INTERNET:
                        Util.showMessage(DialogId.ERROR_INTERNET, ex.accountPrettyNames + ": " + ex.error, R.string.app_error_report_btn, R.string.ok, R.string.app_error_check_internet_btn);
                        break;
                    case Bridge.PendingException.KIND_UNKNOWN:
                    default:
                        Util.showMessageYesNo(DialogId.ERROR_CONFIRM, ex.accountPrettyNames + ": " + ex.error + "\n\n" + Util.tr(R.string.app_error_report));
                }
            }
        }}


    public static String userPath(Context context) {
        return context.getFilesDir().getAbsolutePath();
    }

    @Override
    public String userPath() {
        return userPath(this);
    }

    static Locale defaultLocale = null;
    static Locale getCurrentLocale(Context context){
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.N){
            LocaleList ls = context.getResources().getConfiguration().getLocales();
            return ls.get(0);
        } else{
            //noinspection deprecation
            return context.getResources().getConfiguration().locale;
        }
    }
    static void setLanguageOverride(Context context, String langOverride){
        //Log.i("VIDELIBRI LANG", langOverride);
        if (defaultLocale == null) defaultLocale = getCurrentLocale(context);
        Locale locale = Util.isEmptyString(langOverride) ? defaultLocale : new Locale(langOverride);
        Locale.setDefault(locale);
        Configuration config = new Configuration();
        config.locale = locale;
        context.getApplicationContext().getResources().updateConfiguration(config, null);
    }

}
