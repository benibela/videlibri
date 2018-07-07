package de.benibela.videlibri;

import android.app.Activity;
import android.app.Application;
import android.content.Context;
import android.content.Intent;
import android.content.pm.PackageManager;
import android.graphics.Color;
import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.os.PowerManager;
import android.util.Log;

import org.acra.*;
import org.acra.annotation.*;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import de.benibela.internettools.LazyLoadKeystore;
import de.benibela.internettools.X509TrustManagerWithAdditionalKeystores;
import de.benibela.videlibri.jni.Bridge;

import static org.acra.ReportField.*;

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

        X509TrustManagerWithAdditionalKeystores.defaultKeystoreFactory = new X509TrustManagerWithAdditionalKeystores.LazyLoadKeyStoreFactory() {
            @Override
            public LazyLoadKeystore factor() {
                return new VideLibriKeyStore();
            }
        };

        Bridge.initialize(this);
        refreshAccountList();

        //ACRA.getErrorReporter().putCustomData("app", "VideLibri");
        //ACRA.getErrorReporter().putCustomData("ver", getVersion()+" (android)");

        Bridge.allThreadsDoneHandler = new Handler(){
            @Override
            public void handleMessage(Message msg) {
                mainIconCache = 0;
                VideLibriApp.runningUpdates.clear();

                if (currentActivity != null) {
                    if (currentActivity instanceof VideLibri)
                        ((VideLibri)currentActivity).endLoadingAll(VideLibriBaseActivity.LOADING_ACCOUNT_UPDATE);

                    VideLibriApp.refreshDisplayedLendBooks();
                    //displayed account has an icon cache, so displayAccount needs to be called before updateNotification
                }
                NotificationService.updateNotification(currentActivity);
                showPendingExceptions();
                if (updateWakeLock != null) {
                    System.gc(); //some devices crash when sleep starts during gc run
                    updateWakeLock.release();
                    updateWakeLock = null;
                    Log.i("VideLibri", "Released wakelock");
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


        if (ACRA.isACRASenderServiceProcess()) return;

        NotificationService.resheduleDailyIfNecessary(this, false);
    }

    static int mainIconCache;
    static int getMainIcon(){
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




    static VideLibriApp instance;
    static Activity currentActivity;
    static Context applicationContext;

    static public Context currentContext(){
        if (currentActivity != null) return currentActivity;
        return applicationContext;
    }


    static Bridge.Account accounts[] = null;
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
        if (VideLibri.hiddenAccounts.contains(acc)) VideLibri.hiddenAccounts.remove(acc);
        refreshAccountList();
        refreshDisplayedLendBooks();
    }
    static void changeAccount(Bridge.Account old, Bridge.Account newacc){
        Bridge.VLChangeAccount(old, newacc);
        if (VideLibri.hiddenAccounts.contains(old)) {
            VideLibri.hiddenAccounts.remove(old);
            VideLibri.hiddenAccounts.add(newacc);
        }
        refreshAccountList();
        updateAccount(newacc, false, false);
    }

    static void refreshAccountList(){
        accountUpdateCounter++;
        accounts = Bridge.VLGetAccounts();
    }

    public static void refreshDisplayedLendBooks() {
        VideLibri.refreshDisplayedLendBooks();
    }


    static Bridge.Account getAccount(String libId, String userName) {
        for (Bridge.Account acc: accounts)
            if (Util.equalStrings(acc.libId, libId) && Util.equalStrings(acc.name, userName))
                return acc;
        return null;
    }


    static List<Bridge.Account> runningUpdates = new ArrayList<>();

    static PowerManager.WakeLock updateWakeLock;
    static public void updateAccount(Bridge.Account acc, final boolean autoUpdate, final boolean forceExtend){
        if (acc == null ) {
            if (accounts == null) refreshAccountList();
            if (updateWakeLock == null && currentContext() != null) {
                PowerManager pm = (PowerManager)currentContext().getSystemService(Context.POWER_SERVICE);
                updateWakeLock = pm.newWakeLock(PowerManager.PARTIAL_WAKE_LOCK, "updateLock");
                updateWakeLock.acquire();
                Log.i("VideLibri", "Acquired wakelock");
            }
            for (Bridge.Account a: accounts)
                updateAccount(a, autoUpdate, forceExtend);
            return;
        }
        if (Util.isEmptyString(acc.name) && Util.isEmptyString(acc.pass))
            return; //search only account
        if (Bridge.VLUpdateAccount(acc, autoUpdate, forceExtend)) {
            if (currentActivity instanceof VideLibri)
                ((VideLibri)currentActivity).beginLoading(VideLibriBaseActivity.LOADING_ACCOUNT_UPDATE);
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
        if (!runningUpdates.isEmpty() && currentActivity instanceof VideLibri)
            ((VideLibri)currentActivity).beginLoading(VideLibriBaseActivity.LOADING_ACCOUNT_UPDATE);
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
            intent.putExtra("libName", accounts[0].getLibrary().namePretty);

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



}
