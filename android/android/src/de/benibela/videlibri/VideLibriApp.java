package de.benibela.videlibri;

import android.app.Activity;
import android.app.Application;
import android.content.Context;
import android.content.DialogInterface;
import android.content.Intent;
import android.content.SharedPreferences;
import android.content.pm.PackageManager;
import android.graphics.Color;
import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import org.acra.*;
import org.acra.annotation.*;
import org.acra.config.*;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

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

        //setACRAlogcat(false);


        instance = this;

        Bridge.initialize(this);
        VideLibriHttpClient.BrokenServers = getResources().getStringArray(R.array.broken_servers);
        accounts = Bridge.VLGetAccounts();

        //ACRA.getErrorReporter().putCustomData("app", "VideLibri");
        //ACRA.getErrorReporter().putCustomData("ver", getVersion()+" (android)");

        allThreadsDoneHandler = new Handler(){
            @Override
            public void handleMessage(Message msg) {
                mainIconCache = 0;
                VideLibriApp.runningUpdates.clear();

                if (currentActivity != null) {
                    if (currentActivity instanceof VideLibri)
                        ((VideLibri)currentActivity).endLoadingAll(VideLibriBaseActivity.LOADING_ACCOUNT_UPDATE);

                    VideLibriApp.displayAccount(null);
                    //displayed account has an icon cache, so displayAccount needs to be called before updateNotification
                }
                NotificationService.updateNotification(currentActivity);
                showPendingExceptions();
            }
        };

        installationDoneHandler = new Handler(){
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

        if (ACRA.isACRASenderServiceProcess()) return;

        NotificationService.resheduleDailyIfNecessary(this, false);
    }

    static void setACRAlogcat(boolean enabled) {
        /*ACRAConfiguration config = ACRA.getConfig();
        if (enabled) config.setCustomReportContent(ACRAConstants.DEFAULT_REPORT_FIELDS);
        else {
            ReportField[] fields = ACRAConstants.DEFAULT_REPORT_FIELDS;
            ReportField[] newFields = new ReportField[fields.length];
            int p = 0;
            for (int i=0;i<fields.length;i++)
                if (fields[i] != ReportField.LOGCAT) {
                    newFields[p] = fields[i];
                    p+=1;
                }

            ReportField[] newFields2 = new ReportField[fields.length];
            System.arraycopy(newFields, 0, newFields2, 0, p);
            config.setCustomReportContent(newFields2);
        } */

        SharedPreferences prefs = ACRA.getACRASharedPreferences();
        SharedPreferences.Editor editor = prefs.edit();
        editor.putBoolean(ACRA.PREF_ENABLE_SYSTEM_LOGS, enabled);
        editor.commit();

    }

    String getVersion(){
        try {
            return getPackageManager().getPackageInfo("de.benibela.videlibri", 0).versionName ;
        } catch (PackageManager.NameNotFoundException e) {
            return "??";
        }
    }

    static int mainIconCache;
    static int getMainIcon(){
        if (mainIconCache != 0) return mainIconCache;
        if (accounts == null || accounts.length == 0) return R.drawable.icon;
        boolean hasRed = false;
        boolean hasYellow = false;
        if (currentActivity instanceof VideLibri) {
            for (Bridge.Book book: ((VideLibri)currentActivity).primaryBookCache)
                switch (BookFormatter.getStatusColor(book)) {
                    case Color.RED: hasRed = true; break;
                    case Color.YELLOW: hasYellow = true; break;
                }
        } else {
            for (Bridge.Account facc: VideLibriApp.accounts)
                for (Bridge.Book book: Bridge.VLGetBooks(facc, false))
                    switch (BookFormatter.getStatusColor(book)) {
                        case Color.RED: hasRed = true; break;
                        case Color.YELLOW: hasYellow = true; break;
                    }
        }
        if (hasRed) mainIconCache = R.drawable.iconr;
        else if (hasYellow) mainIconCache = R.drawable.icon;
        else mainIconCache = R.drawable.icong;
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

    static ArrayList<Bridge.PendingException> errors = new ArrayList<Bridge.PendingException>();

    static ArrayList<Bundle> pendingDialogs = new ArrayList<Bundle>();

    static void addAccount(Bridge.Account acc){
        Bridge.VLAddAccount(acc);
        accounts = Bridge.VLGetAccounts();
        updateAccount(acc, false, false);
    }
    static void deleteAccount(Bridge.Account acc){
        if (acc == null) return;
        Bridge.VLDeleteAccount(acc);
        accounts = Bridge.VLGetAccounts();
        if (VideLibri.hiddenAccounts.contains(acc)) VideLibri.hiddenAccounts.remove(acc);
        VideLibriApp.displayAccount(null);
    }
    static void changeAccount(Bridge.Account old, Bridge.Account newacc){
        Bridge.VLChangeAccount(old, newacc);
        accounts = Bridge.VLGetAccounts();
        updateAccount(newacc, false, false);
        if (VideLibri.hiddenAccounts.contains(old)) {
            VideLibri.hiddenAccounts.remove(old);
            VideLibri.hiddenAccounts.add(newacc);
        }
    }
    static Bridge.Account getAccount(String libId, String userName) {
        for (Bridge.Account acc: accounts)
            if (Util.equalStrings(acc.libId, libId) && Util.equalStrings(acc.name, userName))
                return acc;
        return null;
    }


    static List<Bridge.Account> runningUpdates = new ArrayList<Bridge.Account>();
    static Handler allThreadsDoneHandler, installationDoneHandler;
    static int bookUpdateCounter = 1;
    static public void updateAccount(Bridge.Account acc, final boolean autoUpdate, final boolean forceExtend){
        if (acc == null ) {
            if (accounts == null) accounts = Bridge.VLGetAccounts();
            for (Bridge.Account a: accounts)
                updateAccount(a, autoUpdate, forceExtend);
            return;
        }
        if ((acc.name == null || acc.name.equals("")) && (acc.pass == null || acc.pass.equals("")))
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

        String queries = "";
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

    public static void displayAccount(Bridge.Account account) {
        VideLibri.displayAccountStatically(account);
    }


}
