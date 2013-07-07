package de.benibela.videlibri;

import android.app.Application;
import android.content.Intent;
import android.content.SharedPreferences;
import android.content.pm.PackageManager;
import org.acra.*;
import org.acra.annotation.*;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

@ReportsCrashes(formKey = "",
                formUri = "http://www.benibela.de/autoFeedback.php?app=VideLibri",
                logcatArguments = { "-t", "2500", "-v", "threadtime"},
                mode = ReportingInteractionMode.DIALOG,
                resToastText = R.string.crash_toast_text, // optional, displayed as soon as the crash occurs, before collecting data which can take a few seconds
                resDialogText = R.string.crash_dialog_text,
                resDialogIcon = android.R.drawable.ic_dialog_info, //optional. default is a warning sign
                resDialogCommentPrompt = R.string.crash_dialog_comment_prompt, // optional. when defined, adds a user text field input with this text resource as a label
                resDialogOkToast = R.string.crash_dialog_ok_toast // optional. displays a Toast message when the user accepts to send a report.
)
public class VideLibriApp extends Application implements Bridge.VideLibriContext {
    @Override
    public void onCreate() {
        super.onCreate();

        ACRA.init(this);

        setACRAlogcat(false);


        instance = this;

        Bridge.initialize(this);
        VideLibriHttpClient.BrokenServers = getResources().getStringArray(R.array.broken_servers);
        accounts = Bridge.VLGetAccounts();

        //ACRA.getErrorReporter().putCustomData("app", "VideLibri");
        //ACRA.getErrorReporter().putCustomData("ver", getVersion()+" (android)");

    }

    static void setACRAlogcat(boolean enabled) {
        ACRAConfiguration config = ACRA.getConfig();
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
            config.setCustomReportContent( Arrays.copyOf(newFields, p) );
        }

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



    static VideLibriApp instance;
    static Bridge.Account accounts[] = null;

    static void addAccount(Bridge.Account acc){
        if (instance == null) return;
        Bridge.VLAddAccount(acc);
        instance.accounts = Bridge.VLGetAccounts();
        updateAccount(acc, false, false);
    }
    static void deleteAccount(Bridge.Account acc){
        if (instance == null || acc == null) return;
        Bridge.VLDeleteAccount(acc);
        instance.accounts = Bridge.VLGetAccounts();
        if (VideLibri.instance != null) {
            if (VideLibri.instance.hiddenAccounts.contains(acc)) VideLibri.instance.hiddenAccounts.remove(acc);
            VideLibri.instance.displayAccount(null);
        }
    }
    static void changeAccount(Bridge.Account old, Bridge.Account newacc){
        if (instance == null) return;
        Bridge.VLChangeAccount(old, newacc);
        instance.accounts = Bridge.VLGetAccounts();
        updateAccount(newacc, false, false);
        if (VideLibri.instance != null) {
            if (VideLibri.instance.hiddenAccounts.contains(old)) {
                VideLibri.instance.hiddenAccounts.remove(old);
                VideLibri.instance.hiddenAccounts.add(newacc);
            }
        }
    }


    static List<Bridge.Account> runningUpdates = new ArrayList<Bridge.Account>();
    static public void updateAccount(Bridge.Account acc, final boolean autoUpdate, final boolean forceExtend){
        if (acc == null ) {
            if (accounts == null) accounts = Bridge.VLGetAccounts();
            for (Bridge.Account a: accounts)
                updateAccount(a, autoUpdate, forceExtend);
            return;
        }
        if (runningUpdates.contains(acc)) return;
        if ((acc.name == null || acc.name.equals("")) && (acc.pass == null || acc.pass.equals("")))
            return; //search only account
        if (Bridge.VLUpdateAccount(acc, autoUpdate, forceExtend)) {
            if (VideLibri.instance != null) VideLibri.instance.setLoading(true);
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



    static void newSearchActivity(){
        Intent intent;
        if (VideLibri.instance != null)
            intent = new Intent(VideLibri.instance, Search.class);
        else {
            intent = new Intent(instance, Search.class);
            intent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
        }
        if (instance.accounts.length > 0){
            String libId = instance.accounts[0].libId;
            intent.putExtra("libId", libId);
            intent.putExtra("libName", instance.accounts[0].getLibrary().namePretty);

            boolean sure = true;
            for (int i=1;i<instance.accounts.length;i++)
                if (!libId.equals(instance.accounts[i].libId)) {
                    sure = false;
                    break;
                }

            if (!sure) intent.putExtra("showLibList", true);
        }
        if (VideLibri.instance != null) VideLibri.instance.startActivity(intent);
        else instance.startActivity(intent);
    }

    @Override
    public String userPath() {
        return VideLibriSuperBase.userPath(this);
    }
}
