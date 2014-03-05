package de.benibela.videlibri;

import android.app.Activity;
import android.app.Application;
import android.content.DialogInterface;
import android.content.Intent;
import android.content.SharedPreferences;
import android.content.pm.PackageManager;
import android.os.Handler;
import android.os.Message;
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

        allThreadsDoneHandler = new Handler(){
            @Override
            public void handleMessage(Message msg) {
                VideLibriApp.runningUpdates.clear();

                if (currentActivity != null) {
                    NotificationService.showNotification(currentActivity);

                    if (currentActivity instanceof VideLibri)
                        ((VideLibri)currentActivity).setLoading(false);

                    VideLibriApp.displayAccount(null);
                }
                showPendingExceptions();
            }
        };

        installationDoneHandler = new Handler(){
            @Override
            public void handleMessage(Message msg) {
                final int status = msg.what;
                String message = status == 1
                        ? "Bibliothek wurde registriert."
                        : "Bibliotheksregistrierung fehlgeschlagen.";
                Util.showMessage(
                        message,
                        new MessageHandler() {
                            @Override
                            public void onDialogEnd(DialogInterface dialogInterface, int i) {
                                if (status == 1 && (currentActivity instanceof NewLibrary))
                                    currentActivity.finish();
                            }
                        } );
                if (currentActivity instanceof NewLibrary)
                    ((NewLibrary)currentActivity).setLoading(false);
            }
        };

        NotificationService.startIfNecessary(this);
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
            config.setCustomReportContent(Arrays.copyOf(newFields, p));
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
    static Activity currentActivity;

    static Bridge.Account accounts[] = null;

    static ArrayList<Bridge.PendingException> errors = new ArrayList<Bridge.PendingException>();

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
        if (runningUpdates.contains(acc)) return;
        if ((acc.name == null || acc.name.equals("")) && (acc.pass == null || acc.pass.equals("")))
            return; //search only account
        if (Bridge.VLUpdateAccount(acc, autoUpdate, forceExtend)) {
            if (currentActivity instanceof VideLibri) ((VideLibri)currentActivity).setLoading(true);
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
            ((VideLibri)currentActivity).setLoading(true);
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

        if (currentActivity != null) {
            if (VideLibriApp.errors.size() > 3) { //errors eat a lot of memory
                while (VideLibriApp.errors.size() > 3)
                    VideLibriApp.errors.remove(0);
                System.gc();
            }
            VideLibriApp.errors.addAll(Arrays.asList(exceptions));

            String queries = "";
            for (int i=0;i<exceptions.length;i++){
                Bridge.PendingException ex = exceptions[i];
                if (ex.searchQuery != null && !"".equals(ex.searchQuery))
                    queries = queries + "gesucht wurde: " + ex.searchQuery+"\n";
                if (i != exceptions.length - 1) Util.showMessage(ex.accountPrettyNames + ": " + ex.error);
                else {
                    final String message = "Eine Fehlermeldung!!1!\n"+queries+"Bitte auch Kontaktdaten angeben, sonst kann ich keine Antwort/Lösung zurückschicken.";
                    Util.showMessageYesNo(ex.accountPrettyNames + ": " + ex.error + "\n\nWollen Sie den Entwickler über die Meldung benachrichtigen, damit das Template angepasst werden kann?", new MessageHandler() {
                        @Override
                        public void onDialogEnd(DialogInterface dialogInterface, int i) {
                            if (i == DialogInterface.BUTTON_POSITIVE) {
                                Intent intent = new Intent(currentActivity, Feedback.class);
                                intent.putExtra("message", message);
                                currentActivity.startActivity(intent);
                            }
                        }
                    });
                }
            }
        } else VideLibriApp.errors.addAll(Arrays.asList(exceptions));
    }

    @Override
    public String userPath() {
        return VideLibriSuperBase.userPath(this);
    }

    public static void displayAccount(Bridge.Account account) {
        VideLibri.displayAccountStatically(account);
    }
}
