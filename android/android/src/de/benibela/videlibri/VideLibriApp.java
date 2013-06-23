package de.benibela.videlibri;

import android.app.Application;
import android.content.pm.PackageManager;
import org.acra.*;
import org.acra.annotation.*;

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
public class VideLibriApp extends Application {
    @Override
    public void onCreate() {
        super.onCreate();

        ACRA.init(this);

        //ACRA.getErrorReporter().putCustomData("app", "VideLibri");
        //ACRA.getErrorReporter().putCustomData("ver", getVersion()+" (android)");

    }

    String getVersion(){
        try {
            return getPackageManager().getPackageInfo("de.benibela.videlibri", 0).versionName ;
        } catch (PackageManager.NameNotFoundException e) {
            return "??";
        }
    }
}
