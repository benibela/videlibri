package de.benibela.videlibri;

import android.content.ActivityNotFoundException;
import android.content.Intent;
import android.content.pm.PackageManager;
import android.os.Bundle;
import android.view.View;
import android.widget.CheckBox;
import android.widget.CompoundButton;
import android.widget.EditText;
import android.widget.TextView;
import org.acra.ACRA;


import java.io.IOException;
import java.security.KeyManagementException;
import java.security.KeyStoreException;
import java.security.NoSuchAlgorithmException;
import java.security.UnrecoverableKeyException;
import java.util.ArrayList;

import de.benibela.internettools.okhttp.ClientBuilderCustomizer;
import okhttp3.FormBody;
import okhttp3.OkHttpClient;
import okhttp3.Request;
import okhttp3.Response;
import okhttp3.ResponseBody;


public class Feedback extends VideLibriBaseActivity {
    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);    //To change body of overridden methods use File | Settings | File Templates.
        setVideLibriView(R.layout.feedback);
        setTitle(tr(R.string.feedback_feedbacktitle));
        if (!ACRA.getACRASharedPreferences().getBoolean(ACRA.PREF_ENABLE_SYSTEM_LOGS, true))
            ((TextView)findViewById(R.id.feedbackACRAHeader)).setText(tr(R.string.feedback_acraheader));

        if (getIntent().getStringExtra("message") != null)
            ((EditText)findViewById(R.id.text)).setText(getIntent().getStringExtra("message"));

        if (VideLibriApp.errors.size() > 0) {
            {
                final CheckBox errors = ((CheckBox) findViewById(R.id.feedbackIncludeErrors));
                final CheckBox details = (CheckBox) findViewById(R.id.feedbackIncludeErrorDetails);
                final CheckBox anonDetails = (CheckBox) findViewById(R.id.feedbackIncludeErrorAnonymousDetails);
                errors.setVisibility(View.VISIBLE);
                errors.setChecked(true);
                details.setVisibility(View.VISIBLE);
                anonDetails.setVisibility(View.VISIBLE);
                anonDetails.setChecked(true);
                errors.setOnCheckedChangeListener(new CompoundButton.OnCheckedChangeListener() {
                    @Override
                    public void onCheckedChanged(CompoundButton compoundButton, boolean b) {
                        details.setEnabled(b);
                        details.setChecked(details.isChecked() && b);
                        anonDetails.setEnabled(b);
                        anonDetails.setChecked(b);
                    }
                });
            }

            {
                final CheckBox errors = ((CheckBox) findViewById(R.id.feedbackACRAIncludeErrors));
                final CheckBox details = (CheckBox) findViewById(R.id.feedbackACRAIncludeErrorDetails);
                final CheckBox anonDetails = (CheckBox) findViewById(R.id.feedbackACRAIncludeErrorAnonymousDetails);
                errors.setVisibility(View.VISIBLE);
                errors.setChecked(true);
                details.setVisibility(View.VISIBLE);
                anonDetails.setVisibility(View.VISIBLE);
                anonDetails.setChecked(true);
                errors.setOnCheckedChangeListener(new CompoundButton.OnCheckedChangeListener() {
                    @Override
                    public void onCheckedChanged(CompoundButton compoundButton, boolean b) {
                        details.setEnabled(b);
                        details.setChecked(details.isChecked() && b);
                        anonDetails.setEnabled(b);
                        anonDetails.setChecked(b);
                    }
                });
            }
        }

        findButtonById(R.id.button).setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View view) {
                String name = getTextViewText(R.id.name);
                String mail = getTextViewText(R.id.mail);
                String feedback = getTextViewText(R.id.text);
                final String sendData = "Name: "+name+"\n"+"Mail: "+mail+"\n"+feedback;
                final String version = getVersion()+" (android)";
                final boolean includeErrors = ((CheckBox) findViewById(R.id.feedbackIncludeErrors)).isChecked();
                final boolean details = includeErrors && ((CheckBox) findViewById(R.id.feedbackIncludeErrorDetails)).isChecked();
                final boolean anonymousDetails = includeErrors && ((CheckBox) findViewById(R.id.feedbackIncludeErrorAnonymousDetails)).isChecked();
                final ArrayList<Bridge.PendingException> errCache = VideLibriApp.errors;

                (new Thread(new Runnable() {
                    @Override
                    public void run() {
                        OkHttpClient.Builder b = new OkHttpClient.Builder();
                        try {
                            ClientBuilderCustomizer.customize(b);
                        } catch (UnrecoverableKeyException e) {
                            e.printStackTrace();
                        } catch (NoSuchAlgorithmException e) {
                            e.printStackTrace();
                        } catch (KeyStoreException e) {
                            e.printStackTrace();
                        } catch (KeyManagementException e) {
                            e.printStackTrace();
                        }
                        OkHttpClient client = b.build();

                        final int rep = errCache.size() == 0 ? 1 : errCache.size();
                        int ok = 0;
                        String err = "";
                        for (int i = 0; i < rep; i++) { //send each error separately to avoid running out of memory
                            Request.Builder rb = new Request.Builder();
                            rb.url("http://www.benibela.de/autoFeedback.php");
                            FormBody.Builder fbb = new FormBody.Builder();
                            try {
                                fbb.add("app", "VideLibri");
                                fbb.add("ver", version);
                                fbb.add("data", sendData);
                                if (i < errCache.size()) {
                                    Bridge.PendingException e = errCache.get(i);
                                    fbb.add("error" + i, "Error: " + e.error+" bei " + e.library +  "\n");
                                    if (details) fbb.add("errorDetails" + i, e.details);
                                    else if (anonymousDetails) fbb.add("errorAnonDetails" + i, e.anonymousDetails); //including both will cause an outofmemory exception
                                }

                                Request r = rb.post(fbb.build()).build();
                                Response response = client.newCall(r).execute();
                                ResponseBody body = response.body();
                                if (body != null && body.string().contains("PHPOK")) ok += 1;
                            } catch (IOException e) {
                                err = e.getLocalizedMessage();
                                e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
                            }

                            System.gc();
                        }

                        final int fok = ok;
                        final String ferr = err;
                        runOnUiThread(new Runnable() {
                            @Override
                            public void run() {
                                if (fok > 0 )
                                    Util.showMessage(DialogId.FEEDBACK_SEND_ATTEMPTED, tr(fok == rep ? R.string.feedback_send_ok : R.string.feedback_send_failed));
                                else
                                    Util.showMessage(tr(R.string.feedback_send_failedconnect) + "\n" + ferr);
                            }
                        });

                    }
                })).start();

            }
        });

        findViewById(R.id.textViewMail).setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View view) {
                Intent emailIntent = new Intent(android.content.Intent.ACTION_SEND);
                emailIntent.setAction(Intent.ACTION_SEND);
                emailIntent.setType("message/rfc822");
                emailIntent.putExtra(android.content.Intent.EXTRA_EMAIL, "benito@benibela.de");
                emailIntent.putExtra(android.content.Intent.EXTRA_SUBJECT, "VideLibri feedback "+getVersion());
                try{
                    startActivity(emailIntent);
                } catch (ActivityNotFoundException e) {
                    Util.showMessage(tr(R.string.error_nomailapp));
                }
            }
        });

        findViewById(R.id.acra).setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View view) {
                if (((CheckBox) findViewById(R.id.feedbackACRAIncludeErrors)).isChecked()) {
                    boolean details = ((CheckBox) findViewById(R.id.feedbackACRAIncludeErrorDetails)).isChecked();
                    boolean anonymousDetails = ((CheckBox) findViewById(R.id.feedbackACRAIncludeErrorAnonymousDetails)).isChecked();
                    int i = 0;
                    for (Bridge.PendingException e: VideLibriApp.errors) {
                        i+=1;
                        ACRA.getErrorReporter().putCustomData("error"+i, "Error: " + e.error+" bei " + e.library +  "\n");
                        if (details) ACRA.getErrorReporter().putCustomData("errorDetails"+i, e.details);
                        else if (anonymousDetails) ACRA.getErrorReporter().putCustomData("errorAnonDetails"+i, e.anonymousDetails);
                    }
                }


                ACRA.getErrorReporter().handleException(null);
                finish();
            }
        });
    }

    String getVersion(){
        try {
            return getPackageManager().getPackageInfo("de.benibela.videlibri", 0).versionName ;
        } catch (PackageManager.NameNotFoundException e) {
            return "??";
        }
    }

    @Override
    boolean onDialogResult(int dialogId, int buttonId, Bundle more) {
        switch (dialogId){
            case DialogId.FEEDBACK_SEND_ATTEMPTED:
                findViewById(R.id.button).postDelayed(new Runnable() {
                    @Override
                    public void run() {
                        finish();
                    }
                }, 100); //delayed to avoid  "android.view.WindowManager$BadTokenException: Unable to add window -- token android.os.BinderProxy@40b47bd8 is not valid" error.
                // Probably not necessary
                return true;
        }
        return super.onDialogResult(dialogId, buttonId, more);
    }
}
