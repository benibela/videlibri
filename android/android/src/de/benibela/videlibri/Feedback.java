package de.benibela.videlibri;

import android.content.ActivityNotFoundException;
import android.content.DialogInterface;
import android.content.Intent;
import android.content.pm.PackageManager;
import android.os.Bundle;
import android.view.View;
import android.widget.CheckBox;
import android.widget.CompoundButton;
import android.widget.EditText;
import android.widget.TextView;
import org.acra.ACRA;
import org.apache.http.HttpResponse;
import org.apache.http.NameValuePair;
import org.apache.http.client.ClientProtocolException;
import org.apache.http.client.entity.UrlEncodedFormEntity;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.message.BasicNameValuePair;
import org.apache.http.params.BasicHttpParams;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.List;

/**
 * Created with IntelliJ IDEA.
 * User: benito
 * Date: 6/6/13
 * Time: 3:16 PM
 * To change this template use File | Settings | File Templates.
 */
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
                        final int rep = errCache.size() == 0 ? 1 : errCache.size();
                        int ok = 0;
                        for (int i = 0; i < rep; i++) { //send each error separately to avoid running out of memory
                            VideLibriHttpClient client =  new VideLibriHttpClient();
                            HttpPost post = new HttpPost("http://www.benibela.de/autoFeedback.php");
                            try {
                                List<NameValuePair> data = new ArrayList<NameValuePair>(3 + (VideLibriApp.errors != null ? VideLibriApp.errors.size() * 3 : 0));
                                data.add(new BasicNameValuePair("app", "VideLibri"));
                                data.add(new BasicNameValuePair("ver", version));
                                data.add(new BasicNameValuePair("data", sendData));
                                if (i < errCache.size()) {
                                    Bridge.PendingException e = errCache.get(i);
                                    data.add(new BasicNameValuePair("error" + i, "Error: " + e.error+" bei " + e.library +  "\n"));
                                    if (details) data.add(new BasicNameValuePair("errorDetails" + i, e.details));
                                    else if (anonymousDetails) data.add(new BasicNameValuePair("errorAnonDetails" + i, e.anonymousDetails)); //including both will cause an outofmemory exception
                                }

                                post.setEntity(new UrlEncodedFormEntity(data));
                                HttpResponse response = client.execute(post);
                                BufferedReader reader = new BufferedReader(new InputStreamReader(response.getEntity().getContent()));
                                String line;
                                String total = "";
                                // Read response until the end
                                while ((line = reader.readLine()) != null)
                                    total += line;
                                if (total.contains("PHPOK")) ok += 1;
                            } catch (UnsupportedEncodingException e) {
                                e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
                            } catch (ClientProtocolException e) {
                                e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
                            } catch (IOException e) {
                                e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
                            }

                            System.gc();
                        }

                        final int fok = ok;
                        runOnUiThread(new Runnable() {
                            @Override
                            public void run() {
                                if (fok > 0 )
                                    Util.showMessage(DialogId.FEEDBACK_SEND_ATTEMPTED, tr(fok == rep ? R.string.feedback_send_ok : R.string.feedback_send_failed));
                                else
                                    Util.showMessage(tr(R.string.feedback_send_failedconnect));
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
