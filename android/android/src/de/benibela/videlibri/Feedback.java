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
        setContentView(R.layout.feedback);
        setTitle("VideLibri Feedback");
        if (!ACRA.getACRASharedPreferences().getBoolean(ACRA.PREF_ENABLE_SYSTEM_LOGS, true))
            ((TextView)findViewById(R.id.feedbackACRAHeader)).setText("Per ACRA für Fehlerberichte: ");

        if (getIntent().getStringExtra("message") != null)
            ((EditText)findViewById(R.id.text)).setText(getIntent().getStringExtra("message"));

        if (VideLibriApp.errors.size() > 0) {
            {
                final CheckBox errors = ((CheckBox) findViewById(R.id.feedbackIncludeErrors));
                final CheckBox details = (CheckBox) findViewById(R.id.feedbackIncludeErrorDetails);
                errors.setVisibility(View.VISIBLE);
                errors.setChecked(true);
                details.setVisibility(View.VISIBLE);
                errors.setOnCheckedChangeListener(new CompoundButton.OnCheckedChangeListener() {
                    @Override
                    public void onCheckedChanged(CompoundButton compoundButton, boolean b) {
                        details.setEnabled(b);
                        details.setChecked(details.isChecked() && b);
                    }
                });
            }

            {
                final CheckBox errors = ((CheckBox) findViewById(R.id.feedbackACRAIncludeErrors));
                final CheckBox details = (CheckBox) findViewById(R.id.feedbackACRAIncludeErrorDetails);
                errors.setVisibility(View.VISIBLE);
                errors.setChecked(true);
                details.setVisibility(View.VISIBLE);
                errors.setOnCheckedChangeListener(new CompoundButton.OnCheckedChangeListener() {
                    @Override
                    public void onCheckedChanged(CompoundButton compoundButton, boolean b) {
                        details.setEnabled(b);
                        details.setChecked(details.isChecked() && b);
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
                String sendData = "Name: "+name+"\n"+"Mail: "+mail+"\n"+feedback;
                if (((CheckBox) findViewById(R.id.feedbackIncludeErrors)).isChecked()) {
                    boolean details = ((CheckBox) findViewById(R.id.feedbackIncludeErrorDetails)).isChecked();
                    sendData += "\nErrors: \n";
                    for (Bridge.PendingException e: VideLibriApp.errors) {
                        sendData += "Error: " + e.error+"\n";
                        if (details) sendData += e.details +"\n\n\n";
                    }
                }
                final String sendDataFinal = sendData;
                final String version =  getVersion();
                (new Thread(new Runnable() {
                    @Override
                    public void run() {
                        VideLibriHttpClient client =  new VideLibriHttpClient();
                        HttpPost post = new HttpPost("http://www.benibela.de/autoFeedback.php");
                        List<NameValuePair> data = new ArrayList<NameValuePair>(2);
                        data.add(new BasicNameValuePair("app", "VideLibri"));
                        data.add(new BasicNameValuePair("ver", version+" (android)"));
                        data.add(new BasicNameValuePair("data", sendDataFinal));
                        boolean ok = false;
                        try {
                            post.setEntity(new UrlEncodedFormEntity(data));
                            HttpResponse response = client.execute(post);
                            BufferedReader reader = new BufferedReader(new InputStreamReader(response.getEntity().getContent()));
                            String line;
                            String total = "";
                            // Read response until the end
                            while ((line = reader.readLine()) != null)
                                total += line;
                            ok = total.contains("PHPOK");
                        } catch (UnsupportedEncodingException e) {
                            e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
                        } catch (ClientProtocolException e) {
                            e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
                        } catch (IOException e) {
                            e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
                        }

                        final boolean fok = ok;
                        runOnUiThread(new Runnable() {
                            @Override
                            public void run() {
                                if (fok) {
                                    showMessage("Feedback wurde erfolgreich abgeschickt.", new MessageHandler() {
                                        @Override
                                        public void onDialogEnd(DialogInterface dialogInterface, int i) {
                                            if (VideLibriApp.currentActivity == Feedback.this) {
                                                findViewById(R.id.button).postDelayed(new Runnable() {
                                                    @Override
                                                    public void run() {
                                                        finish();
                                                    }
                                                }, 100); //delayed to avoid  "android.view.WindowManager$BadTokenException: Unable to add window -- token android.os.BinderProxy@40b47bd8 is not valid" error.
                                                         // Probably not necessary
                                            }
                                        }
                                    });
                                } else
                                    showMessage("Übertragung fehlgeschlagen.");
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
                    showMessage("Keine Mailapp gefunden.");
                }
            }
        });

        findViewById(R.id.acra).setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View view) {
                String sendData = "";
                if (((CheckBox) findViewById(R.id.feedbackACRAIncludeErrors)).isChecked()) {
                    boolean details = ((CheckBox) findViewById(R.id.feedbackACRAIncludeErrorDetails)).isChecked();
                    sendData += "\nErrors: \n";
                    for (Bridge.PendingException e: VideLibriApp.errors) {
                        sendData += "Error: " + e.error+"\n";
                        if (details) sendData += e.details +"\n\n\n";
                    }
                }

                ACRA.getErrorReporter().putCustomData("errors", sendData);

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
}
