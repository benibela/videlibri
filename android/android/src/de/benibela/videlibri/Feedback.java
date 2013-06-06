package de.benibela.videlibri;

import android.content.ActivityNotFoundException;
import android.content.DialogInterface;
import android.content.Intent;
import android.content.pm.PackageManager;
import android.os.Bundle;
import android.view.View;
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
        findButtonById(R.id.button).setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View view) {
                String name = getTextViewText(R.id.name);
                String mail = getTextViewText(R.id.mail);
                String feedback = getTextViewText(R.id.text);
                final String sendData = "Name: "+name+"\n"+"Mail: "+mail+"\n"+feedback;
                final String version =  getVersion();
                (new Thread(new Runnable() {
                    @Override
                    public void run() {
                        VideLibriHttpClient client =  new VideLibriHttpClient();
                        HttpPost post = new HttpPost("http://www.benibela.de/autoFeedback.php");
                        List<NameValuePair> data = new ArrayList<NameValuePair>(2);
                        data.add(new BasicNameValuePair("app", "VideLibri"));
                        data.add(new BasicNameValuePair("ver", version+" (android)"));
                        data.add(new BasicNameValuePair("data", sendData));
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
                                            finish();
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
    }

    String getVersion(){
        try {
            return getPackageManager().getPackageInfo("de.benibela.videlibri", 0).versionName ;
        } catch (PackageManager.NameNotFoundException e) {
            return "??";
        }
    }
}
