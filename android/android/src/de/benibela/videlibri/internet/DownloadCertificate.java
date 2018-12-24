package de.benibela.videlibri.internet;

import android.app.Activity;
import android.content.Context;
import android.preference.PreferenceManager;

import java.io.IOException;
import java.security.GeneralSecurityException;
import java.security.cert.X509Certificate;

import de.benibela.internettools.okhttp.ClientBuilderCustomizer;
import de.benibela.videlibri.Options;
import de.benibela.videlibri.R;
import de.benibela.videlibri.Util;
import de.benibela.videlibri.UtilKt;
import de.benibela.videlibri.VideLibriApp;
import okhttp3.OkHttpClient;
import okhttp3.Request;
import okhttp3.Response;

public class DownloadCertificate implements Runnable {
    private X509Certificate downloadedCertificate;
    private class AllAcceptingX509TrustManager extends de.benibela.internettools.X509TrustManagerWrapper{
        @Override
        public boolean isCheckServerTrusted(X509Certificate[] chain, String authType) {
            if (chain.length == 0) return false;
            downloadedCertificate = chain[0];
            /*for (int i=0;i<chain.length;i++) {
                byte[] enc=null;
                try {
                    enc = chain[i].getEncoded();
                } catch (CertificateEncodingException e) {
                    e.printStackTrace();
                }
                Log.i("VL"+i, VideLibriKeyStore.getFingerprint(enc) +": "+chain[i].getSubjectDN().toString());
            }*/
            return true;
        }
    }
    private String server;

    public DownloadCertificate(String server){
        this.server = server;
    }

    @Override
    public void run() {
        String message = "Error:" + server;
        try {
            OkHttpClient.Builder b = new OkHttpClient.Builder();
            b.followRedirects(false);
            ClientBuilderCustomizer.customizeWithTrustManager(b, new AllAcceptingX509TrustManager());
            OkHttpClient client = b.build();
            Request r = new Request.Builder().url("https://" + server).build();
            Response response = client.newCall(r).execute();
            response.close();

            if (downloadedCertificate != null) {
                byte encoded[] = downloadedCertificate.getEncoded();
                boolean added = UserKeyStore.addUserCertificate(encoded);
                message = Util.tr(added ? R.string.certificate_added : R.string.certificate_existing, server, UserKeyStore.getFingerprint(encoded));
            }
        } catch (IOException e) {
            message = Util.tr(R.string.certificate_failed, server, e.getLocalizedMessage());
        } catch (GeneralSecurityException e) {
            message = Util.tr(R.string.certificate_failed, server, e.getLocalizedMessage());
        } catch (IllegalArgumentException e) { //for invalid urls
            message = Util.tr(R.string.certificate_failed, server, e.getLocalizedMessage());
        }

        final String fmessage = message;
        final Context context = VideLibriApp.currentContext();
        if (context instanceof Activity){
            ((Activity)context).runOnUiThread(new Runnable() {
                @Override
                public void run() {
                    UserKeyStore.storeUserCertificates(PreferenceManager.getDefaultSharedPreferences(context));
                    UtilKt.showMessage(fmessage);
                    if (context instanceof Options)
                        ((Options) (context)).updatePreferences();
                }
            });
        }
    }
}

