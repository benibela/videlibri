package de.benibela.internettools;

import android.util.Log;

import java.security.KeyStore;
import java.security.cert.CertificateException;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.Arrays;

import javax.net.ssl.TrustManager;
import javax.net.ssl.TrustManagerFactory;
import javax.net.ssl.X509TrustManager;

public class X509TrustManagerWrapper implements X509TrustManager {
    protected ArrayList<X509TrustManager> nestedTrustManagers = new ArrayList<>(); //system TMs
    protected ArrayList<X509Certificate> issuers = new ArrayList<>();
    protected X509Certificate[] issuersArray = new X509Certificate[0];

    public interface CustomTrustManagerFactory{
        ArrayList<X509TrustManager> getTrustManagers();
    }
    public static CustomTrustManagerFactory defaultCustomTrustManagerFactory;

    public X509TrustManagerWrapper(){
        if (defaultCustomTrustManagerFactory != null) {
            ArrayList<X509TrustManager> tms = defaultCustomTrustManagerFactory.getTrustManagers();
            if (tms != null) nestedTrustManagers.addAll(tms);
        }
        loadKeystore(null);
    }

    protected void loadKeystore(KeyStore store){
        final ArrayList<TrustManagerFactory> factories = new ArrayList<>();
        try {
            final TrustManagerFactory original = TrustManagerFactory.getInstance(TrustManagerFactory.getDefaultAlgorithm());
            original.init(store);
            factories.add(original);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }

        /*
         * Iterate over the returned trustmanagers, and hold on
         * to any that are X509TrustManagers
         */
        ArrayList<X509TrustManager> outManagers = new ArrayList<>();
        for (TrustManagerFactory tmf : factories)        {
            for( TrustManager tm : tmf.getTrustManagers() ){
                if (tm instanceof X509TrustManager)
                    outManagers.add( (X509TrustManager)tm );
            }
        }

        nestedTrustManagers.addAll(outManagers);
        for( X509TrustManager tm : outManagers )
            issuers.addAll(Arrays.asList(tm.getAcceptedIssuers()));
        issuersArray = issuers.toArray(new X509Certificate[issuers.size()]);
    }

    public boolean isCheckClientTrusted(X509Certificate[] chain, String authType) {
        for (X509TrustManager tm: nestedTrustManagers)
            try {
                tm.checkClientTrusted(chain, authType);
                return true;
            } catch (CertificateException e){
                //ignore
            }
        return false;
    }

    public void checkClientTrusted(X509Certificate[] chain, String authType) throws CertificateException {
        if (isCheckClientTrusted(chain, authType))
            return;
        throw new CertificateException("Invalid client certificate.");
    }

    public boolean isCheckServerTrusted(X509Certificate[] chain, String authType) {
        for (int i = nestedTrustManagers.size() - 1; i >= 0; i--)
            try {
                nestedTrustManagers.get(i).checkServerTrusted(chain, authType);
                return true;
            } catch (CertificateException e) {
                Log.w("Internet Tools", "HTTPS Error: ", e);
            } catch (RuntimeException re) {
                Log.w("Internet Tools", "HTTPS Error: ", re);
            }
        return false;
    }

    @Override
    public void checkServerTrusted(X509Certificate[] chain, String authType) throws CertificateException {
        if (isCheckServerTrusted(chain, authType))
            return;

        String name = "";
        if (chain.length > 0)
            name = chain[0].getSubjectDN().toString();

        throw new CertificateException("Ungültiges Serverzertifikat für "+name);
    }

    public X509Certificate[] getAcceptedIssuers() {
        return issuersArray;
    }
}
