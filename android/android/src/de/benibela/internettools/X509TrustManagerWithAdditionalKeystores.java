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


/**
 * Based on http://download.oracle.com/javase/1.5.0/docs/guide/security/jsse/JSSERefGuide.html#X509TrustManager
 */
public class X509TrustManagerWithAdditionalKeystores implements X509TrustManager {

    public interface LazyLoadKeyStoreFactory{
        LazyLoadKeystore factor();
    }

    private ArrayList<X509TrustManager> nestedTrustManagers = new ArrayList<>(); //system TMs
    private ArrayList<X509Certificate> issuers = new ArrayList<>();
    private X509Certificate[] issuersArray = new X509Certificate[0];

    LazyLoadKeystore pendingKeystores;
    public static LazyLoadKeyStoreFactory defaultKeystoreFactory;

    public X509TrustManagerWithAdditionalKeystores() {
        if (defaultKeystoreFactory != null)
            pendingKeystores = defaultKeystoreFactory.factor();
        loadKeystore(null);
    }
    public X509TrustManagerWithAdditionalKeystores(LazyLoadKeystore additionalkeyStores) {
        this.pendingKeystores = additionalkeyStores;
        loadKeystore(null);
    }

    private void loadKeystore(KeyStore store){
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

    private void loadPendingKeystore(){
        loadKeystore(pendingKeystores.getStore());
        pendingKeystores = null;
    }

    /*
     * Delegate to the default trust manager.
     */
    public void checkClientTrusted(X509Certificate[] chain, String authType) throws CertificateException {
        for (X509TrustManager tm: nestedTrustManagers)
            try {
                tm.checkClientTrusted(chain, authType);
                return;
            } catch (CertificateException e){
                //ignore
            }

        if (pendingKeystores != null) {
            loadPendingKeystore();
            checkClientTrusted(chain, authType);
            return;
        }
        throw  new CertificateException("Ungültiges Clientzertifikat.");

    }

    /*
     * Loop over the trustmanagers until we find one that accepts our server
     */
    public void checkServerTrusted(X509Certificate[] chain, String authType) throws CertificateException {
        for (int i=nestedTrustManagers.size()-1;i>=0;i--)
            try {
                nestedTrustManagers.get(i).checkServerTrusted(chain, authType);
                return;
            } catch (CertificateException e){
                Log.d("VideLibri", "HTTPS Error: ", e);
            }
            catch (RuntimeException re) {
                Log.d("VideLibri", "HTTPS Error: ", re);
            }

        //only load additional keystores when the system ones failed
        if (pendingKeystores != null) {
            loadPendingKeystore();
            checkServerTrusted(chain, authType);
            return;
        }

        String name = "";
        if (chain.length > 0)
            name = chain[0].getSubjectDN().toString();

        throw new CertificateException("Ungültiges Serverzertifikat für "+name);
    }

    public X509Certificate[] getAcceptedIssuers() {
        return issuersArray;
    }
}
