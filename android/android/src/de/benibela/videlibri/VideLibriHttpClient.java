package de.benibela.videlibri;

import android.util.Log;
import org.apache.http.Header;
import org.apache.http.HttpVersion;
import org.apache.http.client.*;
import org.apache.http.conn.ClientConnectionManager;
import org.apache.http.conn.scheme.PlainSocketFactory;
import org.apache.http.conn.scheme.Scheme;
import org.apache.http.conn.scheme.SchemeRegistry;
import org.apache.http.conn.scheme.SocketFactory;
import org.apache.http.cookie.*;
import org.apache.http.impl.client.DefaultHttpClient;

import javax.net.ssl.SSLContext;
import javax.net.ssl.TrustManager;
import javax.net.ssl.TrustManagerFactory;
import javax.net.ssl.X509TrustManager;
import java.io.IOException;
import java.io.InputStream;
import java.net.*;
import java.net.CookieStore;
import java.security.*;
import java.security.cert.CertificateException;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;

import org.apache.http.conn.ssl.SSLSocketFactory;
import org.apache.http.impl.conn.SingleClientConnManager;
import org.apache.http.params.BasicHttpParams;
import org.apache.http.params.HttpParams;
import org.apache.http.params.HttpProtocolParams;
import org.apache.http.protocol.HTTP;
//based on http://stackoverflow.com/questions/2642777/trusting-all-certificates-using-httpclient-over-https

/*class SSLSocketFactoryAllowAll extends org.apache.http.conn.ssl.SSLSocketFactory {
    SSLContext sslContext = SSLContext.getInstance("TLS");

    public SSLSocketFactoryAllowAll(KeyStore truststore) throws NoSuchAlgorithmException, KeyManagementException, KeyStoreException, UnrecoverableKeyException {
        super(truststore);

        TrustManager tm = new X509TrustManager() {
            public void checkClientTrusted(X509Certificate[] chain, String authType) throws CertificateException {
            }

            public void checkServerTrusted(X509Certificate[] chain, String authType) throws CertificateException {
            }

            public X509Certificate[] getAcceptedIssuers() {
                return null;
            }
        };

        sslContext.init(null, new TrustManager[] { tm }, null);
    }

    @Override
    public Socket createSocket(Socket socket, String host, int port, boolean autoClose) throws IOException, UnknownHostException {
        return sslContext.getSocketFactory().createSocket(socket, host, port, autoClose);
    }

    @Override
    public Socket createSocket() throws IOException {
        return sslContext.getSocketFactory().createSocket();
    }
}        */


class LazyLoadKeystore {
    private KeyStore store;
    public KeyStore getStore(){
        if (store == null) store = loadStore();
        return store;
    }
    protected KeyStore loadStore(){
        return null;
    }
}

class SSLSocketFactoryWithAdditionalLazyKeyStore extends SSLSocketFactory {
    protected SSLContext sslContext = SSLContext.getInstance("TLS");

    public SSLSocketFactoryWithAdditionalLazyKeyStore(LazyLoadKeystore keyStore) throws NoSuchAlgorithmException, KeyManagementException, KeyStoreException, UnrecoverableKeyException {
        super(null, null, null, null, null, null);
        sslContext.init(null, new TrustManager[]{new AdditionalKeyStoresTrustManager(keyStore)}, null);
    }

    @Override
    public Socket createSocket(Socket socket, String host, int port, boolean autoClose) throws IOException {
        return sslContext.getSocketFactory().createSocket(socket, host, port, autoClose);
    }

    @Override
    public Socket createSocket() throws IOException {
        return sslContext.getSocketFactory().createSocket();
    }



    /**
     * Based on http://download.oracle.com/javase/1.5.0/docs/guide/security/jsse/JSSERefGuide.html#X509TrustManager
     */
    public static class AdditionalKeyStoresTrustManager implements X509TrustManager {

        protected ArrayList<X509TrustManager> originalTrustManagers = new ArrayList<X509TrustManager>(); //system TMs
        protected ArrayList<X509TrustManager> cachedAdditionalTrustManagers = new ArrayList<X509TrustManager>(); //our TMs that accepted the connection (will probably accept later connections as well)
        protected ArrayList<X509TrustManager> additionalTrustManagers = new ArrayList<X509TrustManager>(); //other TMs to check
        protected ArrayList<X509Certificate> issuers = new ArrayList<X509Certificate>();
        protected X509Certificate[] issuersArray = new X509Certificate[0];

        LazyLoadKeystore additionalKeystores;

        protected AdditionalKeyStoresTrustManager(LazyLoadKeystore additionalkeyStores) {
            this.additionalKeystores = additionalkeyStores;
            loadKeystore(null, originalTrustManagers);

            for( X509TrustManager tm : originalTrustManagers )
                issuers.addAll(Arrays.asList(tm.getAcceptedIssuers()));
            issuersArray = issuers.toArray(new X509Certificate[issuers.size()]);
        }

        void loadKeystore(KeyStore store, ArrayList<X509TrustManager> outManagers){
            final ArrayList<TrustManagerFactory> factories = new ArrayList<TrustManagerFactory>();
            try {
                // The default Trustmanager with default keystore
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
            for (TrustManagerFactory tmf : factories)        {
                for( TrustManager tm : tmf.getTrustManagers() ){
                    if (tm instanceof X509TrustManager)
                        outManagers.add( (X509TrustManager)tm );
                }
            }
        }

        /*
         * Delegate to the default trust manager.
         */
        public void checkClientTrusted(X509Certificate[] chain, String authType) throws CertificateException {
            for (X509TrustManager tm: originalTrustManagers)
                try {
                    tm.checkClientTrusted(chain, authType);
                    return;
                } catch (CertificateException e){
                    //ignore
                }
            throw  new CertificateException("Ungültiges Clientzertifikat.");

        }

        /*
         * Loop over the trustmanagers until we find one that accepts our server
         */
        public void checkServerTrusted(X509Certificate[] chain, String authType) throws CertificateException {

            if (chain.length > 0) {
                String name = chain[0].getSubjectDN().toString();
                for (String bs: VideLibriHttpClient.BrokenServers)
                    if (name.startsWith(bs)) {
                        //Only check the first certificate in the chain for the libraries we have the certificates in the keystore
                        //Workaround for big trouble :
                        // -  Checking the full chain with the original and our TM fails to validate the libraries with broken https
                        //    (even if all certificates of the chain were included in the keystore)
                        // -  Checking only the first certificate with the original and our TM does not make any sense
                        // -  Checking the full chain with the original TM and then checking the first certificate with our TM
                        //    fails with java.lang.RuntimeException: java.lang.RuntimeException: error:0407006A:rsa routines:RSA_padding_check_PKCS1_type_1:block type is not 01 (SHA-1)

                        chain = new X509Certificate[]{ chain[0] };
                    }
            }


            for (X509TrustManager tm: cachedAdditionalTrustManagers)
                try {
                    tm.checkServerTrusted(chain, authType);
                    return;
                } catch (CertificateException e){ /*ignore*/ }

            for (int i=originalTrustManagers.size()-1;i>=0;i--)
                try {
                    originalTrustManagers.get(i).checkServerTrusted(chain, authType);
                    //cachedAdditionalTrustManagers.add(originalTrustManagers.get(i));
                    //originalTrustManagers.remove(i);
                    return;
                } catch (CertificateException e){ /*ignore*/ }
                catch (RuntimeException re) { /* ignore */ }

            //only load additional keystores when the system ones failed
            if (additionalKeystores != null) {
                loadKeystore(additionalKeystores.getStore(), additionalTrustManagers);
                   additionalKeystores = null;
            }

            for (int i=additionalTrustManagers.size()-1;i>=0;i--)
                try {
                    additionalTrustManagers.get(i).checkServerTrusted(chain, authType);

                    issuers.addAll(Arrays.asList(additionalTrustManagers.get(i).getAcceptedIssuers()));
                    issuersArray = issuers.toArray(new X509Certificate[issuers.size()]);
                    cachedAdditionalTrustManagers.add(additionalTrustManagers.get(i));
                    additionalTrustManagers.remove(i);
                    return;
                } catch (CertificateException e) {
                    //ignore
                }

            throw new CertificateException("Ungültiges Serverzertifikat.");
        }

        public X509Certificate[] getAcceptedIssuers() {
            return issuersArray;
        }
    }

}



//also based on http://blog.antoine.li/2010/10/22/android-trusting-ssl-certificates/
public class VideLibriHttpClient extends DefaultHttpClient {
     public VideLibriHttpClient(){
        super();
        setCookieStore(cookies);
    }

    @Override
    protected ClientConnectionManager createClientConnectionManager() {
        SchemeRegistry registry = new SchemeRegistry();
        registry.register(new Scheme("http", PlainSocketFactory.getSocketFactory(), 80));
        // Register for port 443 our SSLSocketFactory with our keystore
        // to the ConnectionManager
        registry.register(new Scheme("https", createAdditionalCertsSSLSocketFactory(), 443));
        return new SingleClientConnManager(getParams(), registry);
    }
    static private SocketFactory createAdditionalCertsSSLSocketFactory() {
        try {
            return new SSLSocketFactoryWithAdditionalLazyKeyStore(new LazyLoadKeystore(){
                @Override
                protected KeyStore loadStore() {
                    final KeyStore ks;
                    try {
                        ks = KeyStore.getInstance("BKS");
                        final InputStream in = VideLibriApp.instance.getResources().openRawResource( R.raw.keystore);
                        try {
                            ks.load(in, ( "password" ).toCharArray());
                        } finally {
                            in.close();
                        }
                    } catch( Exception e ) {
                        throw new RuntimeException(e);
                    }
                    return ks;
                }});
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    static org.apache.http.client.CookieStore cookies = (new org.apache.http.client.CookieStore(){


        @Override
        public void addCookie(Cookie cookie) {

        }

        @Override
        public List<Cookie> getCookies() {
            return new ArrayList<Cookie>();
        }

        @Override
        public boolean clearExpired(Date date) {
            return false;
        }

        @Override
        public void clear() {

        }
    });


    static String[] BrokenServers = new String[0];

}
