package de.benibela.videlibri;

import java.security.KeyManagementException;
import java.security.KeyStoreException;
import java.security.NoSuchAlgorithmException;
import java.security.UnrecoverableKeyException;

import javax.net.ssl.X509TrustManager;

import de.benibela.videlibri.internettools.ModernSSLSocketFactory;
import de.benibela.videlibri.internettools.X509TrustManagerWithAdditionalKeystores;
import okhttp3.OkHttpClient;

public class VideLibriOkHttpClientBuilder {
    static void customize(OkHttpClient.Builder builder) throws UnrecoverableKeyException, NoSuchAlgorithmException, KeyStoreException, KeyManagementException {
        X509TrustManager tm = new X509TrustManagerWithAdditionalKeystores(new VideLibriKeyStore());
        builder.sslSocketFactory(new ModernSSLSocketFactory(tm), tm);
    }
}
