package de.benibela.internettools.okhttp;

import android.util.Log;

import java.io.IOException;
import java.security.KeyManagementException;
import java.security.KeyStoreException;
import java.security.NoSuchAlgorithmException;
import java.security.UnrecoverableKeyException;

import javax.net.ssl.X509TrustManager;

import de.benibela.internettools.ModernSSLSocketFactory;
import de.benibela.internettools.X509TrustManagerWithAdditionalKeystores;
import okhttp3.Interceptor;
import okhttp3.OkHttpClient;
import okhttp3.Request;
import okhttp3.Response;

public class ClientBuilderCustomizer {
    static void customize(OkHttpClient.Builder builder) throws UnrecoverableKeyException, NoSuchAlgorithmException, KeyStoreException, KeyManagementException {
        X509TrustManager tm = new X509TrustManagerWithAdditionalKeystores();
        builder.sslSocketFactory(new ModernSSLSocketFactory(tm), tm);
        /*builder.addInterceptor(new Interceptor() {
            @Override
            public Response intercept(Chain chain) throws IOException {
                Request r = chain.request();
                Log.i("VideLibri App Headers", r.headers().toString());
                if (r.body()!=null)
                Log.i("VL App Body Mediat", r.body().contentType().toString());
                return chain.proceed(r);
            }
        });
        builder.addNetworkInterceptor(new Interceptor() {
            @Override
            public Response intercept(Chain chain) throws IOException {
                Request r = chain.request();
                Log.i("VideLib Network Headers", r.headers().toString());
                if (r.body()!=null)
                Log.i("VL Body N Mediat", r.body().contentType().toString());
                return chain.proceed(r);
            }
        });*/
    }
}
