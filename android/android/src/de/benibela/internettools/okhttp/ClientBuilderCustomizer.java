package de.benibela.internettools.okhttp;

import java.security.KeyManagementException;
import java.security.KeyStoreException;
import java.security.NoSuchAlgorithmException;
import java.security.UnrecoverableKeyException;
import java.util.ArrayList;
import java.util.concurrent.TimeUnit;

import javax.net.ssl.X509TrustManager;

import de.benibela.internettools.ModernSSLSocketFactory;
import de.benibela.internettools.X509TrustManagerWithAdditionalKeystores;
import okhttp3.ConnectionSpec;
import okhttp3.OkHttpClient;

public class ClientBuilderCustomizer {
    public static void customizeWithTrustManager(OkHttpClient.Builder builder, X509TrustManager manager) throws KeyManagementException, NoSuchAlgorithmException {
        builder.connectTimeout(5, TimeUnit.MINUTES);
        builder.readTimeout(5, TimeUnit.MINUTES);
        builder.writeTimeout(5, TimeUnit.MINUTES);
        ArrayList<ConnectionSpec> temp = new ArrayList<>();
        temp.add(new ConnectionSpec.Builder(ConnectionSpec.MODERN_TLS).allEnabledCipherSuites().allEnabledTlsVersions().build());
        temp.add(ConnectionSpec.CLEARTEXT);
        temp.add(ConnectionSpec.MODERN_TLS);
        temp.add(ConnectionSpec.COMPATIBLE_TLS);
        builder.connectionSpecs(temp);
        builder.sslSocketFactory(new ModernSSLSocketFactory(manager), manager);
    }

    public static void customize(OkHttpClient.Builder builder) throws UnrecoverableKeyException, NoSuchAlgorithmException, KeyStoreException, KeyManagementException {
        customizeWithTrustManager(builder, new X509TrustManagerWithAdditionalKeystores());
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
