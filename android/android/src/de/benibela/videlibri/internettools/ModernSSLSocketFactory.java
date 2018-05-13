package de.benibela.videlibri.internettools;

import android.os.Build;
import android.util.Log;

import java.io.IOException;
import java.lang.reflect.Method;
import java.net.Socket;
import java.security.KeyManagementException;
import java.security.KeyStoreException;
import java.security.NoSuchAlgorithmException;
import java.security.UnrecoverableKeyException;
import java.util.ArrayList;
import java.util.Arrays;

import javax.net.ssl.SSLContext;
import javax.net.ssl.SSLSocket;
import javax.net.ssl.TrustManager;

public class ModernSSLSocketFactory {
    protected SSLContext sslContext = SSLContext.getInstance("TLS");

    public ModernSSLSocketFactory (LazyLoadKeystore keyStore) throws NoSuchAlgorithmException, KeyManagementException, KeyStoreException, UnrecoverableKeyException {
        sslContext.init(null, new TrustManager[]{new X509TrustManagerWithAdditionalKeystores(keyStore)}, null);
    }

    public Socket createSocket(Socket socket, String host, int port, boolean autoClose) throws IOException {
        return setSocketHostName(modernize(sslContext.getSocketFactory().createSocket(socket, host, port, autoClose)), host);
    }

    public Socket createSocket() throws IOException {
        return modernize(sslContext.getSocketFactory().createSocket());
    }


    //https://stackoverflow.com/questions/29249630/android-enable-tlsv1-2-in-okhttp
    private Socket modernize(Socket somesocket) {
        if(somesocket != null && (somesocket instanceof SSLSocket)) {
            //Activate all protocols
            //todo: disable SSL? but need to make sure no library uses that
            SSLSocket socket = (SSLSocket)somesocket;
            socket.setEnabledProtocols(socket.getSupportedProtocols());

            //Activate all ciphers
            socket.setEnabledCipherSuites(socket.getSupportedCipherSuites());
        }
        return somesocket;
    }

    public Socket setSocketHostName(Socket somesocket, String host){
        if (somesocket instanceof SSLSocket) {
            //Activate SNI
            //see https://stackoverflow.com/questions/35782882/how-do-we-enable-sni-in-httpclient-4-4-on-android
            //this is required for Stb Gelsenkirchen with Apache HTTPClient on my smartbook table. todo: check if this is still needed for newer http libraries
            SSLSocket socket = (SSLSocket)somesocket;
            try {
                //if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.JELLY_BEAN_MR1) {
                Method method = socket.getClass().getMethod("setHostname", String.class);
                if (method != null)
                    method.invoke(socket, host);
            } catch (Exception ex) {
                Log.d("videlibri", "SNI configuration failed", ex);
            }

        }
        return somesocket;
    }
}
