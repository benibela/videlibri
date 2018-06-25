package de.benibela.internettools;

import android.util.Log;

import java.io.IOException;
import java.lang.reflect.Method;
import java.net.InetAddress;
import java.net.Socket;
import java.net.UnknownHostException;
import java.security.KeyManagementException;
import java.security.KeyStoreException;
import java.security.NoSuchAlgorithmException;
import java.security.UnrecoverableKeyException;

import javax.net.ssl.SSLContext;
import javax.net.ssl.SSLSocket;
import javax.net.ssl.SSLSocketFactory;
import javax.net.ssl.TrustManager;
import javax.net.ssl.X509TrustManager;

public class ModernSSLSocketFactory extends SSLSocketFactory {
    private SSLContext sslContext = SSLContext.getInstance("TLS");
    private SSLSocketFactory baseFactory(){
        return sslContext.getSocketFactory();
    }


    public ModernSSLSocketFactory () throws NoSuchAlgorithmException, KeyManagementException, KeyStoreException, UnrecoverableKeyException {
        sslContext.init(null, new TrustManager[]{new X509TrustManagerWithAdditionalKeystores()}, null);
    }
    public ModernSSLSocketFactory (LazyLoadKeystore keyStore) throws NoSuchAlgorithmException, KeyManagementException, KeyStoreException, UnrecoverableKeyException {
        sslContext.init(null, new TrustManager[]{new X509TrustManagerWithAdditionalKeystores(keyStore)}, null);
    }
    public ModernSSLSocketFactory (X509TrustManager trustManager) throws NoSuchAlgorithmException, KeyManagementException, KeyStoreException, UnrecoverableKeyException {
        sslContext.init(null, new TrustManager[]{trustManager}, null);
    }

    @Override
    public String[] getDefaultCipherSuites() {
        return baseFactory().getDefaultCipherSuites();
    }

    @Override
    public String[] getSupportedCipherSuites() {
        return baseFactory().getSupportedCipherSuites();
    }

    @Override
    public Socket createSocket(Socket socket, String host, int port, boolean autoClose) throws IOException {
        return modernize(baseFactory().createSocket(socket, host, port, autoClose), host);
    }

    @Override
    public Socket createSocket() throws IOException {
        return modernize(baseFactory().createSocket());
    }

    @Override
    public Socket createSocket(String host, int port) throws IOException, UnknownHostException {
        return modernize(baseFactory().createSocket(host, port), host);
    }

    @Override
    public Socket createSocket(String host, int port, InetAddress localHost, int localPort) throws IOException, UnknownHostException {
        return modernize(baseFactory().createSocket(host, port, localHost, localPort), host);
    }

    @Override
    public Socket createSocket(InetAddress host, int port) throws IOException {
        return modernize(baseFactory().createSocket(host, port), host.getHostName());
    }

    @Override
    public Socket createSocket(InetAddress address, int port, InetAddress localAddress, int localPort) throws IOException {
        return modernize(baseFactory().createSocket(address, port, localAddress, localPort), address.getHostName());
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

    private Socket modernize(Socket somesocket, String host) {
        return setSocketHostName(modernize(somesocket), host);
    }

    private Socket setSocketHostName(Socket somesocket, String host){
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
