package de.benibela.internettools.apache;

import org.apache.http.conn.ClientConnectionManager;
import org.apache.http.conn.scheme.PlainSocketFactory;
import org.apache.http.conn.scheme.Scheme;
import org.apache.http.conn.scheme.SchemeRegistry;
import org.apache.http.conn.scheme.SocketFactory;
import org.apache.http.conn.ssl.SSLSocketFactory;
import org.apache.http.cookie.Cookie;
import org.apache.http.impl.client.DefaultHttpClient;
import org.apache.http.impl.conn.SingleClientConnManager;

import java.io.IOException;
import java.net.Socket;
import java.security.KeyManagementException;
import java.security.KeyStoreException;
import java.security.NoSuchAlgorithmException;
import java.security.UnrecoverableKeyException;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import de.benibela.internettools.LazyLoadKeystore;
import de.benibela.internettools.ModernSSLSocketFactory;

//based on http://stackoverflow.com/questions/2642777/trusting-all-certificates-using-httpclient-over-https

class SSLSocketFactoryWithAdditionalLazyKeyStore extends SSLSocketFactory {
    private ModernSSLSocketFactory modernSSLSocketFactory;

    public SSLSocketFactoryWithAdditionalLazyKeyStore() throws NoSuchAlgorithmException, KeyManagementException, KeyStoreException, UnrecoverableKeyException {
        super(null, null, null, null, null, null);
        modernSSLSocketFactory = new ModernSSLSocketFactory();
    }

    public SSLSocketFactoryWithAdditionalLazyKeyStore(LazyLoadKeystore keyStore) throws NoSuchAlgorithmException, KeyManagementException, KeyStoreException, UnrecoverableKeyException {
        super(null, null, null, null, null, null);
        modernSSLSocketFactory = new ModernSSLSocketFactory(keyStore);
    }

    @Override
    public Socket createSocket(Socket socket, String host, int port, boolean autoClose) throws IOException {
        return modernSSLSocketFactory.createSocket(socket, host, port, autoClose);
    }

    @Override
    public Socket createSocket() throws IOException {
        return modernSSLSocketFactory.createSocket();
    }

}



//also based on http://blog.antoine.li/2010/10/22/android-trusting-ssl-certificates/
public class ModernHttpClient extends DefaultHttpClient {
     public ModernHttpClient(){
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
            return new SSLSocketFactoryWithAdditionalLazyKeyStore();
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    static org.apache.http.client.CookieStore cookies = (new org.apache.http.client.CookieStore(){
        @Override
        public void addCookie(Cookie cookie) {}
        @Override
        public List<Cookie> getCookies() {
            return new ArrayList<Cookie>();
        }
        @Override
        public boolean clearExpired(Date date) {
            return false;
        }
        @Override
        public void clear() { }
    });

}
