package de.benibela.videlibri.internet;

import android.os.Build;

import java.io.InputStream;
import java.security.KeyStore;

import de.benibela.internettools.LazyLoadKeystore;
import de.benibela.videlibri.R;
import de.benibela.videlibri.VideLibriApp;

public class VideLibriKeyStore extends LazyLoadKeystore {
    @Override
    protected KeyStore loadStore() {
        final String password = "psswrd"; // length must be at most 7 ??: https://stackoverflow.com/a/12528853
        final KeyStore ks;
        try {
            ks = KeyStore.getInstance("BKS");
            int keystore = Build.VERSION.SDK_INT >= Build.VERSION_CODES.JELLY_BEAN_MR1 ? R.raw.keystore : R.raw.keystoreold; //https://stackoverflow.com/a/33197845
            InputStream in = VideLibriApp.instance.getResources().openRawResource( keystore );
            try {
                ks.load(in, ( password ).toCharArray());
            } finally {
                in.close();
            }
        } catch( Exception e ) {
            throw new RuntimeException(e);
        }
        return ks;
    }




}
