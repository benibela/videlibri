package de.benibela.videlibri.internet

import android.os.Build
import de.benibela.internettools.LazyLoadKeystore
import de.benibela.videlibri.R
import de.benibela.videlibri.VideLibriApp.Companion.staticApplicationContext
import java.security.KeyStore

class VideLibriKeyStore : LazyLoadKeystore() {
    override fun loadStore(): KeyStore {
        val password = "psswrd" // length must be at most 7 ??: https://stackoverflow.com/a/12528853
        val ks: KeyStore
        try {
            ks = KeyStore.getInstance("BKS")
            val keystore = if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.JELLY_BEAN_MR1) R.raw.keystore else R.raw.keystoreold //https://stackoverflow.com/a/33197845
            val `in` = staticApplicationContext!!.resources.openRawResource(keystore)
            try {
                ks.load(`in`, password.toCharArray())
            } finally {
                `in`.close()
            }
        } catch (e: Exception) {
            throw RuntimeException(e)
        }
        return ks
    }
}