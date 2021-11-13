package de.benibela.videlibri.internet

import android.os.Build
import de.benibela.internettools.LazyLoadKeystore
import de.benibela.videlibri.R
import de.benibela.videlibri.VideLibriApp.Companion.staticApplicationContext
import java.security.KeyStore

class VideLibriKeyStore : LazyLoadKeystore() {
    override fun loadStore(): KeyStore = run {
        val password = "psswrd" // length must be at most 7 ??: https://stackoverflow.com/a/12528853
        val keystoreId = if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.JELLY_BEAN_MR1) R.raw.keystore else R.raw.keystoreold //https://stackoverflow.com/a/33197845
        KeyStore.getInstance("BKS").also { ks ->
            staticApplicationContext?.resources?.openRawResource(keystoreId)?.use {
                ks.load(it, password.toCharArray())
            }
        }
    }
}