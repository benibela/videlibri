package de.benibela.videlibri.internet

import android.app.Activity
import android.preference.PreferenceManager
import de.benibela.internettools.X509TrustManagerWrapper
import de.benibela.internettools.okhttp.ClientBuilderCustomizer
import de.benibela.videlibri.R
import de.benibela.videlibri.activities.Options
import de.benibela.videlibri.utils.currentActivity
import de.benibela.videlibri.utils.getString
import de.benibela.videlibri.utils.showMessage
import okhttp3.OkHttpClient
import okhttp3.Request
import java.io.IOException
import java.security.GeneralSecurityException
import java.security.cert.X509Certificate

class DownloadCertificate(private val server: String) : Runnable {
    private var downloadedCertificate: X509Certificate? = null

    private inner class AllAcceptingX509TrustManager : X509TrustManagerWrapper() {
        override fun isCheckServerTrusted(chain: Array<X509Certificate>?, authType: String?): Boolean {
            downloadedCertificate = chain?.getOrNull(0)
            /*for (int i=0;i<chain.length;i++) {
                byte[] enc=null;
                try {
                    enc = chain[i].getEncoded();
                } catch (CertificateEncodingException e) {
                    e.printStackTrace();
                }
                Log.i("VL"+i, VideLibriKeyStore.getFingerprint(enc) +": "+chain[i].getSubjectDN().toString());
            }*/return true
        }
    }

    override fun run() {
        val message =
        try {
            val client = OkHttpClient.Builder().also { b ->
                b.followRedirects(false)
                ClientBuilderCustomizer.customizeWithTrustManager(b, AllAcceptingX509TrustManager())
            }.build()
            val r = Request.Builder().url("https://$server").build()
            client.newCall(r).execute().close()
            downloadedCertificate?.encoded?.let {
                val added = UserKeyStore.addUserCertificate(it)
                getString(if (added) R.string.certificate_added else R.string.certificate_existing, server, UserKeyStore.getFingerprint(it))
            } ?: "Error: $server"
        } catch (e: IOException) {
            getString(R.string.certificate_failed, server, e.localizedMessage)
        } catch (e: GeneralSecurityException) {
            getString(R.string.certificate_failed, server, e.localizedMessage)
        } catch (e: IllegalArgumentException) { //for invalid urls
            getString(R.string.certificate_failed, server, e.localizedMessage)
        }
        val context = currentActivity<Activity>() ?: return
        context.runOnUiThread {
            UserKeyStore.storeUserCertificates(PreferenceManager.getDefaultSharedPreferences(context))
            showMessage(message)
            if (context is Options) context.updatePreferences()
        }
    }
}