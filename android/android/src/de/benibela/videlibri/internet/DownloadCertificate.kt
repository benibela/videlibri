package de.benibela.videlibri.internet

import android.app.Activity
import android.preference.PreferenceManager
import de.benibela.internettools.X509TrustManagerWrapper
import de.benibela.internettools.okhttp.ClientBuilderCustomizer
import de.benibela.videlibri.R
import de.benibela.videlibri.VideLibriApp.Companion.currentContext
import de.benibela.videlibri.activities.Options
import de.benibela.videlibri.utils.Util
import de.benibela.videlibri.utils.showMessage
import okhttp3.OkHttpClient
import okhttp3.Request
import java.io.IOException
import java.security.GeneralSecurityException
import java.security.cert.X509Certificate

class DownloadCertificate(private val server: String) : Runnable {
    private var downloadedCertificate: X509Certificate? = null

    private inner class AllAcceptingX509TrustManager : X509TrustManagerWrapper() {
        override fun isCheckServerTrusted(chain: Array<X509Certificate>, authType: String): Boolean {
            if (chain.size == 0) return false
            downloadedCertificate = chain[0]
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
        var message = "Error:$server"
        try {
            val b = OkHttpClient.Builder()
            b.followRedirects(false)
            ClientBuilderCustomizer.customizeWithTrustManager(b, AllAcceptingX509TrustManager())
            val client = b.build()
            val r = Request.Builder().url("https://$server").build()
            val response = client.newCall(r).execute()
            response.close()
            if (downloadedCertificate != null) {
                val encoded = downloadedCertificate!!.encoded
                val added = UserKeyStore.addUserCertificate(encoded)
                message = Util.tr(if (added) R.string.certificate_added else R.string.certificate_existing, server, UserKeyStore.getFingerprint(encoded))
            }
        } catch (e: IOException) {
            message = Util.tr(R.string.certificate_failed, server, e.localizedMessage)
        } catch (e: GeneralSecurityException) {
            message = Util.tr(R.string.certificate_failed, server, e.localizedMessage)
        } catch (e: IllegalArgumentException) { //for invalid urls
            message = Util.tr(R.string.certificate_failed, server, e.localizedMessage)
        }
        val fmessage = message
        val context = currentContext()
        if (context is Activity) {
            context.runOnUiThread {
                UserKeyStore.storeUserCertificates(PreferenceManager.getDefaultSharedPreferences(context))
                showMessage(fmessage)
                if (context is Options) context.updatePreferences()
            }
        }
    }

}