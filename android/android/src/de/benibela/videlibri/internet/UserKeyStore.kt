package de.benibela.videlibri.internet

import android.content.SharedPreferences
import android.util.Base64
import de.benibela.internettools.X509TrustManagerWrapper.CustomTrustManagerFactory
import java.security.MessageDigest
import java.security.cert.CertificateException
import java.security.cert.X509Certificate
import java.util.*
import javax.net.ssl.X509TrustManager

object UserKeyStore {
    var certificates //can't use a set, since equals/hashCode are not defined properly for arrays
            : ArrayList<ByteArray>? = null
        private set

    @Synchronized
    fun setFromSerialization(certs: String?) {
        if (certs == null || "" == certs) {
            certificates = null
            return
        }
        val nc = ArrayList<ByteArray>()
        for (s in certs.split("\\|").toTypedArray()) nc.add(Base64.decode(s, Base64.DEFAULT))
        certificates = nc
    }

    @Synchronized
    private fun serialize(): String {
        if (certificates == null) return ""
        val result = StringBuilder()
        var first = true
        for (a in certificates!!) {
            if (!first) result.append('|')
            first = false
            result.append(Base64.encodeToString(a, Base64.DEFAULT))
        }
        return result.toString()
    }

    private fun certificateInList(list: ArrayList<ByteArray>?, cert: ByteArray): Boolean {
        if (list == null) return false
        for (b in list) if (Arrays.equals(cert, b)) return true
        return false
    }

    @Synchronized
    fun addUserCertificate(encoded: ByteArray): Boolean {
        if (certificateInList(certificates, encoded)) return false
        if (certificates == null) certificates = ArrayList()
        certificates!!.add(encoded)
        return true
    }

    @Synchronized
    fun removeUserCertificate(cert: ByteArray?) {
        if (!hasCertificates()) return
        val nc = ArrayList<ByteArray>()
        for (b in certificates!!) if (!Arrays.equals(cert, b)) nc.add(b)
        certificates = nc
    }

    fun storeUserCertificates(preferences: SharedPreferences) {
        val editor = preferences.edit()
        if (!hasCertificates()) editor.remove("additionalCertificatesBase64") else editor.putString("additionalCertificatesBase64", serialize())
        editor.apply()
    }

    fun loadUserCertificates(preferences: SharedPreferences) {
        if (preferences.contains("additionalCertificatesBase64")) setFromSerialization(preferences.getString("additionalCertificatesBase64", null))
    }

    fun hasCertificates(): Boolean {
        return certificates != null && !certificates!!.isEmpty()
    }

    @Synchronized
    fun hasCertificate(cert: ByteArray): Boolean {
        return certificateInList(certificates, cert)
    }

    fun makeFactory(): CustomTrustManagerFactory {
        val tms = ArrayList<X509TrustManager>()
        tms.add(object : X509TrustManager {
            @Throws(CertificateException::class)
            override fun checkClientTrusted(chain: Array<X509Certificate>, authType: String) {
                checkServerTrusted(chain, authType)
            }

            @Throws(CertificateException::class)
            override fun checkServerTrusted(chain: Array<X509Certificate>, authType: String) {
                if (chain.size > 0) {
                    if (hasCertificate(chain[0].encoded)) return
                }
                throw CertificateException("Unknown certificate")
            }

            override fun getAcceptedIssuers(): Array<X509Certificate> {
                return arrayOfNulls(0)
            }
        })
        return CustomTrustManagerFactory { tms }
    }

    fun getFingerprint(cert: ByteArray?): String {
        var result: String
        //if (fingerprints == null) fingerprints = new HashMap<>();
        //String result = fingerprints.get(cert);
        //if (result == null)
        try {
            //https://stackoverflow.com/questions/5980658/how-to-sha1-hash-a-string-in-android
            val messageDigest = MessageDigest.getInstance("SHA-1")
            messageDigest.update(cert)
            val bytes = messageDigest.digest()
            val buffer = StringBuilder()
            var first = true
            for (b in bytes) {
                if (!first) buffer.append(':')
                first = false
                buffer.append(Integer.toString((b and 0xff) + 0x100, 16).substring(1))
            }
            result = buffer.toString()
            //fingerprints.put(cert, result);
        } catch (ignored: Exception) {
            ignored.printStackTrace()
            result = "missing fingerprint"
        }
        return result
    }
}