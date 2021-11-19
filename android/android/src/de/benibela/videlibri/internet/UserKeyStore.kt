package de.benibela.videlibri.internet

import android.util.Base64
import de.benibela.internettools.X509TrustManagerWrapper.CustomTrustManagerFactory
import java.security.MessageDigest
import java.security.cert.CertificateException
import java.security.cert.X509Certificate
import javax.net.ssl.X509TrustManager

object UserKeyStore {
    var certificates = mutableSetOf<ByteArray>()
        private set

    @Synchronized
    fun toArray(): Array<String> =
        certificates.map { Base64.encodeToString(it, Base64.DEFAULT) }.toTypedArray()

    @Synchronized
    fun addUserCertificate(encoded: ByteArray): Boolean = certificates.add(encoded)

    @Synchronized
    fun removeUserCertificate(cert: ByteArray?) = certificates.remove(cert)


    @Synchronized
    fun loadUserCertificates(certs: Array<String>) {
        certificates = certs.map { Base64.decode(it, Base64.DEFAULT) }.toMutableSet()
    }

    @Synchronized
    fun hasCertificates(): Boolean = certificates.isNotEmpty()

    @Synchronized
    fun hasCertificate(cert: ByteArray): Boolean = certificates.contains(cert)



    fun makeFactory(): CustomTrustManagerFactory =  CustomTrustManagerFactory { arrayListOf(
        object : X509TrustManager {
            @Throws(CertificateException::class)
            override fun checkClientTrusted(chain: Array<X509Certificate>?, authType: String?) =
                checkServerTrusted(chain, authType)

            @Throws(CertificateException::class)
            override fun checkServerTrusted(chain: Array<X509Certificate>?, authType: String?) {
                if (chain?.getOrNull(0)?.let { hasCertificate(it.encoded) } ?: false)
                    return
                throw CertificateException("Unknown certificate")
            }

            override fun getAcceptedIssuers(): Array<X509Certificate> = arrayOf()
        }
    )}

    fun getFingerprint(cert: ByteArray): String =
        try {
            MessageDigest.getInstance("SHA-1").digest(cert).joinToString(":") { "%02x".format(it) }
            //fingerprints.put(cert, result);
        } catch (ignored: Exception) {
            ignored.printStackTrace()
            "missing fingerprint"
        }
}