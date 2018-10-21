package de.benibela.videlibri.internet;

import android.content.SharedPreferences;
import android.util.Base64;

import java.security.MessageDigest;
import java.security.cert.CertificateException;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.Arrays;

import javax.net.ssl.X509TrustManager;

import de.benibela.internettools.X509TrustManagerWrapper;
import de.benibela.videlibri.Util;


@SuppressWarnings("WeakerAccess")
public class UserKeyStore {
    static private ArrayList<byte[]> certificates; //can't use a set, since equals/hashCode are not defined properly for arrays

    static public synchronized void setFromSerialization(String certs){
        if (Util.isEmptyString(certs)) {
            certificates = null;
            return;
        }
        ArrayList<byte[]> nc = new ArrayList<>();
        for (String s: certs.split("\\|"))
            nc.add(Base64.decode(s, Base64.DEFAULT));
        certificates = nc;
    }

    static private synchronized String serialize(){
        if (certificates == null) return "";
        StringBuilder result = new StringBuilder();
        boolean first = true;
        for (byte[] a: certificates) {
            if (!first) result.append('|');
            first = false;
            result.append(Base64.encodeToString(a, Base64.DEFAULT));
        }
        return result.toString();
    }

    private static boolean certificateInList(ArrayList<byte[]> list, byte[] cert){
        if (list == null) return false;
        for (byte[] b: list)
            if (Arrays.equals(cert, b))
                return true;
        return false;
    }

    public static synchronized boolean addUserCertificate(byte[] encoded) {
        if (certificateInList(certificates, encoded)) return false;
        if (certificates == null) certificates = new ArrayList<>();
        certificates.add(encoded);
        return true;
    }

    public static synchronized void removeUserCertificate(byte[] cert) {
        if (!hasCertificates()) return;
        ArrayList<byte[]> nc = new ArrayList<>();
        for (byte[] b: certificates)
            if (!Arrays.equals(cert, b))
                nc.add(b);
        certificates = nc;
    }
    public static void storeUserCertificates(SharedPreferences preferences){
        SharedPreferences.Editor editor = preferences.edit();
        if (!hasCertificates())
            editor.remove("additionalCertificatesBase64");
        else
            editor.putString("additionalCertificatesBase64", serialize());
        editor.apply();
    }
    public static void loadUserCertificates(SharedPreferences preferences){
        if (preferences.contains("additionalCertificatesBase64"))
            setFromSerialization(preferences.getString("additionalCertificatesBase64", null));
    }

    public static boolean hasCertificates(){
        return certificates != null && !certificates.isEmpty();
    }

    public static synchronized boolean hasCertificate(byte[] cert){
        return certificateInList(certificates, cert);
    }

    public static ArrayList<byte[]> getCertificates(){
        return certificates;
    }

    public static X509TrustManagerWrapper.CustomTrustManagerFactory makeFactory() {
        final ArrayList<X509TrustManager> tms = new ArrayList<>();
        tms.add(new X509TrustManager(){
            @Override
            public void checkClientTrusted(X509Certificate[] chain, String authType) throws CertificateException {
                checkServerTrusted(chain, authType);
            }

            @Override
            public void checkServerTrusted(X509Certificate[] chain, String authType) throws CertificateException {
                if (chain.length > 0) {
                    if (hasCertificate(chain[0].getEncoded()))
                        return;
                }
                throw new CertificateException("Unknown certificate");
            }

            @Override
            public X509Certificate[] getAcceptedIssuers() {
                return new X509Certificate[0];
            }
        });
        return new X509TrustManagerWrapper.CustomTrustManagerFactory() {
            @Override
            public ArrayList<X509TrustManager> getTrustManagers() {
                return tms;
            }
        };
    }

    static public String getFingerprint(byte[] cert){
        String result;
        //if (fingerprints == null) fingerprints = new HashMap<>();
        //String result = fingerprints.get(cert);
        //if (result == null)
        try {
            //https://stackoverflow.com/questions/5980658/how-to-sha1-hash-a-string-in-android
            MessageDigest messageDigest = MessageDigest.getInstance("SHA-1");
            messageDigest.update(cert);
            byte[] bytes = messageDigest.digest();
            StringBuilder buffer = new StringBuilder();
            boolean first = true;
            for (byte b : bytes) {
                if (!first) buffer.append(':');
                first = false;
                buffer.append(Integer.toString((b & 0xff) + 0x100, 16).substring(1));
            }
            result = buffer.toString();
            //fingerprints.put(cert, result);
        } catch (Exception ignored) {
            ignored.printStackTrace();
            result = "missing fingerprint";
        }
        return result;
    }

}
