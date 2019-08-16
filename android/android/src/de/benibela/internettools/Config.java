package de.benibela.internettools;

public class Config {
    public static X509TrustManagerWithAdditionalKeystores.LazyLoadKeyStoreFactory defaultKeystoreFactory;
    public static X509TrustManagerWrapper.CustomTrustManagerFactory defaultCustomTrustManagerFactory;


    public static String invalidCerticateMessage = "Invalid certificate: %s";
}
