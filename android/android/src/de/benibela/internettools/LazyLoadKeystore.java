package de.benibela.internettools;

import java.security.KeyStore;

public class LazyLoadKeystore {
    private KeyStore store;
    public KeyStore getStore(){
        if (store == null) store = loadStore();
        return store;
    }
    protected KeyStore loadStore(){
        return null;
    }
}
