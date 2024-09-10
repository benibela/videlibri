package de.benibela.videlibri.jni

import android.content.Context
import android.util.Log
import com.getkeepsafe.relinker.ReLinker

var initializedJNI = false
fun initializeJNI(context: Context) {
    if (initializedJNI) return
    initializedJNI = true
    Log.i("Videlibri", "Trying to load liblclapp.so")
    try {
        System.loadLibrary("lclapp")
    } catch (e: UnsatisfiedLinkError) {
        Log.i("Videlibri", "Android is broken, trying Relinker.")
        ReLinker.loadLibrary(context, "lclapp")
    }
    Log.i("Videlibri", "Initializing Windows VM and Pascal layer")
    Bridge.VLInit(context)
    //throw new Error("test");
}
