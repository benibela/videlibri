package de.benibela.videlibri.internet;

import android.content.Context;
import android.net.ConnectivityManager;
import android.net.Network;
import android.os.Build;

public class VideLibriNetworkInfo {
    public static boolean isNetworkConnected(Context context){
        try {
            ConnectivityManager cmanager = (ConnectivityManager) context.getSystemService(Context.CONNECTIVITY_SERVICE);
            if (cmanager == null) return false;
            {
                android.net.NetworkInfo networkInfo = cmanager.getActiveNetworkInfo();
                if (networkInfo != null && networkInfo.isAvailable() && networkInfo.isConnected())
                    return true;
            }
            if (Build.VERSION.SDK_INT < 23) {
                android.net.NetworkInfo networkInfos[] = cmanager.getAllNetworkInfo();
                for (android.net.NetworkInfo networkInfo: networkInfos) {
                    if (networkInfo != null && networkInfo.isAvailable() && networkInfo.isConnectedOrConnecting())
                        return true;
                }
            }
            if (Build.VERSION.SDK_INT >= 21){
                Network[] networks = cmanager.getAllNetworks();
                for (Network network: networks) {
                    android.net.NetworkInfo info = cmanager.getNetworkInfo(network);
                    if (info != null && info.isAvailable() && info.isConnectedOrConnecting())
                        return true;
                }
            }
        }catch (Exception e) { return false; } //there are a few reports about networkinfo crashing on stackoverflow
        return false;
    }
}
