package com.benibela.videlibri;
import android.content.Context;
import android.content.Intent;
import android.content.res.AssetManager;
import java.io.InputStream;
import java.util.*;
import java.lang.*;
import android.app.*;
import android.os.Bundle;
import android.util.Log;


public class VideLibri extends  Activity{
 /* AssetManager assets;
  byte[] getDataFile(String fileName){//implemented with JNI on pascal side
    InputStream is = assets.open(fileName);
    
    ArrayList<byte[]> result = new ArrayList<byte[]>();
    int totalSize = 0;
    byte[] buffer = new byte[4096];
    int len = is.read(buffer);
    while (len >= 0) {
      totalSize += len;
      byte[] temp = new byte[len];
      System.arraycopy(buffer, 0, temp, 0, len);
      result.add(temp);
      len = is.read(buffer);
    }
    byte [] fresult = new byte[totalSize];
    int offset = 0;
    for (int i=0;i<result.size();i++) {
      System.arraycopy(result.get(i), 0, fresult, offset, result.get(i).length);
      offset += result.get(i).length;
    }*/
    
    /*
    faster way??
    AssetFileDescriptor afd = assets.openFd(fileName);
    int len = afd.getLength();
    byte[] result = new byte[len];
    int read;
    FileInputStream fis =  */
//  }
    static VideLibri instance;
    Bridge.Account accounts[];
    public VideLibri(){
    }

    //Bridge functions called from VideLibri-midend
    String userPath(){
        return getFilesDir().getAbsolutePath();
    }

    static void allThreadsDone(){
        if (instance == null) return;
        //Log.i("VideLibri", "allThreadsDone started");
        instance.runOnUiThread(new Runnable() {
            @Override
            public void run() {
                instance.displayAccount(null);
                runningUpdates.clear();

                Bridge.PendingException[] exceptions = Bridge.VLTakePendingExceptions();
                for (Bridge.PendingException ex : exceptions)
                    instance.showMessage(ex.accountPrettyNames + ": " + ex.error);
            }
        });

    }

    //Called from Android OS

    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        Log.i("VideLibri", "onCreate")               ;

        instance = this;

        Bridge.VLInit(this);

        accounts = Bridge.VLGetAccounts();
        if (accounts == null || accounts.length == 0) startActivity(new Intent(this, NewAccountWizard.class));
        else {
            displayAccount(null);
            for (Bridge.Account a: accounts) updateAccount(a, true, false);
        }
    }



    public void onDestroy(){
        super.onDestroy();
        instance = null;
        Bridge.VLFinalize();
    }


    //Mix

    static void addAccount(Bridge.Account acc){
        if (instance == null) return;
        Bridge.VLAddAccount(acc);
        instance.accounts = Bridge.VLGetAccounts();
        VideLibri.updateAccount(acc, false, false);
    }

    public void displayAccount(Bridge.Account acc){
        if (acc == null) {
            for (Bridge.Account facc: accounts) displayAccount(facc);
            return;
        }
        Bridge.Book[] books = Bridge.VLGetBooks(acc, false);
        showMessage(books.length+"");
        String temp = "";
        for (Bridge.Book b: books)
            showMessage(b.title + " von " + b.author + "\n");

    }

    public void showMessage(String message){ showMessage(this, message); }
    static public void showMessage(Context context, String message){
        AlertDialog.Builder builder = new AlertDialog.Builder(context);
        builder.setMessage(message);
        builder.setTitle("VideLibri");
        builder.setNegativeButton("OK", null);
        builder.show();
    }

    static List<Bridge.Account> runningUpdates = new ArrayList<Bridge.Account>();
    static public void updateAccount(Bridge.Account acc, final boolean autoUpdate, final boolean forceExtend){
        if (runningUpdates.contains(acc)) return;
        runningUpdates.add(acc);
        Bridge.VLUpdateAccount(acc, autoUpdate, forceExtend);
       /* final Bridge.Account facc = acc;
        Thread t = new Thread(new Runnable() {
            @Override
            public void run() {
                Bridge.VLUpdateAccount(facc, autoUpdate, forceExtend);
               // runningUpdates.remove(facc);
               // instance.displayAccount(facc);
            }
        });
        t.start();*/
    }

}
