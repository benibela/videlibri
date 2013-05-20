package de.benibela.videlibri;
import android.content.Context;
import android.content.Intent;
import android.content.res.AssetManager;
import java.io.InputStream;
import java.text.DateFormat;
import java.util.*;
import java.lang.*;
import android.app.*;
import android.graphics.Color;
import android.os.Bundle;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.*;


public class VideLibri extends  BookListActivity{
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
        super();
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

                instance.setLoading(false);

                Bridge.PendingException[] exceptions = Bridge.VLTakePendingExceptions();
                for (Bridge.PendingException ex : exceptions)
                    instance.showMessage(ex.accountPrettyNames + ": " + ex.error);

            }
        });

    }

    //Called from Android OS

    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
       // Log.i("VideLibri", "onCreate")               ;

        instance = this;

        VideLibriHttpClient.BrokenServers = getResources().getStringArray(R.array.broken_servers);

        Bridge.VLInit(this);

        accounts = Bridge.VLGetAccounts();
        if (accounts == null || accounts.length == 0) ; //startActivity(new Intent(this, NewAccountWizard.class));
        else {
            displayAccount(null);
            for (Bridge.Account a: accounts) updateAccount(a, true, false);
        }

    }

    @Override
    protected void onResume() {
        super.onResume();
        defaultColor = getResources().getColor(android.R.color.primary_text_dark);
        if (accounts == null || accounts.length == 0) newAccountDialog(true);
        setTitle("Ausleihen");  //does not work in onCreate (why? makes the title invisible) No. it just works sometimes?
    }

    public void onDestroy(){
        super.onDestroy();
        instance = null;
        Bridge.VLFinalize();
    }


    //Mix

    void newAccountDialog(boolean initial){
        Intent intent = new Intent(this, AccountInfo.class);
        intent.putExtra("mode", initial ? AccountInfo.MODE_ACCOUNT_CREATION_INITIAL : AccountInfo.MODE_ACCOUNT_CREATION) ;
        startActivity(intent);
    }

    static void newSearchActivity(){
        Intent intent = new Intent(instance, Search.class);
        if (instance.accounts.length > 0){
            //intent.putExtra("libId", instance.accounts[0].libId);
            Bridge.Library[] libraries = Bridge.getLibraries();
            for (Bridge.Library lib: libraries)
                if (lib.id.equals(instance.accounts[0].libId)){
                    lib.putInIntent(intent);
                    break;
                }
        }
        instance.startActivity(intent);
    }

    @Override
    protected void onActivityResult(int requestCode, int resultCode, Intent data) {

    }

    static void addAccount(Bridge.Account acc){
        if (instance == null) return;
        Bridge.VLAddAccount(acc);
        instance.accounts = Bridge.VLGetAccounts();
        VideLibri.updateAccount(acc, false, false);
    }

    public void displayAccount(Bridge.Account acc){
        if (acc == null) {
            bookCache = new ArrayList<Bridge.Book>();
            for (Bridge.Account facc: accounts) {
                Bridge.Book[] books = Bridge.VLGetBooks(facc, false);
                for (Bridge.Book b: books)
                    bookCache.add(b);
            }
        } else {
            ArrayList<Bridge.Book> oldBookCache = bookCache;
            bookCache = new ArrayList<Bridge.Book>();
            for (Bridge.Book b: oldBookCache)
                if (!acc.equals(b.account)) bookCache.add(b);
            Bridge.Book[] books = Bridge.VLGetBooks(acc, false);
            for (Bridge.Book b: books)
                bookCache.add(b);
        }

        Collections.sort(bookCache, new Comparator<Bridge.Book>() {
            @Override
            public int compare(Bridge.Book book, Bridge.Book book2) {
                if (book.history != book2.history) {
                    if (book.history) return  -1;
                    else return 1;
                }
                return book.dueDate.compareTo(book2.dueDate);
            }
        }
        );

        displayBookCache();

    }

    static List<Bridge.Account> runningUpdates = new ArrayList<Bridge.Account>();
    static public void updateAccount(Bridge.Account acc, final boolean autoUpdate, final boolean forceExtend){
        if (runningUpdates.contains(acc)) return;
        if ((acc.name == null || acc.name.equals("")) && (acc.pass == null || acc.pass.equals("")))
            return; //search only account
        runningUpdates.add(acc);
        if (instance != null) instance.setLoading(true);
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
