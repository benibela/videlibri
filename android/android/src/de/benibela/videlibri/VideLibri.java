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
import com.actionbarsherlock.view.Menu;


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
    static Bridge.Account accounts[] = null;
    public VideLibri(){
        super();
    }

    //Bridge functions called from VideLibri-midend
    String userPath(){
        return getFilesDir().getAbsolutePath();
    }

    static void allThreadsDone(){
        if (NotificationService.instance != null)
            NotificationService.instance.stopSelf();
        if (instance == null) return;
        //Log.i("VideLibri", "allThreadsDone started");
        instance.runOnUiThread(new Runnable() {
            @Override
            public void run() {
                NotificationService.showNotification(VideLibri.instance);

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

        Bridge.initialize();
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

       /* if (accounts == null || accounts.length == 0) newAccountDialog(true);
        else
         */

        if (displayHistoryActually != displayHistory || !hiddenAccounts.equals(hiddenAccountsActually))
            displayAccount(null);
        //setTitle("Ausleihen");  //does not work in onCreate (why? makes the title invisible) No. it just works sometimes?

        if (accounts == null || accounts.length == 0){
            View v = findViewById(R.id.layout); //need an arbitrary view. Depends on landscape/portrait, which is there
            if (v == null) v = findViewById(R.id.booklistview);
            if (v == null) v = findViewById(R.id.list);
            if (v != null) v.postDelayed(new Runnable() {
                @Override
                public void run() {
                    newAccountDialog(true); //do not call directly, because then the main activity becomes not visible till restart
                }
            }, 400);
        }


    }

    public void onDestroy(){
        super.onDestroy();
        instance = null;
        Bridge.VLFinalize();
    }


    //Mix

    public void newAccountDialog(boolean initial){
        Intent intent = new Intent(this, AccountInfo.class);
        intent.putExtra("mode", initial ? AccountInfo.MODE_ACCOUNT_CREATION_INITIAL : AccountInfo.MODE_ACCOUNT_CREATION) ;
        startActivity(intent);
    }

    static void newSearchActivity(){
        Intent intent = new Intent(instance, Search.class);
        if (instance.accounts.length > 0){
            String libId = instance.accounts[0].libId;
            intent.putExtra("libId", libId);
            intent.putExtra("libName", instance.accounts[0].getLibrary().namePretty);

            boolean sure = true;
            for (int i=1;i<instance.accounts.length;i++)
                if (!libId.equals(instance.accounts[i].libId)) {
                    sure = false;
                    break;
                }

            if (!sure) intent.putExtra("showLibList", true);
        }
        instance.startActivity(intent);
    }

    public boolean displayHistory = false;
    private boolean displayHistoryActually = false;
    public ArrayList<Bridge.Account> hiddenAccounts = new ArrayList<Bridge.Account>();
    private ArrayList<Bridge.Account> hiddenAccountsActually = new ArrayList<Bridge.Account>();

    @Override
    public boolean onPrepareOptionsMenu(Menu menu) {
        boolean x = super.onPrepareOptionsMenu(menu);    //To change body of overridden methods use File | Settings | File Templates.
        menu.findItem(R.id.accounts).setVisible(false);
        return x;
    }

    @Override
    protected void onActivityResult(int requestCode, int resultCode, Intent data) {
        super.onActivityResult(requestCode, resultCode, data);
    }

    static void addAccount(Bridge.Account acc){
        if (instance == null) return;
        Bridge.VLAddAccount(acc);
        instance.accounts = Bridge.VLGetAccounts();
        VideLibri.updateAccount(acc, false, false);
    }
    static void deleteAccount(Bridge.Account acc){
        if (instance == null) return;
        Bridge.VLDeleteAccount(acc);
        instance.accounts = Bridge.VLGetAccounts();
        instance.displayAccount(null);
    }
    static void changeAccount(Bridge.Account old, Bridge.Account newacc){
        if (instance == null) return;
        Bridge.VLChangeAccount(old, newacc);
        instance.accounts = Bridge.VLGetAccounts();
        VideLibri.updateAccount(newacc, false, false);
    }

    public void displayAccount(Bridge.Account acc){
        displayHistoryActually = displayHistory;
        hiddenAccountsActually.clear();
        hiddenAccountsActually.addAll(hiddenAccounts);


        if (acc == null) {
            bookCache = new ArrayList<Bridge.Book>();
            for (Bridge.Account facc: accounts) {
                if (hiddenAccounts.contains(facc))
                    continue;
                Bridge.Book[] books = Bridge.VLGetBooks(facc, false);
                for (Bridge.Book b: books) bookCache.add(b);
                if (displayHistoryActually){
                    books = Bridge.VLGetBooks(facc, true);
                    for (Bridge.Book b: books) bookCache.add(b);
                }
            }
        } else {
            ArrayList<Bridge.Book> oldBookCache = bookCache;
            boolean hidden = hiddenAccounts.contains(acc);
            if (hidden) {
                boolean currentlyVisible = false;
                for (Bridge.Book b: oldBookCache)
                    if (acc.equals(b.account)) {
                        currentlyVisible = true;
                        break;
                    }
                if (hiddenAccounts.size() == 0) setTitle(bookCache.size() + "Ausleihen");
                else setTitle(bookCache.size() + " Ausleihen: "+(accounts.length-hiddenAccounts.size())+ "/"+accounts.length+" Konten");
                if (!currentlyVisible) return;
            }
            bookCache = new ArrayList<Bridge.Book>();
            for (Bridge.Book b: oldBookCache)
                if (!acc.equals(b.account)) bookCache.add(b);
            if (!hidden) {
                Bridge.Book[] books = Bridge.VLGetBooks(acc, false);
                for (Bridge.Book b: books) bookCache.add(b);
                if (displayHistoryActually){
                    books = Bridge.VLGetBooks(acc, true);
                    for (Bridge.Book b: books) bookCache.add(b);
                }
            }
        }

        Collections.sort(bookCache, new Comparator<Bridge.Book>() {
            @Override
            public int compare(Bridge.Book book, Bridge.Book book2) {
                if (book.history != book2.history) {
                    if (book.history) return  1;
                    else return -1;
                }
                if ((book.getStatus() == Bridge.Book.StatusEnum.Ordered  || book.getStatus() == Bridge.Book.StatusEnum.Provided)
                    !=
                    (book2.getStatus() == Bridge.Book.StatusEnum.Ordered || book2.getStatus() == Bridge.Book.StatusEnum.Provided))
                    if ((book.getStatus() == Bridge.Book.StatusEnum.Ordered  || book.getStatus() == Bridge.Book.StatusEnum.Provided)) return 1;
                    else return -1;

                if ((book.dueDate == null) != (book2.dueDate == null))
                    if (book.dueDate == null) return -1;
                    else return 1;

                return book.dueDate.compareTo(book2.dueDate);
            }
        }
        );

        displayBookCache();

        if (hiddenAccounts.size() == 0) setTitle(bookCache.size() + " Ausleihen");
        else setTitle(bookCache.size() + " Ausleihen: "+(accounts.length-hiddenAccounts.size())+ "/"+accounts.length+" Konten");

    }

    static List<Bridge.Account> runningUpdates = new ArrayList<Bridge.Account>();
    static public void updateAccount(Bridge.Account acc, final boolean autoUpdate, final boolean forceExtend){
        if (acc == null ) {
            if (accounts == null) accounts = Bridge.VLGetAccounts();
            for (Bridge.Account a: accounts)
                updateAccount(a, autoUpdate, forceExtend);
            return;
        }
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
