package de.benibela.videlibri;
import android.content.Context;
import android.content.DialogInterface;
import android.content.Intent;
import android.content.res.AssetManager;
import java.io.InputStream;
import java.text.DateFormat;
import java.text.Normalizer;
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
    public VideLibri(){
        super();
    }

    //Called from Android OS

    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

       // Log.i("VideLibri", "onCreate")               ;

        instance = this;

        if (VideLibriApp.accounts == null || VideLibriApp.accounts.length == 0) ; //startActivity(new Intent(this, NewAccountWizard.class));
        else {
            displayAccount(null);
            for (Bridge.Account a: VideLibriApp.accounts) VideLibriApp.updateAccount(a, true, false);
        }

    }



    @Override
    protected void onResume() {
        super.onResume();

       /* if (accounts == null || accounts.length == 0) newAccountDialog(true);
        else
         */

        if (displayHistoryActually != displayHistory || !hiddenAccounts.equals(hiddenAccountsActually) || noDetailsInOverviewActually != noDetailsInOverview)
            displayAccount(null);
        //setTitle("Ausleihen");  //does not work in onCreate (why? makes the title invisible) No. it just works sometimes?

        if (VideLibriApp.accounts == null || VideLibriApp.accounts.length == 0){
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
        if (instance == this) {
        //    Bridge.VLFinalize();
        //    Bridge.initialized = false;
            instance = null;
        }
    }


    //Mix

    public void newAccountDialog(boolean initial){
        Intent intent = new Intent(this, AccountInfo.class);
        intent.putExtra("mode", initial ? AccountInfo.MODE_ACCOUNT_CREATION_INITIAL : AccountInfo.MODE_ACCOUNT_CREATION) ;
        startActivity(intent);
    }


    public boolean displayHistory = false;
    private boolean displayHistoryActually = false;
    private boolean noDetailsInOverviewActually = false;
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



    public void displayAccount(Bridge.Account acc){
        displayHistoryActually = displayHistory;
        noDetailsInOverviewActually = noDetailsInOverview;
        hiddenAccountsActually.clear();
        hiddenAccountsActually.addAll(hiddenAccounts);


        if (acc == null) {
            bookCache = new ArrayList<Bridge.Book>();
            for (Bridge.Account facc: VideLibriApp.accounts) {
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
                else setTitle(bookCache.size() + " Ausleihen: "+(VideLibriApp.accounts.length-hiddenAccounts.size())+ "/"+VideLibriApp.accounts.length+" Konten");
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
                if (book == null || book2 == null) return 0;
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

                if (book.dueDate == null && book2.dueDate == null)
                    return 0;

                return book.dueDate.compareTo(book2.dueDate);
            }
        }
        );

        displayBookCache();

        if (hiddenAccounts.size() == 0) setTitle(bookCache.size() + " Ausleihen");
        else setTitle(bookCache.size() + " Ausleihen: "+(VideLibriApp.accounts.length-hiddenAccounts.size())+ "/"+VideLibriApp.accounts.length+" Konten");

    }



    @Override
    public void onBookActionButtonClicked(final Bridge.Book book) {
        int action = -1;
        switch (book.getStatus()) {
            case Normal:
                Bridge.VLBookOperation(new Bridge.Book[]{book}, Bridge.BOOK_OPERATION_RENEW); //renew
                if (detailsOpened) onBackPressed();
                break;
            case Ordered: case Provided:
                showMessageYesNo("Soll die Bestellung abgebrochen werden?", new MessageHandler() {
                    @Override
                    public void onDialogEnd(DialogInterface dialogInterface, int i) {
                        if (i == DialogInterface.BUTTON_POSITIVE) {
                            Bridge.VLBookOperation(new Bridge.Book[]{book}, Bridge.BOOK_OPERATION_CANCEL); //cancel
                            setLoading(true);
                            if (detailsOpened) onBackPressed();
                        }
                    }
                });
        }
    }
}
