package de.benibela.videlibri;
import android.content.Context;
import android.content.DialogInterface;
import android.content.Intent;
import android.content.SharedPreferences;
import android.content.res.AssetManager;
import java.io.InputStream;
import java.text.DateFormat;
import java.text.Normalizer;
import java.util.*;
import java.lang.*;
import android.app.*;
import android.graphics.Color;
import android.os.Bundle;
import android.preference.PreferenceManager;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.*;
import com.actionbarsherlock.view.Menu;
import com.actionbarsherlock.view.MenuItem;


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
    public VideLibri(){
        super();
    }

    //Called from Android OS

    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

       // Log.i("VideLibri", "onCreate")               ;


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
        noDetailsInOverview = PreferenceManager.getDefaultSharedPreferences(this).getBoolean("noLendBookDetails", false);
        if (displayHistoryActually != displayHistory
                || !hiddenAccounts.equals(hiddenAccountsActually)
                || noDetailsInOverviewActually != noDetailsInOverview
                || displayForcedCounterActually != displayForcedCounter)
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

        setLoading(!VideLibriApp.runningUpdates.isEmpty());
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        if (item.getItemId() == android.R.id.home) {
            if (detailsOpened) onBackPressed();
            else openOptionsMenu();
            return true;
        }
        return super.onOptionsItemSelected(item);
    }


    //Mix

    public void newAccountDialog(boolean initial){
        Intent intent = new Intent(this, AccountInfo.class);
        intent.putExtra("mode", initial ? AccountInfo.MODE_ACCOUNT_CREATION_INITIAL : AccountInfo.MODE_ACCOUNT_CREATION) ;
        startActivity(intent);
    }


    static public boolean displayHistory = false;
    private boolean displayHistoryActually = false;
    private boolean noDetailsInOverviewActually = false;
    static public ArrayList<Bridge.Account> hiddenAccounts = new ArrayList<Bridge.Account>();
    private ArrayList<Bridge.Account> hiddenAccountsActually = new ArrayList<Bridge.Account>();

    @Override
    public boolean onPrepareOptionsMenu(Menu menu) {
        boolean x = super.onPrepareOptionsMenu(menu);    //To change body of overridden methods use File | Settings | File Templates.
        //menu.findItem(R.id.accounts).setVisible(false);
        return x;
    }

    @Override
    protected void onActivityResult(int requestCode, int resultCode, Intent data) {
        super.onActivityResult(requestCode, resultCode, data);
    }

    static public ArrayList<Bridge.Book> makeUpdatedBookCache(Bridge.Account acc, ArrayList<Bridge.Book> oldBookCache){
        ArrayList<Bridge.Book> bookCache = new ArrayList<Bridge.Book>();
        if (acc == null) {
            for (Bridge.Account facc: VideLibriApp.accounts) {
                if (hiddenAccounts.contains(facc))
                    continue;
                Bridge.Book[] books = Bridge.VLGetBooks(facc, false);
                for (Bridge.Book b: books) bookCache.add(b);
                if (displayHistory){
                    books = Bridge.VLGetBooks(facc, true);
                    for (Bridge.Book b: books) bookCache.add(b);
                }
            }
        } else {
            boolean hidden = hiddenAccounts.contains(acc);
            for (Bridge.Book b: oldBookCache)
                if (!acc.equals(b.account)) bookCache.add(b);
            if (!hidden) {
                Bridge.Book[] books = Bridge.VLGetBooks(acc, false);
                for (Bridge.Book b: books) bookCache.add(b);
                if (displayHistory){
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

        return bookCache;
    }


    public void displayAccount(Bridge.Account acc){
        displayHistoryActually = displayHistory;
        noDetailsInOverviewActually = noDetailsInOverview;
        displayForcedCounterActually = displayForcedCounter;
        hiddenAccountsActually.clear();
        hiddenAccountsActually.addAll(hiddenAccounts);


        bookCache = makeUpdatedBookCache(acc, bookCache);
        displayBookCache();

        if (hiddenAccounts.size() == 0) setTitle(bookCache.size() + " Ausleihen");
        else setTitle(bookCache.size() + " Ausleihen: "+(VideLibriApp.accounts.length-hiddenAccounts.size())+ "/"+VideLibriApp.accounts.length+" Konten");

    }



    @Override
    public void onBookActionButtonClicked(final Bridge.Book book) {
        int action = -1;
        switch (book.getStatus()) {
            case Normal:
                VideLibriApp.renewBooks(new Bridge.Book[]{book});
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

    static int displayForcedCounter = 1;
    int displayForcedCounterActually;
    public static void displayAccountStatically(Bridge.Account account) {
        displayForcedCounter += 1;
        if (VideLibriApp.currentActivity instanceof VideLibri)
            ((VideLibri)VideLibriApp.currentActivity).displayAccount(account);
    }
}
