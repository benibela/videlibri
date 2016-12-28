package de.benibela.videlibri;
import android.content.Context;
import android.content.DialogInterface;
import android.content.Intent;
import android.content.SharedPreferences;
import android.content.res.AssetManager;
import java.io.InputStream;
import java.text.DateFormat;
import java.text.Normalizer;
import java.text.SimpleDateFormat;
import java.util.*;
import java.lang.*;
import android.app.*;
import android.graphics.Color;
import android.os.Bundle;
import android.preference.PreferenceManager;
import android.support.v7.app.AppCompatActivity;
import android.text.Editable;
import android.text.TextWatcher;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.Menu;
import android.view.MenuItem;
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
    public VideLibri(){
        super();
    }

    //Called from Android OS

    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

       // Log.i("VideLibri", "onCreate")               ;

        if (savedInstanceState != null)
            filterActually = savedInstanceState.getString("filterActually");
        updateViewFilters();

        findViewById(R.id.searchFilterPanel).setVisibility(View.VISIBLE);
        EditText et = (EditText) findViewById(R.id.searchFilter);
        et.addTextChangedListener(new TextWatcher() {
            @Override
            public void beforeTextChanged(CharSequence charSequence, int i, int i2, int i3) { }

            @Override
            public void onTextChanged(CharSequence charSequence, int i, int i2, int i3) { }

            @Override
            public void afterTextChanged(Editable editable) {
                filterActually = editable.toString();
                refreshBookCache();
            }
        });


        if (VideLibriApp.accounts == null || VideLibriApp.accounts.length == 0) ; //startActivity(new Intent(this, NewAccountWizard.class));
        else {
            displayAccount(null);
            for (Bridge.Account a: VideLibriApp.accounts) VideLibriApp.updateAccount(a, true, false);
        }

    }



    @Override
    protected void onResume() {
        super.onResume();

        updateViewFilters();

        if (displayHistoryActually != displayHistory
                || !hiddenAccounts.equals(hiddenAccountsActually)
                || noDetailsInOverviewActually != options.contains(BookOverviewAdapter.DisplayEnum.NoDetails)
                || showRenewCountActually != options.contains(BookOverviewAdapter.DisplayEnum.ShowRenewCount)
                || displayForcedCounterActually != displayForcedCounter
                || !groupingKey.equals(groupingKeyActually)
                || !sortingKey.equals(sortingKeyActually)
                || !filterKey.equals(filterKeyActually)
                ) {
            View searchPanel = findViewById(R.id.searchFilterPanel);
            if (searchPanel != null) searchPanel.setVisibility( "__disabled".equals(filterKey) ? View.GONE : View.VISIBLE );
            displayAccount(null);
        }
        //setTitle("Ausleihen");  //does not work in onCreate (why? makes the title invisible) No. it just works sometimes?

        if (VideLibriApp.accounts == null || VideLibriApp.accounts.length == 0){
            View v = findViewById(R.id.layout); //need an arbitrary view. Depends on landscape/portrait, which is there
            if (v == null) v = findViewById(R.id.booklistview);
            //if (v == null) v = findViewById(R.id.list);
            if (v != null) v.postDelayed(new Runnable() {
                @Override
                public void run() {
                    newAccountDialog(true); //do not call directly, because then the main activity becomes not visible till restart
                }
            }, 400);
        }

        setLoading(!VideLibriApp.runningUpdates.isEmpty());
        if (!cacheShown)
            displayBookCache();
    }

    @Override
    protected void onSaveInstanceState(Bundle outState) {
        super.onSaveInstanceState(outState);
        outState.putString("filterActually", filterActually);
    }

    private void updateViewFilters(){
        SharedPreferences sp = PreferenceManager.getDefaultSharedPreferences(this);
        setOption(BookOverviewAdapter.DisplayEnum.NoDetails, sp.getBoolean("noLendBookDetails", false));
        setOption(BookOverviewAdapter.DisplayEnum.ShowRenewCount, sp.getBoolean("showRenewCount", true));
        sortingKey = sp.getString("sorting", "dueDate");
        groupingKey = sp.getString("grouping", "_dueWeek");
        filterKey = sp.getString("filtering", "");
        displayHistory = sp.getBoolean("displayHistory", false);
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        if (item.getItemId() == android.R.id.home) {
            if (showList())
                openOptionsMenu();
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
    private boolean noDetailsInOverviewActually = false, showRenewCountActually = true;
    private String sortingKeyActually, groupingKeyActually, filterActually, filterKeyActually, filterKey;
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


    static private int dateToWeek(int pascalDate){
        return (pascalDate - 2) / 7;
    }
    static public String getWeekString(Bridge.SimpleDate date){
        if (date == null) return Util.tr(R.string.unknown_date);

        int week = dateToWeek(date.pascalDate);
        if (Bridge.currentPascalDate > 0)
            switch (week - dateToWeek(Bridge.currentPascalDate)){
                case -1: return Util.tr(R.string.last_week);
                case 0: return Util.tr(R.string.this_week);
                case 1: return Util.tr(R.string.next_week);
            }
        int delta =  date.pascalDate - 2 - week * 7;
        return String.format(Util.tr(R.string.week_from_to),
                Util.formatDate(new Date(date.getTime().getTime() - delta * 1000 * 60 * 60 * 24 )),
                Util.formatDate(new Date(date.getTime().getTime() + (6 - delta) * 1000 * 60 * 60 * 24 )));
    }
    static public String getKeyValue(Bridge.Book b, String key){
        if ("_dueWeek".equals(key)) {
            return getWeekString(b.dueDate);
        } else if ("_account".equals(key)) {
            return b.account.prettyName;
        } else if ("dueDate".equals(key)) {
            return Util.formatDate(b.dueDate);
        } else if ("_status".equals(key)) {
            switch (b.getStatus()) {
                case Unknown: case Normal: return Util.tr(R.string.book_status_normal);
                case Problematic: return Util.tr(R.string.book_status_problematic);
                case Ordered: return Util.tr(R.string.book_status_ordered);
                case Provided: return Util.tr(R.string.book_status_provided);
                default: return Util.tr(R.string.book_status_unknown);
            }
        } else if ("_issueWeek".equals(key)) {
            return getWeekString(b.issueDate);
        } else if ("issueDate".equals(key)) {
            return Util.formatDate(b.issueDate);
        } else if ("".equals(key)) {
            return "";
        } else return b.getProperty(key);
    }
    static public int compareForStateMismatch(Bridge.Book book, Bridge.Book book2){
        if (book.history != book2.history) {
            if (book.history) return  1;
            else return -1;
        }
        if ((book.getStatus() == Bridge.Book.StatusEnum.Ordered  || book.getStatus() == Bridge.Book.StatusEnum.Provided)
                !=
                (book2.getStatus() == Bridge.Book.StatusEnum.Ordered || book2.getStatus() == Bridge.Book.StatusEnum.Provided))
            if ((book.getStatus() == Bridge.Book.StatusEnum.Ordered  || book.getStatus() == Bridge.Book.StatusEnum.Provided)) return 1;
            else return -1;

        return Util.compareNullFirst(book.dueDate, book2.dueDate);
    }
    static public int compareStatus(Bridge.Book.StatusEnum s1, Bridge.Book.StatusEnum s2) {
        if (s1 == s2) return 0;
        //order: ordered normal provided problematic
        switch (s1) {
            case Unknown:
            case Normal:
                switch (s2) {
                    case Problematic: case Provided: return 1;
                    default: return -1;
                }
            case Problematic:
                return -1; //problematic is the highest
            case Ordered:
                return 1; //ordered the lowest
            case Provided:
                if (s2 == Bridge.Book.StatusEnum.Problematic) return 1;
                return -1;
        }
        return 0;
    }
    static public int compareForKey(Bridge.Book book, Bridge.Book book2, String key){
        if (book == null || book2 == null) return 0;
        if ("_dueWeek".equals(key)) {
            int temp = compareForStateMismatch(book, book2);
            if (temp != 0 || book.dueDate == null) return temp;
            return Util.compare(dateToWeek(book.dueDate.pascalDate), dateToWeek(book2.dueDate.pascalDate));
        } else if ("_account".equals(key)) {
            int temp = Util.compareNullFirst(book.account, book2.account);
            if (temp != 0 || book.account == null) return 0;
            temp = Util.compareNullFirst(book.account.prettyName, book2.account.prettyName);
            if (temp != 0 || book.account.prettyName == null) return 0;
            return book.account.prettyName.compareTo(book2.account.prettyName);
        } else if ("dueDate".equals(key)) {
            int temp = compareForStateMismatch(book, book2);
            if (temp != 0 || book.dueDate == null) return temp;
            return Util.compare(book.dueDate.pascalDate, book2.dueDate.pascalDate);
        } else if ("_status".equals(key)) {
            int temp = compareForStateMismatch(book, book2);
            if (temp != 0) return temp;
            return compareStatus(book.getStatus(), book2.getStatus());
        } else if ("_issueWeek".equals(key)) {
            int temp = compareForStateMismatch(book, book2);
            if (temp != 0) return temp;
            temp = Util.compareNullFirst(book.issueDate, book2.issueDate);
            if (temp != 0 || book.issueDate == null) return temp;
            return Util.compare(dateToWeek(book.issueDate.pascalDate), dateToWeek(book2.issueDate.pascalDate));
        } else if ("issueDate".equals(key)) {
            int temp = compareForStateMismatch(book, book2);
            if (temp != 0) return temp;
            temp = Util.compareNullFirst(book.issueDate, book2.issueDate);
            if (temp != 0 || book.issueDate == null) return temp;
            return Util.compare(book.issueDate.pascalDate, book2.issueDate.pascalDate);
        } else if ("".equals(key)) {
            return 0;
        } else return book.getProperty(key).compareTo(book2.getProperty(key));
    }

    static final ArrayList<Bridge.Book.Pair> crazyHeaderHack = new ArrayList<Bridge.Book.Pair>();
    static public ArrayList<Bridge.Book> makePrimaryBookCache(Bridge.Account acc, ArrayList<Bridge.Book> oldBookCache,
                                                              boolean renewableOnly){
        //renewableOnly is not supported for acc != null

        boolean addHistory = displayHistory && !renewableOnly;
        ArrayList<Bridge.Book> bookCache = new ArrayList<Bridge.Book>();
        if (acc == null) {
            for (Bridge.Account facc: VideLibriApp.accounts) {
                if (hiddenAccounts.contains(facc))
                    continue;
                Bridge.Book[] books = Bridge.VLGetBooks(facc, false);
                if (!renewableOnly) {
                    for (Bridge.Book b: books) bookCache.add(b);
                } else
                    for (Bridge.Book b: books)
                        if (b.getStatus() == Bridge.Book.StatusEnum.Unknown || b.getStatus() == Bridge.Book.StatusEnum.Normal)
                            bookCache.add(b);
                if (addHistory){
                    books = Bridge.VLGetBooks(facc, true);
                    for (Bridge.Book b: books) bookCache.add(b);
                }
            }
        } else {
            boolean hidden = hiddenAccounts.contains(acc);
            for (Bridge.Book b: oldBookCache)
                if (!acc.equals(b.account) && b.account != null) bookCache.add(b);
            if (!hidden) {
                Bridge.Book[] books = Bridge.VLGetBooks(acc, false);
                for (Bridge.Book b: books) bookCache.add(b);
                if (addHistory){
                    books = Bridge.VLGetBooks(acc, true);
                    for (Bridge.Book b: books) bookCache.add(b);
                }
            }
        }


        return bookCache;
    }
    static public ArrayList<Bridge.Book> filterToSecondaryBookCache(ArrayList<Bridge.Book> oldBookCache,
                                                                    final String groupingKey, final String sortingKey,
                                                                    String filter, String filterKey){
        ArrayList<Bridge.Book> bookCache = new ArrayList<Bridge.Book>();

        if (filter != null && !"".equals(filter) && !"__disabled".equals(filterKey)) {
            filter = filter.toLowerCase();
            for (Bridge.Book book: oldBookCache)
                if (book.matchesFilter(filter, filterKey))
                    bookCache.add(book);
        } else bookCache.addAll(oldBookCache);

        Collections.sort(bookCache, new Comparator<Bridge.Book>() {
            @Override
            public int compare(Bridge.Book book, Bridge.Book book2) {
                int temp = compareForKey(book, book2, groupingKey);
                if (temp != 0) return temp;
                return compareForKey(book, book2, sortingKey);
            }
        }
        );

        if (!"".equals(groupingKey)) {
            String lastGroup = "";
            Bridge.Book groupHeader = null;
            for (int i=0;i<bookCache.size();i++) {
                Bridge.Book b = bookCache.get(i);
                String newGroup = getKeyValue(b, groupingKey);
                if (!newGroup.equals(lastGroup)) {
                    groupHeader = new Bridge.Book();
                    groupHeader.title = newGroup;
                    groupHeader.more = crazyHeaderHack;
                    groupHeader.history = true;
                    groupHeader.setStatus(Bridge.Book.StatusEnum.Ordered);
                    bookCache.add(i, groupHeader);
                    lastGroup = newGroup;
                    i++;
                }
                if (groupHeader != null) {
                    if (!b.history) groupHeader.history = false;
                    if (compareStatus(groupHeader.getStatus(), b.getStatus()) > 0)
                        groupHeader.setStatus(b.getStatus());
                    if (b.dueDate != null && (groupHeader.dueDate == null || groupHeader.dueDate.pascalDate > b.dueDate.pascalDate))
                        groupHeader.dueDate = b.dueDate;
                }
            }
        }

        return bookCache;
    }


    public ArrayList<Bridge.Book> primaryBookCache = new ArrayList<Bridge.Book>();
    public void displayAccount(Bridge.Account acc){
        displayHistoryActually = displayHistory;
        noDetailsInOverviewActually = options.contains(BookOverviewAdapter.DisplayEnum.NoDetails);
        showRenewCountActually = options.contains(BookOverviewAdapter.DisplayEnum.ShowRenewCount);
        groupingKeyActually = groupingKey;
        sortingKeyActually = sortingKey;
        filterKeyActually = filterKey;
        displayForcedCounterActually = displayForcedCounter;
        hiddenAccountsActually.clear();
        hiddenAccountsActually.addAll(hiddenAccounts);


        primaryBookCache = makePrimaryBookCache(acc, bookCache, false);
        refreshBookCache();
    }




    public void refreshBookCache(){
        boolean xquery = filterActually != null && (filterActually.startsWith("xquery version") || filterActually.startsWith("for $") || filterActually.contains("$book"));
        if (!xquery && filterActually != null) {
            int controlchars = 0;
            for (int i=0;i<filterActually.length();i++)
                switch (filterActually.charAt(i) ){
                    case '!':case '$':case '(':case ')':case'*':case '+':case '"':case ',':case '/':case'<':case'=':case'>':case'[':case']':
                        controlchars++;
                }
            xquery = controlchars >= 4;
        }
        if (!xquery)
            bookCache = filterToSecondaryBookCache(primaryBookCache, groupingKeyActually, sortingKeyActually, filterActually, filterKeyActually);
        else {
            bookCache = new ArrayList<Bridge.Book>();
            for (Bridge.Book b: Bridge.VLXQuery(filterActually)) bookCache.add(b);
        }

        displayBookCache();

        int realCount = 0, realCountPrimary = 0;
        for (Bridge.Book b: primaryBookCache) if (b.account != null) realCountPrimary++;
        if (primaryBookCache.size() == bookCache.size()) realCount = realCountPrimary;
        else for (Bridge.Book b: bookCache) if (b.account != null) realCount++;
        if (hiddenAccounts.size() > 0) setTitle(tr(R.string.main_bookaccountcountDDD, realCount, (VideLibriApp.accounts.length-hiddenAccounts.size()), VideLibriApp.accounts.length));
        else if (realCountPrimary != realCount) setTitle(tr(R.string.main_bookcountDD, realCount, realCountPrimary));
        else setTitle(tr(R.string.main_bookcountD, realCount));
    }


    static private Bridge.Book lastSelectedBookForDialog;

    @Override
    public void onBookActionButtonClicked(final Bridge.Book book) {
        int action = -1;
        switch (book.getStatus()) {
            case Normal:
                VideLibriApp.renewBooks(new Bridge.Book[]{book});
                showList();
                break;
            case Ordered: case Provided:
                lastSelectedBookForDialog = book;
                Util.showMessageYesNo(DialogId.CANCEL_CONFIRM, tr(R.string.main_cancelconfirm));
        }
    }

    static int displayForcedCounter = 1;
    int displayForcedCounterActually;
    public static void displayAccountStatically(Bridge.Account account) {
        displayForcedCounter += 1;
        if (VideLibriApp.currentActivity instanceof VideLibri)
            ((VideLibri)VideLibriApp.currentActivity).displayAccount(account);
    }

    @Override
    boolean onDialogResult(int dialogId, int buttonId, Bundle more) {
        switch (dialogId) {
            case DialogId.CANCEL_CONFIRM:
                if (buttonId == DialogInterface.BUTTON_POSITIVE) {
                    Bridge.VLBookOperation(new Bridge.Book[]{lastSelectedBookForDialog}, Bridge.BOOK_OPERATION_CANCEL); //cancel
                    setLoading(true);
                    showList();
                }
                lastSelectedBookForDialog = null;
                return true;
        }
        return super.onDialogResult(dialogId, buttonId, more);
    }
}
