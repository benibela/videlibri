package de.benibela.videlibri;
import android.content.*;
import android.content.res.AssetManager;
import java.io.InputStream;
import java.text.DateFormat;
import java.text.Normalizer;
import java.text.SimpleDateFormat;
import java.util.*;
import java.lang.*;
import android.app.*;
import android.graphics.BitmapFactory;
import android.graphics.Color;
import android.os.Build;
import android.os.Bundle;
import android.preference.PreferenceManager;
import android.support.v7.app.AppCompatActivity;
import android.support.v7.widget.*;
import android.text.Editable;
import android.text.TextWatcher;
import android.util.Log;
import android.view.*;
import android.widget.*;
import android.widget.Toolbar;

import org.json.JSONArray;
import org.json.JSONException;


public class VideLibri extends  BookListActivity implements AdapterView.OnItemSelectedListener {
    public VideLibri(){
        super();
    }

    //Called from Android OS

    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        createDrawerToggle();

       // Log.i("VideLibri", "onCreate")               ;

        if (savedInstanceState != null) {
            filterActually = savedInstanceState.getString("filterActually");
            accountFilterOverride = savedInstanceState.getInt("accountFilter", ACCOUNT_FILTER_ALL);
            accountFilterHistoryOverride = savedInstanceState.getBoolean("accountFilterHistory", false);
        } else accountFilterOverride = ACCOUNT_FILTER_ALL;
        updateViewFilters();
        setTitle("");

        Spinner filterSpinner = new Spinner(this);
        filterSpinnerAdapter = new ArrayAdapter<String>(this, android.R.layout.simple_spinner_item, accountFilters);
        filterSpinnerAdapter.setDropDownViewResource(android.R.layout.simple_spinner_dropdown_item);
        filterSpinner.setAdapter(filterSpinnerAdapter);
        filterSpinner.setSelection(
                accountFilterOverride == ACCOUNT_FILTER_SOME ? accountFilters.size() - 1 :
                accountFilterOverride + 1 + (accountFilterHistoryOverride ? VideLibriApp.accounts.length + 1 : 0)
        );
        filterSpinner.setOnItemSelectedListener(this);
        bookCountView = (TextView) getLayoutInflater().inflate(android.R.layout.simple_spinner_item, null);
        bookCountView.setGravity(Gravity.CENTER_VERTICAL);


        android.support.v7.widget.Toolbar bar = (android.support.v7.widget.Toolbar) findViewById(R.id.actionbar);
        bar.addView(filterSpinner, ViewGroup.LayoutParams.WRAP_CONTENT, ViewGroup.LayoutParams.MATCH_PARENT);
        bar.addView(bookCountView, ViewGroup.LayoutParams.WRAP_CONTENT, ViewGroup.LayoutParams.MATCH_PARENT);


        findViewById(R.id.searchFilterPanel).setVisibility(View.VISIBLE);
        EditText et = (EditText) findViewById(R.id.searchFilter);
        registerForContextMenu(et);
        et.addTextChangedListener(new TextWatcher() {
            @Override
            public void beforeTextChanged(CharSequence charSequence, int i, int i2, int i3) { }

            @Override
            public void onTextChanged(CharSequence charSequence, int i, int i2, int i3) { }

            @Override
            public void afterTextChanged(Editable editable) {
                setFilter(editable.toString());
            }
        });


        if (VideLibriApp.accounts == null || VideLibriApp.accounts.length == 0) ; //startActivity(new Intent(this, NewAccountWizard.class));
        else {
            displayAccounts();
            VideLibriApp.updateAccount(null, true, false);
        }


        endLoadingAll(VideLibriBaseActivity.LOADING_COVER_IMAGE);
    }

    void setFilter(String s) {
        String oldFilterActually = filterActually;
        filterActually = s;
        if (alwaysFilterOnHistory && ( Util.isEmptyString(oldFilterActually) != Util.isEmptyString(filterActually))) {
            displayAccounts();
        } else
            refreshBookCache();
    }


    @Override
    protected void onResume() {
        super.onResume();

        updateViewFilters();

        if (displayHistoryActually != (displayHistory || (alwaysFilterOnHistory && !Util.isEmptyString(filterActually)) || accountFilterHistoryOverride)
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
            displayAccounts();
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

        if (!VideLibriApp.runningUpdates.isEmpty()) beginLoading(LOADING_ACCOUNT_UPDATE);
        if (!cacheShown)
            displayBookCache();
    }

    @Override
    protected void onSaveInstanceState(Bundle outState) {
        super.onSaveInstanceState(outState);
        outState.putString("filterActually", filterActually);
        outState.putInt("accountFilter", accountFilterOverride);
        outState.putBoolean("accountFilterHistory", accountFilterHistoryOverride);
    }


    private void updateViewFilters(){
        SharedPreferences sp = PreferenceManager.getDefaultSharedPreferences(this);
        setOption(BookOverviewAdapter.DisplayEnum.NoDetails, sp.getBoolean("noLendBookDetails", false));
        setOption(BookOverviewAdapter.DisplayEnum.ShowRenewCount, sp.getBoolean("showRenewCount", true));
        sortingKey = sp.getString("sorting", "dueDate");
        groupingKey = sp.getString("grouping", "_dueWeek");
        filterKey = sp.getString("filtering", "");
        displayHistory = sp.getBoolean("displayHistory", false);
        alwaysFilterOnHistory = sp.getBoolean("alwaysFilterOnHistory", true);

        if (accountUpdateVersion != VideLibriApp.accountUpdateCounter
                || !hiddenAccounts.equals(hiddenAccountsActually)) {
            accountUpdateVersion = VideLibriApp.accountUpdateCounter;
            accountFilters.clear();
            accountFilters.add(tr(R.string.main_allaccounts));
            for (Bridge.Account acc: VideLibriApp.accounts)
                accountFilters.add(acc.prettyName);
            String plusHistory = " + " + tr(R.string.main_accountwithhistory);
            accountFilters.add(tr(R.string.main_allaccounts) + plusHistory);
            for (Bridge.Account acc: VideLibriApp.accounts)
                accountFilters.add(acc.prettyName + plusHistory);
            accountFilters.add(tr(R.string.main_accountcountDD, (VideLibriApp.accounts.length-hiddenAccounts.size()), VideLibriApp.accounts.length));
            if (filterSpinnerAdapter != null) filterSpinnerAdapter.notifyDataSetChanged();
        }
    }


    @Override
    public void onItemSelected(AdapterView<?> parent, View view, int position, long id) {
        if (position == accountFilters.size() - 1 ) {
            accountFilterOverride = ACCOUNT_FILTER_SOME;
            accountFilterHistoryOverride = false;
            //todo: this is not called on reselection: https://stackoverflow.com/questions/5335306/how-can-i-get-an-event-in-android-spinner-when-the-current-selected-item-is-sele
            startActivity(new Intent(this, Options.class));
        } else {
            accountFilterOverride = position - 1;
            accountFilterHistoryOverride = position > VideLibriApp.accounts.length;
            if (accountFilterHistoryOverride) accountFilterOverride -= VideLibriApp.accounts.length + 1;
        }
        displayAccounts();
    }

    @Override
    public void onNothingSelected(AdapterView<?> parent) {

    }


    //Mix

    public void newAccountDialog(boolean initial){
        Intent intent = new Intent(this, AccountInfo.class);
        intent.putExtra("mode", initial ? AccountInfo.MODE_ACCOUNT_CREATION_INITIAL : AccountInfo.MODE_ACCOUNT_CREATION) ;
        startActivity(intent);
    }

    static final int ACCOUNT_FILTER_ALL = -1;
    static final int ACCOUNT_FILTER_SOME = -2;
    protected int accountFilterOverride;
    private boolean accountFilterHistoryOverride;

    private ArrayAdapter<String> filterSpinnerAdapter;
    private int accountUpdateVersion;
    private ArrayList<String> accountFilters = new ArrayList<>();
    private TextView bookCountView;

    static public boolean displayHistory = false;
    public boolean alwaysFilterOnHistory = true;
    private boolean displayHistoryActually = false;
    private boolean noDetailsInOverviewActually = false, showRenewCountActually = true;
    private String sortingKeyActually, groupingKeyActually, filterActually, filterKeyActually, filterKey;
    static public ArrayList<Bridge.Account> hiddenAccounts = new ArrayList<Bridge.Account>();
    private ArrayList<Bridge.Account> hiddenAccountsActually = new ArrayList<Bridge.Account>();

    @Override
    protected int onPrepareOptionsMenuVisibility() {
        boolean hasAccounts = VideLibriApp.accounts.length > 0;
        if (!hasAccounts) return 0;
        return super.onPrepareOptionsMenuVisibility()
                | (hasAccounts ? ACTIONBAR_MENU_RENEW_ALL | ACTIONBAR_MENU_RENEW_LIST : 0)
                | (hasAccounts && VideLibriApp.runningUpdates.isEmpty() ? ACTIONBAR_MENU_REFRESH : 0);
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
    static public ArrayList<Bridge.Book> makePrimaryBookCache(int account,
                                                              boolean addHistory,
                                                              boolean renewableOnly){
        //renewableOnly is not supported for acc != null
        addHistory = addHistory && !renewableOnly;
        Bridge.Account[] accounts = account < 0 ? VideLibriApp.accounts : new Bridge.Account[]{VideLibriApp.accounts[account]};
        ArrayList<Bridge.Book> bookCache = new ArrayList<Bridge.Book>();
        for (Bridge.Account facc: accounts) {
            if (account == -2 && hiddenAccounts.contains(facc))
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
                    if (!b.history) {
                        groupHeader.history = false;
                        if (compareStatus(groupHeader.getStatus(), b.getStatus()) > 0)
                            groupHeader.setStatus(b.getStatus());
                        if (b.dueDate != null && (groupHeader.dueDate == null || groupHeader.dueDate.pascalDate > b.dueDate.pascalDate))
                            groupHeader.dueDate = b.dueDate;
                    }
                }
            }
        }

        return bookCache;
    }


    public ArrayList<Bridge.Book> primaryBookCache = new ArrayList<Bridge.Book>();
    public void displayAccounts(){
        displayHistoryActually = displayHistory || (alwaysFilterOnHistory && !Util.isEmptyString(filterActually)) || accountFilterHistoryOverride;
        noDetailsInOverviewActually = options.contains(BookOverviewAdapter.DisplayEnum.NoDetails);
        showRenewCountActually = options.contains(BookOverviewAdapter.DisplayEnum.ShowRenewCount);
        groupingKeyActually = groupingKey;
        sortingKeyActually = sortingKey;
        filterKeyActually = filterKey;
        displayForcedCounterActually = displayForcedCounter;
        hiddenAccountsActually.clear();
        hiddenAccountsActually.addAll(hiddenAccounts);


        primaryBookCache = makePrimaryBookCache( accountFilterOverride, displayHistoryActually, false);
        refreshBookCache();

        if (VideLibriApp.getMainIcon() != currentMainIcon){
            checkMainIcon();
            if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.LOLLIPOP) {
                setTaskDescription(new ActivityManager.TaskDescription(null, BitmapFactory.decodeResource(getResources(), VideLibriApp.getMainIcon(), null)));
            }
        }
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

        int bookCount = 0, bookCountPrimary = 0; //number of shown books (after filtering), number of books (before filtering)
        bookCount = bookCountPrimary = 0;
        for (Bridge.Book b: primaryBookCache) if (b.account != null) bookCountPrimary++;
        if (primaryBookCache.size() == bookCache.size()) bookCount = bookCountPrimary;
        else for (Bridge.Book b: bookCache) if (b.account != null) bookCount++;

        String title;
        if (bookCountPrimary != bookCount) title = tr(R.string.main_bookcountDD, bookCount, bookCountPrimary);
        else if (bookCount == 1) title = tr(R.string.main_bookcount1);
        else title = tr(R.string.main_bookcountD, bookCount);
        bookCountView.setText(title);
    }


    static private Bridge.Book lastSelectedBookForDialog;

    @Override
    public void onBookActionButtonClicked(final Bridge.Book book) {
        int action = -1;
        switch (book.getStatus()) {
            case Normal:
                lastSelectedBookForDialog = book;
                Util.showMessageNegPos(DialogId.RENEW_SINGLE_CONFIRM, tr(R.string.renew_single_confirm), R.string.cancel, R.string.renew);
                break;
            case Ordered: case Provided:
                lastSelectedBookForDialog = book;
                Util.showMessageYesNo(DialogId.CANCEL_CONFIRM, tr(R.string.main_cancelconfirm));
        }
    }

    static int displayForcedCounter = 1;
    int displayForcedCounterActually;
    public static void refreshDisplayedLendBooks() {
        displayForcedCounter += 1;
        if (VideLibriApp.currentActivity instanceof VideLibri)
            ((VideLibri)VideLibriApp.currentActivity).displayAccounts();
    }

    @Override
    boolean onDialogResult(int dialogId, int buttonId, Bundle more) {
        switch (dialogId) {
            case DialogId.RENEW_SINGLE_CONFIRM:
                if (buttonId == DialogInterface.BUTTON_POSITIVE) {
                    VideLibriApp.renewBooks(new Bridge.Book[]{lastSelectedBookForDialog});
                    showList();
                }
                return true;
            case DialogId.CANCEL_CONFIRM:
                if (buttonId == DialogInterface.BUTTON_POSITIVE) {
                    Bridge.VLBookOperation(new Bridge.Book[]{lastSelectedBookForDialog}, Bridge.BOOK_OPERATION_CANCEL); //cancel
                    beginLoading(LOADING_ACCOUNT_UPDATE);
                    showList();
                }
                lastSelectedBookForDialog = null;
                return true;
            case DialogId.FILTER_LOAD_LIST:
                if (buttonId >= 0)
                    setEditTextText(R.id.searchFilter, getFilterHistory().get(buttonId));
                return true;
        }
        return super.onDialogResult(dialogId, buttonId, more);
    }


    @Override
    public void onCreateContextMenu(ContextMenu menu, View v, ContextMenu.ContextMenuInfo menuInfo) {
        if (v != null && v.getId() == R.id.searchFilter) {
            getMenuInflater().inflate(R.menu.searchfiltercontextmenu, menu);
            contextMenuSelectedItem = filterActually;
        } else
            super.onCreateContextMenu(menu, v, menuInfo);
    }

    private ArrayList<String> getFilterHistory(){
        ArrayList<String> filters = new ArrayList<String>();
        try {
            JSONArray filtersJson = new JSONArray(PreferenceManager.getDefaultSharedPreferences(this).getString("filterHistory", tr(R.string.config_example_filters_json)));
            for (int i=0;i<filtersJson.length();i++)
                filters.add(filtersJson.getString(i));
        } catch (JSONException e) {
        }
        return filters;
    }

    @Override
    public boolean onContextItemSelected(MenuItem item) {
        switch (item.getItemId()){
            case R.id.load_filter: {
                Util.chooseDialog(DialogId.FILTER_LOAD_LIST, tr(R.string.menu_context_load_filter), getFilterHistory().toArray(new String[0]));
                return true;
            } case R.id.save_filter:
                ArrayList<String> filters = getFilterHistory();
                if (filters.contains(filterActually)) filters.remove(filterActually);
                filters.add(0, filterActually);
                JSONArray filtersJson = new JSONArray();
                for (String s: filters) filtersJson.put(s);
                SharedPreferences sp = PreferenceManager.getDefaultSharedPreferences(this);
                SharedPreferences.Editor editor = sp.edit();
                editor.putString("filterHistory", filtersJson.toString());
                editor.commit();
                return true;
            case R.id.clear:
                setEditTextText(R.id.searchFilter, "");
                return true;
            case R.id.paste:
            case R.id.pastereplace:
                CharSequence text = null;
                if(android.os.Build.VERSION.SDK_INT < android.os.Build.VERSION_CODES.HONEYCOMB) {
                    text = ((android.text.ClipboardManager) getSystemService(CLIPBOARD_SERVICE)).getText();
                } else {
                    ClipData data = ((android.content.ClipboardManager)getSystemService(CLIPBOARD_SERVICE)).getPrimaryClip();
                    if (data != null) {
                        ClipData.Item i = data.getItemAt(0);
                        if (i != null) text = i.coerceToText(this);
                    }
                }
                if (text != null) {
                    setEditTextText(R.id.searchFilter, (item.getItemId() == R.id.pastereplace ? "" : filterActually) + text);
                }
                return true;
        }
        return super.onContextItemSelected(item);
    }

}
