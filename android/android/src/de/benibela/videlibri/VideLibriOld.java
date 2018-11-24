package de.benibela.videlibri;
import android.Manifest;
import android.app.Activity;
import android.app.ActivityManager;
import android.app.AlertDialog;
import android.app.Dialog;
import android.content.DialogInterface;
import android.content.Intent;
import android.content.SharedPreferences;
import android.content.pm.PackageManager;
import android.graphics.BitmapFactory;
import android.os.Build;
import android.os.Bundle;
import android.preference.PreferenceManager;
import android.support.annotation.NonNull;
import android.support.v4.app.ActivityCompat;
import android.support.v4.content.ContextCompat;
import android.text.Editable;
import android.text.TextWatcher;
import android.view.ContextMenu;
import android.view.Gravity;
import android.view.LayoutInflater;
import android.view.MenuItem;
import android.view.View;
import android.widget.Button;
import android.widget.CompoundButton;
import android.widget.EditText;
import android.widget.LinearLayout;

import org.json.JSONArray;
import org.json.JSONException;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;

import de.benibela.videlibri.jni.Bridge;


public class VideLibriOld extends BookListActivity {
    //Called from Android OS

    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        createDrawerToggle();

       // Log.i("VideLibri", "onCreate")               ;

        if (savedInstanceState != null) {
            filterActually = savedInstanceState.getString("filterActually");
            setFilterMultiLine(savedInstanceState.getBoolean("filterIsMultiLine", false));
        }
        updateViewFilters();


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

        updateAccountView();

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

        if (ContextCompat.checkSelfPermission(this, Manifest.permission.INTERNET) != PackageManager.PERMISSION_GRANTED)
            //unnecessary, because not dangerous??
            ActivityCompat.requestPermissions(this, new String[]{Manifest.permission.INTERNET}, 0);
    }



    @Override
    protected void onSaveInstanceState(Bundle outState) {
        super.onSaveInstanceState(outState);
        outState.putString("filterActually", filterActually);
        outState.putBoolean("filterIsMultiLine", filterIsMultiLine);
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
    }

    private void updateAccountView(){
        updateViewFilters();
        if (displayHistoryActually != (displayHistory || (alwaysFilterOnHistory && !Util.isEmptyString(filterActually)))
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
    }


    //Mix

    public void newAccountDialog(boolean initial){
        Intent intent = new Intent(this, AccountInfo.class);
        intent.putExtra("mode", initial ? AccountInfo.MODE_ACCOUNT_CREATION_INITIAL : AccountInfo.MODE_ACCOUNT_CREATION) ;
        startActivity(intent);
    }

    //private int accountUpdateVersion;
    //private TextView bookCountView;

    static public boolean displayHistory = false;
    public boolean alwaysFilterOnHistory = true;
    private boolean displayHistoryActually = false;
    private boolean noDetailsInOverviewActually = false, showRenewCountActually = true;
    private String sortingKeyActually, groupingKeyActually, filterActually, filterKeyActually, filterKey;
    private boolean filterIsMultiLine;
    static public ArrayList<Bridge.Account> hiddenAccounts = new ArrayList<>();
    private ArrayList<Bridge.Account> hiddenAccountsActually = new ArrayList<>();



    static private int dateToWeek(int pascalDate){
        return (pascalDate - 2) / 7;
    }
    static public String getWeekString(int pascalDate){
        if (pascalDate == 0) return Util.tr(R.string.unknown_date);

        int week = dateToWeek(pascalDate);
        if (Bridge.currentPascalDate > 0)
            switch (week - dateToWeek(Bridge.currentPascalDate)){
                case -1: return Util.tr(R.string.last_week);
                case 0: return Util.tr(R.string.this_week);
                case 1: return Util.tr(R.string.next_week);
            }
        int delta =  pascalDate - 2 - week * 7;
        return String.format(Util.tr(R.string.week_from_to),
                Util.formatDate(Bridge.pascalDateToDate(pascalDate - delta)),
                Util.formatDate(Bridge.pascalDateToDate(pascalDate - delta + 6)));
    }
    static public String getKeyValue(Bridge.Book b, String key){
        switch (key) {
            case "_dueWeek":
                return getWeekString(b.dueDate);
            case "_account":
                return b.account.prettyName;
            case "dueDate":
                return BookFormatter.formatDate(b.dueDate);
            case "_status":
                switch (b.getStatus()) {
                    case Unknown:
                    case Normal:
                        return Util.tr(R.string.book_status_normal);
                    case Problematic:
                        return Util.tr(R.string.book_status_problematic);
                    case Ordered:
                        return Util.tr(R.string.book_status_ordered);
                    case Provided:
                        return Util.tr(R.string.book_status_provided);
                    default:
                        return Util.tr(R.string.book_status_unknown);
                }
            case "_issueWeek":
            case "issueDate":
                int date = b.issueDate != 0 ? b.issueDate : b.firstExistsDate;
                if (date == 0) return Util.tr(R.string.unknown_date);
                return "issueDate".equals(key) ? BookFormatter.formatDate(date) : getWeekString(date);
            case "":
                return "";
            default:
                return b.getProperty(key);
        }
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

        return Util.compare(book.dueDate != 0, book2.dueDate != 0);
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
        switch (key) {
            case "_dueWeek": {
                int temp = compareForStateMismatch(book, book2);
                if (temp != 0 || book.dueDate == 0) return temp;
                return Util.compare(dateToWeek(book.dueDate), dateToWeek(book2.dueDate));
            }
            case "_account": {
                int temp = Util.compareNullFirst(book.account, book2.account);
                if (temp != 0 || book.account == null) return 0;
                temp = Util.compareNullFirst(book.account.prettyName, book2.account.prettyName);
                if (temp != 0 || book.account.prettyName == null) return 0;
                return book.account.prettyName.compareTo(book2.account.prettyName);
            }
            case "dueDate": {
                int temp = compareForStateMismatch(book, book2);
                if (temp != 0 || book.dueDate == 0) return temp;
                return Util.compare(book.dueDate, book2.dueDate);
            }
            case "_status": {
                int temp = compareForStateMismatch(book, book2);
                if (temp != 0) return temp;
                return compareStatus(book.getStatus(), book2.getStatus());
            }
            case "_issueWeek": case "issueDate": {
                int temp = compareForStateMismatch(book, book2);
                if (temp != 0) return temp;
                int d1 = book.issueDate != 0 ? book.issueDate : book.firstExistsDate;
                int d2 = book2.issueDate != 0 ? book2.issueDate : book2.firstExistsDate;
                temp = Util.compare(d1 != 0, d2 != 0);
                if (temp != 0 || d1 == 0) return temp;
                if ("_issueWeek".equals(key)) {
                    d1 = dateToWeek(d1);
                    d2 = dateToWeek(d2);
                }
                return Util.compare(d1, d2);
            }
            case "":
                return 0;
            default:
                return book.getProperty(key).compareTo(book2.getProperty(key));
        }
    }

    static public ArrayList<Bridge.Book> makePrimaryBookCache(boolean addHistory,
                                                              boolean renewableOnly){
        //renewableOnly is not supported for acc != null
        addHistory = addHistory && !renewableOnly;
        Bridge.Account[] accounts = VideLibriApp.accounts;
        ArrayList<Bridge.Book> bookCache = new ArrayList<>();
        for (Bridge.Account facc: accounts) {
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

        return bookCache;
    }
    static public ArrayList<Bridge.Book> filterToSecondaryBookCache(ArrayList<Bridge.Book> oldBookCache,
                                                                    final String groupingKey, final String sortingKey,
                                                                    String filter, String filterKey){
        ArrayList<Bridge.Book> bookCache = new ArrayList<>();

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
                    groupHeader = new Bridge.Book() {
                        @Override
                        public boolean isGroupingHeader() {
                            return true;
                        }
                    };
                    groupHeader.title = newGroup;
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
                        if (b.dueDate != 0 && (groupHeader.dueDate == 0 || groupHeader.dueDate > b.dueDate))
                            groupHeader.dueDate = b.dueDate;
                    }
                }
            }
        }

        return bookCache;
    }


    public ArrayList<Bridge.Book> primaryBookCache = new ArrayList<>();
    public void displayAccounts(){
        displayHistoryActually = displayHistory || (alwaysFilterOnHistory && !Util.isEmptyString(filterActually));
        noDetailsInOverviewActually = options.contains(BookOverviewAdapter.DisplayEnum.NoDetails);
        showRenewCountActually = options.contains(BookOverviewAdapter.DisplayEnum.ShowRenewCount);
        groupingKeyActually = groupingKey;
        sortingKeyActually = sortingKey;
        filterKeyActually = filterKey;
        displayForcedCounterActually = displayForcedCounter;
        hiddenAccountsActually.clear();
        hiddenAccountsActually.addAll(hiddenAccounts);


        primaryBookCache = makePrimaryBookCache( displayHistoryActually, false);
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
            bookCache = new ArrayList<>();
            for (Bridge.Book b: Bridge.VLXQuery(filterActually)) bookCache.add(b);
        }

        displayBookCache();

        int bookCount = 0, bookCountPrimary = 0; //number of shown books (after filtering), number of books (before filtering)
        int bookCountPrimaryNoHistory = 0;
        if (displayHistoryActually) {
            for (Bridge.Book b : primaryBookCache) if (b.account != null) {
                bookCountPrimary++;
                if (!b.history) bookCountPrimaryNoHistory++;
            }
        } else {
            for (Bridge.Book b : primaryBookCache) if (b.account != null) bookCountPrimary++;
        }
        if (primaryBookCache.size() == bookCache.size()) bookCount = bookCountPrimary;
        else for (Bridge.Book b: bookCache) if (b.account != null) bookCount++;

        String title;
        if (displayHistoryActually) {
            if (bookCountPrimary == bookCount)
                title = tr(R.string.main_bookcounthistoryDD, bookCountPrimaryNoHistory, bookCountPrimary);
            else
                title = tr(R.string.main_bookcounthistoryDDD, bookCount, bookCountPrimaryNoHistory, bookCountPrimary);
        } else {
            if (bookCountPrimary != bookCount)
                title = tr(R.string.main_bookcountDD, bookCount, bookCountPrimary);
            else
                title = getResources().getQuantityString(R.plurals.main_bookcountPluralD, bookCount, bookCount);
        }
        if (!hiddenAccounts.isEmpty()){
            int shownAccounts = VideLibriApp.accounts.length - hiddenAccounts.size();
            if (shownAccounts == 1) {
                for (Bridge.Account account: VideLibriApp.accounts)
                    if (!hiddenAccounts.contains(account)) {
                        title += ", " + account.prettyName;
                        break;
                    }
            } else {
                title += ", " + tr(R.string.main_accountcountDD, shownAccounts, VideLibriApp.accounts.length);
            }
        }
        setTitle(title);
    }


    static private Bridge.Book lastSelectedBookForDialog;

    @Override
    public void onBookActionButtonClicked(final Bridge.Book book) {
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
        if (VideLibriApp.currentActivity instanceof LendingList)
            ((LendingList)VideLibriApp.currentActivity).displayAccounts();
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
            case DialogId.SPECIAL_LEND_LIST_OPTIONS:
                updateAccountView();
                return true;
        }
        return super.onDialogResult(dialogId, buttonId, more);
    }

    public boolean onOptionsItemIdSelected(int id) {
        switch (id) {
            case R.id.filter:
                (new ViewOptionsDialog()).show(getSupportFragmentManager(), null);
                return true;
        }
        return super.onOptionsItemIdSelected(id);
    }

    public static class ViewOptionsDialog extends android.support.v4.app.DialogFragment implements DialogInterface.OnCancelListener{
        View view;
        @NonNull
        public Dialog onCreateDialog(Bundle savedInstanceState) {
            Activity activity = getActivity();
            if (activity == null) return null;
            AlertDialog.Builder builder = new AlertDialog.Builder(activity);
            LayoutInflater inflater = activity.getLayoutInflater();
            view = inflater.inflate(R.layout.options_lendings, null);
            Options.showLendingOptionsInView(activity,view);

            LinearLayout linearLayout = (LinearLayout) view.findViewById(R.id.viewaccounts);
            linearLayout.removeAllViews();
            final ArrayList<CompoundButton> switchboxes = new ArrayList<>();
            CompoundButton.OnCheckedChangeListener checkListener = new CompoundButton.OnCheckedChangeListener() {
                @Override
                public void onCheckedChanged(CompoundButton compoundButton, boolean b) {
                    Bridge.Account acc = (Bridge.Account)compoundButton.getTag();
                    if (acc == null) {
                        if (b) LendingList.hiddenAccounts.clear();
                        else LendingList.hiddenAccounts = new ArrayList<>(Arrays.asList(VideLibriApp.accounts));
                        for (CompoundButton cb: switchboxes) cb.setChecked(b);
                    } else {
                        if (!b == LendingList.hiddenAccounts.contains(acc)) return;
                        if (!b) LendingList.hiddenAccounts.add(acc);
                        else LendingList.hiddenAccounts.remove(acc);
                    }
                }
            };
            CompoundButton.OnClickListener clickListener = new CompoundButton.OnClickListener(){
                @Override
                public void onClick(View v) {
                    Bridge.Account acc = (Bridge.Account)v.getTag();
                    if (acc == null) {
                        LendingList.hiddenAccounts.clear();
                    } else {
                        LendingList.hiddenAccounts.clear();
                        for (final Bridge.Account acc2 : VideLibriApp.accounts)
                            if (!acc.equals(acc2)) LendingList.hiddenAccounts.add(acc2);
                    }
                    ((CompoundButton) getDialog().findViewById(R.id.viewHistory)).setChecked(v.getId() == R.id.buttonforhistory);
                    getDialog().cancel();
                }
            };

            for (int i = -1; i < VideLibriApp.accounts.length; i++) {
                Bridge.Account acc = i == -1 ? null : VideLibriApp.accounts[i];
                View group = inflater.inflate(R.layout.options_lendings_accountrow, null);
                CompoundButton sb = ((CompoundButton)group.findViewById(R.id.switchbox));
                sb.setChecked(acc == null ? LendingList.hiddenAccounts.size() <= VideLibriApp.accounts.length / 2 : !LendingList.hiddenAccounts.contains(acc));
                sb.setTag(acc);
                sb.setOnCheckedChangeListener(checkListener);
                if (acc != null) switchboxes.add(sb);
                Button btn = ((Button) group.findViewById(R.id.button));
                btn.setText(acc == null ? getText(R.string.main_allaccounts) : acc.prettyName);
                btn.setTag(acc);
                btn.setOnClickListener(clickListener);
                btn = ((Button) group.findViewById(R.id.buttonforhistory));
                btn.setTag(acc);
                btn.setOnClickListener(clickListener);
                linearLayout.addView(group);
            }

            builder.setView(view);
            builder.setOnCancelListener(this);
            Dialog d = builder.create();
            if (d.getWindow() != null) d.getWindow().setGravity(Gravity.RIGHT | Gravity.TOP);
            return d;
        }

        @Override
        public void onCancel(DialogInterface dialog) {
            super.onCancel(dialog);
            Activity activity = getActivity();
            if (activity == null) return;
            Options.putLendingOptionsFromView(activity, view);
            if (activity instanceof LendingList)
                ((VideLibriOld)activity).updateAccountView();
        }
    }

    @Override
    public void onCreateContextMenu(ContextMenu menu, View v, ContextMenu.ContextMenuInfo menuInfo) {
        if (v != null && v.getId() == R.id.searchFilter) {
            getMenuInflater().inflate(R.menu.searchfiltercontextmenu, menu);
            menu.findItem(R.id.toggle_singleline).setTitle(filterIsMultiLine ? R.string.menu_context_filter_singleline : R.string.menu_context_filter_multiline);
            contextMenuSelectedItem = filterActually;
        } else
            super.onCreateContextMenu(menu, v, menuInfo);
    }

    private ArrayList<String> getFilterHistory(){
        ArrayList<String> filters = new ArrayList<>();
        try {
            JSONArray filtersJson = new JSONArray(PreferenceManager.getDefaultSharedPreferences(this).getString("filterHistory", tr(R.string.config_example_filters_json)));
            for (int i=0;i<filtersJson.length();i++)
                filters.add(filtersJson.getString(i));
        } catch (JSONException ignored) {
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
                CharSequence text = Util.Clipboard.getText(this);
                if (text != null) {
                    setEditTextText(R.id.searchFilter, (item.getItemId() == R.id.pastereplace ? "" : filterActually) + text);
                }
                return true;
            case R.id.toggle_singleline:
                setFilterMultiLine(!filterIsMultiLine);
                return true;
        }
        return super.onContextItemSelected(item);
    }

    void setFilterMultiLine(boolean ml){
        if (ml == filterIsMultiLine) return;
        filterIsMultiLine = ml;
        EditText edit = (EditText)findViewById(R.id.searchFilter);
        if (filterIsMultiLine) {
            edit.setSingleLine(false);
            edit.setMaxLines(10);
        } else {
            edit.setMaxLines(1);
            edit.setSingleLine(true);
        }
    }
}
