package de.benibela.videlibri;

import android.content.DialogInterface;
import android.os.Bundle;
import android.util.Log;
import android.view.Menu;
import android.view.MenuItem;
import de.benibela.videlibri.BookListActivity;
import de.benibela.videlibri.Bridge;
import de.benibela.videlibri.VideLibriBaseActivity;

import java.util.ArrayList;
import java.util.Arrays;

public class SearchResult extends BookListActivity implements Bridge.SearchEventHandler  {

    Bridge.SearcherAccess searcher;
    String libId = "";

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        Bridge.Book book = (Bridge.Book) getIntent().getSerializableExtra("searchQuery");
        if (book == null) { Log.i("VideLibri", "search without book. Abort."); finish(); return; }

        searcher = Search.searchers.isEmpty() ? null : Search.searchers.get(Search.searchers.size()-1);
        if (searcher == null) {
            setTitle(tr(R.string.search_lost));
            return;
        }
        libId = searcher.libId;
        switch (searcher.state){
            case Search.SEARCHER_STATE_INIT:
                searcher.connect(); //should not happen
                break;
            case Search.SEARCHER_STATE_CONNECTED:
                searcher.waitingForDetails = -1;
                searcher.nextDetailsRequested = -1;
                searcher.start(book, getIntent().getIntExtra("homeBranch", -1), getIntent().getIntExtra("searchBranch", -1));
                break;
            default:
                bookCache = searcher.bookCache;
                break;
        }
        setLoading(searcher.state != Search.SEARCHER_STATE_STOPPED);
        setTitle(tr(R.string.search_loading));
    }

    @Override
    protected void onResume() {
        super.onResume();
        if (searcher != null) {
            if (!cacheShown && bookCache != null)
                displayBookCache(Math.max(bookCache.size(), searcher.totalResultCount));
            for (Bridge.SearchEvent event: searcher.pendingEvents)
                onSearchEvent(searcher, event);
            searcher.pendingEvents.clear();
        }
    }

    @Override
    public boolean onPrepareOptionsMenu(Menu menu) {
        boolean x= super.onPrepareOptionsMenu(menu);
        menu.findItem(R.id.search).setVisible(false);
        return x;
    }

    @Override
    protected void onDestroy() {
        super.onDestroy();
    }

    @Override
    public boolean onSearchEvent(Bridge.SearcherAccess access, Bridge.SearchEvent event) {
        if (access != searcher) return false;
        searcher.heartBeat = System.currentTimeMillis();
        switch (event.kind) {
            case CONNECTED:
                access.state = Search.SEARCHER_STATE_CONNECTED;
                access.homeBranches = (String[])event.obj1;
                access.searchBranches = (String[])event.obj2;
                searcher.waitingForDetails = -1;
                searcher.nextDetailsRequested = -1;
                searcher.start((Bridge.Book) getIntent().getSerializableExtra("searchQuery"), getIntent().getIntExtra("homeBranch", -1), getIntent().getIntExtra("searchBranch", -1));
                return true;
            case FIRST_PAGE: //obj1 = Book[] books
                searcher.state = Search.SEARCHER_STATE_SEARCHING;
                onSearchFirstPageComplete((Bridge.Book[])event.obj1);
                break;
            case NEXT_PAGE:  //obj1 = Book[] books
                onSearchNextPageComplete((Bridge.Book[])event.obj1);
                break;
            case DETAILS:    //obj1 = Book book
                onSearchDetailsComplete((Bridge.Book)event.obj1);
                break;
            case ORDER_COMPLETE: //obj1 = Book book
                onOrderComplete((Bridge.Book)event.obj1);
                break;
            case ORDER_CONFIRM:  //obj1 = Book book
                onOrderConfirm((Bridge.Book)event.obj1);
                break;
            case TAKE_PENDING_MESSAGE: //arg1 = int kind, obj1 = String caption, obj2 = String[] options
                onTakePendingMessage(event.arg1, (String)(event.obj1), (String[])event.obj2);
                break;
            case PENDING_MESSAGE_COMPLETE:
                onPendingMessageCompleted();
                break;
            case EXCEPTION:
                setLoading(false);
                setTitle(tr(R.string.search_failed));
                searcher.state = Search.SEARCHER_STATE_STOPPED;
                searcher = null;
                Search.gcSearchers();
                VideLibriApp.showPendingExceptions();
                return true;
        }
        return true;
    }

    public void onSearchFirstPageComplete(final Bridge.Book[] books) {
        searcher.bookCache.clear();
        for (Bridge.Book b : books) searcher.bookCache.add(b);
        bookCache = searcher.bookCache;
        int realCount = Math.max(searcher.totalResultCount, bookCache.size());
        displayBookCache(searcher.totalResultCount);
        setTitle(tr(R.string.search_resultcountD,  realCount));
        setLoading(false || searcher.orderingAccount != null);
    }

    public void onSearchNextPageComplete(final Bridge.Book[] books) {
        for (Bridge.Book b: books) searcher.bookCache.add(b);
        bookCache = searcher.bookCache;
        updateDisplayBookCache();
        setLoading(searcher.waitingForDetails != -1 || searcher.orderingAccount != null);
    }


    public void onSearchDetailsComplete(final Bridge.Book book) {
        int oldWaitingForDetails = searcher.waitingForDetails;
        searcher.waitingForDetails = -1; //search has ended

        book.setProperty("__details", "");
        if (oldWaitingForDetails >= 0 && oldWaitingForDetails < searcher.bookCache.size())
            searcher.bookCache.set(oldWaitingForDetails, book); //still save the search result, so it does not need to be searched again

        setLoading(false || searcher.orderingAccount != null);

        if (detailsVisible()) {
            if (searcher.nextDetailsRequested == -1)
                return;
            if (searcher.nextDetailsRequested != oldWaitingForDetails) {
                searcher.waitingForDetails = searcher.nextDetailsRequested;
                setLoading(true);
                if (searcher == null) return;
                searcher.details(bookCache.get(searcher.waitingForDetails));
                return;
            }

            details.setBook(book);
        }

    }

    public void onOrderConfirm(final Bridge.Book book) {
        //see bookSearchForm.pas
        String question = book.getProperty("orderConfirmation").replace("\\n", "\n");
        String orderConfirmationOptionTitles = book.getProperty("orderConfirmationOptionTitles");

        book.account = searcher.orderingAccount;

        setLoading(searcher.waitingForDetails != -1 || false);
        if (bookActionButton != null) bookActionButton.setClickable(true);

        if (question == null || "".equals(question))
            searcher.orderConfirmed(book);
        else if (orderConfirmationOptionTitles == null || "".equals(orderConfirmationOptionTitles)) {
            lastSelectedBookForDialog = book;
            Util.showMessageYesNo(DialogId.SEARCHER_ORDER_CONFIRM, question);
        } else {
            final String[] options = orderConfirmationOptionTitles.split("\\\\[|]");
            lastSelectedBookForDialog = book;
            Util.chooseDialog(DialogId.SEARCHER_CHOOSE_ORDER, question, options);
        }
    }

    public void onTakePendingMessage(final int kind, final String caption, final String[] options){
        setLoading(searcher.waitingForDetails != -1 || false);
        switch (kind) {
            case 1: Util.showMessageYesNo(DialogId.SEARCHER_MESSAGE_CONFIRM, caption); break;
            case 2: Util.chooseDialog(DialogId.SEARCHER_MESSAGE_CHOOSE, caption, options);
        }
    }
    public void onPendingMessageCompleted(){
        setLoading(searcher.waitingForDetails != -1 || false);
    }


    public void onOrderComplete(final Bridge.Book book) {
        Util.showMessage(tr(R.string.search_orderedokS, book.title));
        VideLibriApp.displayAccount(searcher.orderingAccount);
        if (searcher.orderingAccount != null)
            VideLibriApp.updateAccount(searcher.orderingAccount, false, false); //full update, so the book is only shown if it worked, and canceling work
        searcher.orderingAccount = null; //???? todo
        setLoading(searcher.waitingForDetails != -1 || false);
        if (bookActionButton != null) bookActionButton.setClickable(true);
    }


    public void onPlaceHolderShown(int position){
        if (searcher == null) return;
        if (searcher.nextPageAvailable) {
            searcher.nextPageAvailable = false;
            setLoading(true);
            searcher.nextPage();
        }
    }

    @Override
    public void viewDetails(int bookpos) {
        super.viewDetails(bookpos);    //To change body of overridden methods use File | Settings | File Templates.
        if (searcher == null) return;
        if (bookCache.get(bookpos).hasProperty("__details"))    {
            searcher.nextDetailsRequested = -1;
            return;
        }
        searcher.nextDetailsRequested = bookpos;
        if (searcher.waitingForDetails == -1) {
            searcher.waitingForDetails = bookpos;
            setLoading(true);
            searcher.details(bookCache.get(searcher.waitingForDetails));
        }
    }


    public void orderBook(final Bridge.Book book, int choosenOrder){
        if (searcher == null) return;
        book.setProperty("choosenOrder", "" + choosenOrder);
        final java.util.ArrayList<Bridge.Account> matchingAccounts = new java.util.ArrayList();
        for (Bridge.Account acc: VideLibriApp.accounts)
            if (acc.libId.equals(libId) && acc.name != null && !acc.name.equals(""))
                matchingAccounts.add(acc);
        if (matchingAccounts.size() == 0) {
            Util.showMessage(tr(R.string.search_needaccount));
            return;
        }
        if (matchingAccounts.size() > 1) {
            String [] temp = new String[matchingAccounts.size()];
            for (int i=0;i<matchingAccounts.size();i++)
                temp[i] = matchingAccounts.get(i).prettyName;
            lastSelectedBookForDialog = book;
            lastMatchingAccountsForDialog = matchingAccounts;
            Util.chooseDialog(DialogId.SEARCHER_CHOOSE_TARGET_ACCOUNT, tr(R.string.search_orderTargetAccount), temp);
        } else {
            book.account = matchingAccounts.get(0);
            searcher.orderingAccount = book.account;
            if (bookActionButton != null) bookActionButton.setClickable(false);
            setLoading(true);
            searcher.order(book);
        }
    }

    @Override
    public void onBookActionButtonClicked(final Bridge.Book book){
        if (book != null && book.isOrderable()) {
            int orders = 1;
            try{
                orders = Integer.parseInt(book.getProperty("orderable"));
            } catch (NumberFormatException e) {
            }

            if (orders == 1) orderBook(book, 0);
            else {
                final String versions[] = new String[orders];
                for (int i=0;i<orders;i++)
                    versions[i] = book.getProperty("orderTitle"+i);
                lastSelectedBookForDialog = book;
                Util.chooseDialog(DialogId.SEARCHER_CHOOSE_ORRERTITLE, tr(R.string.search_chooseitem), versions);
            }


        }
    }



    private static Bridge.Book lastSelectedBookForDialog = null;
    private static ArrayList<Bridge.Account> lastMatchingAccountsForDialog = null;
    @Override
    boolean onDialogResult(int dialogId, int buttonId, Bundle more) {
        switch (dialogId) {
            case DialogId.SEARCHER_MESSAGE_CONFIRM:
                setLoading(true);
                searcher.completePendingMessage( buttonId == DialogInterface.BUTTON_POSITIVE ? 1 : 0 );
                return true;
            case DialogId.SEARCHER_MESSAGE_CHOOSE:
                setLoading(true);
                searcher.completePendingMessage( buttonId );
            case DialogId.SEARCHER_ORDER_CONFIRM:
                if (lastSelectedBookForDialog == null) break;
                if (buttonId == DialogInterface.BUTTON_POSITIVE) {
                    setLoading(true);
                    if (bookActionButton != null) bookActionButton.setClickable(false);
                    searcher.orderConfirmed(lastSelectedBookForDialog);
                }
                lastSelectedBookForDialog = null;
                return true;
            case DialogId.SEARCHER_CHOOSE_ORDER:
                if (lastSelectedBookForDialog == null) break;
                if (buttonId >= 0 /*&& buttonId < options.length*/) {
                        lastSelectedBookForDialog.setProperty("choosenConfirmation", (buttonId+1)+"");
                    setLoading(true);
                    if (bookActionButton != null) bookActionButton.setClickable(false);
                    searcher.orderConfirmed(lastSelectedBookForDialog);
                }
                lastSelectedBookForDialog = null;
                return true;
            case DialogId.SEARCHER_CHOOSE_ORRERTITLE:
                if (lastSelectedBookForDialog == null) break;
                if (buttonId >= 0 /*&& i < versions.length*/)
                    orderBook(lastSelectedBookForDialog, buttonId);
                return true;
            case DialogId.SEARCHER_CHOOSE_TARGET_ACCOUNT:
                if (lastSelectedBookForDialog == null || lastMatchingAccountsForDialog == null) break;
                if (buttonId >= 0 && buttonId < lastMatchingAccountsForDialog.size()) {
                    lastSelectedBookForDialog.account = lastMatchingAccountsForDialog.get(buttonId);
                    searcher.orderingAccount = lastSelectedBookForDialog.account; //this property is lost on roundtrip, saved it on java side
                    if (bookActionButton != null) bookActionButton.setClickable(false);
                    setLoading(true);
                    searcher.order(lastSelectedBookForDialog);
                }
                lastSelectedBookForDialog = null;
                lastMatchingAccountsForDialog = null;
                return true;
        }
        return super.onDialogResult(dialogId, buttonId, more);
    }
}
