package de.benibela.videlibri;

import android.content.DialogInterface;
import android.os.Bundle;
import android.util.Log;
import com.actionbarsherlock.view.Menu;
import de.benibela.videlibri.BookListActivity;
import de.benibela.videlibri.Bridge;
import de.benibela.videlibri.VideLibriBaseActivity;

import java.util.Arrays;

public class SearchResult extends BookListActivity implements Bridge.SearchResultDisplay  {

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
        searcher.setDisplay(this);
        searcher.start(book, getIntent().getIntExtra("homeBranch", -1), getIntent().getIntExtra("searchBranch", -1));
        waitingForDetails = -1;
        nextDetailsRequested = -1;
        setLoading(true);
        setTitle(tr(R.string.search_loading));
    }

    @Override
    protected void onResume() {
        super.onResume();

        nextDetailsRequested = -1;
    }

    @Override
    public boolean onPrepareOptionsMenu(Menu menu) {
        boolean x= super.onPrepareOptionsMenu(menu);
        menu.findItem(R.id.search).setVisible(false);
        return x;
    }

    @Override
    protected void onDestroy() {
        Search.removeSearcherOwner(this);
        searcher = null;
        super.onDestroy();
    }

    @Override
    public void onSearchFirstPageComplete(final Bridge.Book[] books) {
        runOnUiThread(new Runnable() {
            @Override
            public void run() {
                if (searcher == null) return;
                bookCache.clear();
                for (Bridge.Book b : books) bookCache.add(b);
                displayBookCache(searcher.totalResultCount);
                setTitle(tr(R.string.search_resultcountD,  Math.max(searcher.totalResultCount, bookCache.size())));
                setLoading(false || orderingAccount != null);
            }
        });
    }

    @Override
    public void onSearchNextPageComplete(final Bridge.Book[] books) {
        runOnUiThread(new Runnable() {
            @Override
            public void run() {
                for (Bridge.Book b: books) bookCache.add(b);
                updateDisplayBookCache();
                setLoading(waitingForDetails != -1 || orderingAccount != null);
            }
        });
    }

    //The detail search runs in the background, for a single book.
    //But the user might request other detail searches, before the search is complete.
    //Then wait for the old search to complete, and then start the newest search, unless the user has closed the view
    int waitingForDetails;    //nr of book currently searched. Only set when the search is started or has ended (-1 if no search is running)
    int nextDetailsRequested; //nr of the book that *should* be searched. Set when requesting a new search, or to -1 to cancel the current search

    @Override
    public void onSearchDetailsComplete(final Bridge.Book book) {
        runOnUiThread(new Runnable() {
            @Override
            public void run() {
                int oldWaitingForDetails = waitingForDetails;
                waitingForDetails = -1; //search has ended

                book.setProperty("__details", "");
                bookCache.set(oldWaitingForDetails, book); //still save the search result, so it does not need to be searched again

                setLoading(false || orderingAccount != null);

                if (details() != null) {
                    if (nextDetailsRequested == -1)
                        return;
                    if (nextDetailsRequested != oldWaitingForDetails) {
                        waitingForDetails = nextDetailsRequested;
                        setLoading(true);
                        if (searcher == null) return;
                        searcher.details(bookCache.get(waitingForDetails));
                        return;
                    }

                    details().setBook(book);
                }
            }
        });
    }

    @Override
    public void onOrderConfirm(final Bridge.Book book) {
        runOnUiThread(new Runnable() {
            @Override
            public void run() {
                //see bookSearchForm.pas
                String question = book.getProperty("orderConfirmation").replace("\\n", "\n");
                String orderConfirmationOptionTitles = book.getProperty("orderConfirmationOptionTitles");

                book.account = orderingAccount;

                setLoading(waitingForDetails != -1 || false);
                if (bookActionButton != null) bookActionButton.setClickable(true);

                if (question == null || "".equals(question))
                    searcher.orderConfirmed(book);
                else if (orderConfirmationOptionTitles == null || "".equals(orderConfirmationOptionTitles)) {
                    showMessageYesNo(question, new MessageHandler() {
                        @Override
                        public void onDialogEnd(DialogInterface dialogInterface, int i) {
                            if (i == DialogInterface.BUTTON_POSITIVE) {
                                setLoading(true);
                                if (bookActionButton != null) bookActionButton.setClickable(false);
                                searcher.orderConfirmed(book);
                            }
                        }
                    });
                } else {
                    final String[] options = orderConfirmationOptionTitles.split("\\\\[|]");
                    Util.chooseDialog(SearchResult.this, question, options,new MessageHandler() {
                        @Override
                        public void onDialogEnd(DialogInterface dialogInterface, int i) {
                            if (i >= 0 && i < options.length) {
                                book.setProperty("choosenConfirmation", (i+1)+"");
                                setLoading(true);
                                if (bookActionButton != null) bookActionButton.setClickable(false);
                                searcher.orderConfirmed(book);
                            }
                        }
                    });
                }
            }
        });
    }

    public void onTakePendingMessage(final int kind, final String caption, final String[] options){
        runOnUiThread(new Runnable() {
            @Override
            public void run() {
                setLoading(waitingForDetails != -1 || false);
                switch (kind) {
                    case 1: showMessageYesNo(caption, new MessageHandler() {
                        @Override
                        public void onDialogEnd(DialogInterface dialogInterface, int i) {
                         setLoading(true);
                            searcher.completePendingMessage( i == DialogInterface.BUTTON_POSITIVE ? 1 : 0 );
                        }
                    });
                        break;
                    case 2:
                        Util.chooseDialog(SearchResult.this, caption, options,new MessageHandler() {
                            @Override
                            public void onDialogEnd(DialogInterface dialogInterface, int i) {
                                setLoading(true);
                                searcher.completePendingMessage( i );
                            }
                        });
                }
            }
        });
    }
    public void onPendingMessageCompleted(){
        runOnUiThread(new Runnable() {
            @Override
            public void run() {
                setLoading(waitingForDetails != -1 || false);
            }
        });

    }


    @Override
    public void onOrderComplete(final Bridge.Book book) {
        runOnUiThread(new Runnable() {
            @Override
            public void run() {
                showMessage(tr(R.string.search_orderedokS, book.title));
                VideLibriApp.displayAccount(orderingAccount);
                if (orderingAccount != null)
                    VideLibriApp.updateAccount(orderingAccount, false, false); //full update, so the book is only shown if it worked, and canceling work
                orderingAccount = null;
                setLoading(waitingForDetails != -1 || false);
                if (bookActionButton != null) bookActionButton.setClickable(true);
            }
        });
    }

    @Override
    public void onException() {
        runOnUiThread(new Runnable() {
            @Override
            public void run() {
                setLoading(false);
                setTitle(tr(R.string.search_failed));
                VideLibriApp.showPendingExceptions();
            }
        });
    }

    public void onPlaceHolderShown(int position){
        if (searcher == null) return;
        if (searcher.nextPageAvailable) {
            setLoading(true);
            searcher.nextPage();
        }
        searcher.nextPageAvailable = false;
    }

    @Override
    public void viewDetails(int bookpos) {
        super.viewDetails(bookpos);    //To change body of overridden methods use File | Settings | File Templates.
        if (bookCache.get(bookpos).hasProperty("__details"))    {
            nextDetailsRequested = -1;
            return;
        }
        nextDetailsRequested = bookpos;
        if (waitingForDetails == -1) {
            waitingForDetails = bookpos;
            setLoading(true);
            if (searcher == null) return;
            searcher.details(bookCache.get(waitingForDetails));
        }
    }

    private Bridge.Account orderingAccount;

    public void orderBook(final Bridge.Book book, int choosenOrder){
        if (searcher == null) return;
        book.setProperty("choosenOrder", "" + choosenOrder);
        final java.util.ArrayList<Bridge.Account> matchingAccounts = new java.util.ArrayList();
        for (Bridge.Account acc: VideLibriApp.accounts)
            if (acc.libId.equals(libId) && acc.name != null && !acc.name.equals(""))
                matchingAccounts.add(acc);
        if (matchingAccounts.size() == 0) {
            showMessage(tr(R.string.search_needaccount));
            return;
        }
        if (matchingAccounts.size() > 1) {
            String [] temp = new String[matchingAccounts.size()];
            for (int i=0;i<matchingAccounts.size();i++)
                temp[i] = matchingAccounts.get(i).prettyName;

            Util.chooseDialog(this, tr(R.string.search_orderTargetAccount), temp, new MessageHandler() {
                @Override
                public void onDialogEnd(DialogInterface dialogInterface, int i) {
                    if (i >= 0 && i < matchingAccounts.size()) {
                        book.account = matchingAccounts.get(i);
                        orderingAccount = book.account; //this property is lost on roundtrip, saved it on java side
                        if (bookActionButton != null) bookActionButton.setClickable(false);
                        setLoading(true);
                        searcher.order(book);
                    }
                }
            });
        } else {
            book.account = matchingAccounts.get(0);
            orderingAccount = book.account;
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
                Util.chooseDialog(this, tr(R.string.search_chooseitem), versions, new MessageHandler() {
                    @Override
                    public void onDialogEnd(DialogInterface dialogInterface, int i) {
                        if (i >= 0 && i < versions.length)
                            orderBook(book, i);
                    }
                });
            }


        }
    }

    @Override
    public void onConnected(String[] homeBranches, String[] searchBranches) {

    }



}
