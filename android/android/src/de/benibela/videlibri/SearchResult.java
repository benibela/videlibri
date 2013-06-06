package de.benibela.videlibri;

import android.content.DialogInterface;
import android.os.Bundle;
import android.util.Log;
import de.benibela.videlibri.BookListActivity;
import de.benibela.videlibri.Bridge;
import de.benibela.videlibri.VideLibriBaseActivity;


public class SearchResult extends BookListActivity implements Bridge.SearchResultDisplay  {

    Bridge.SearcherAccess searcher;
    String libId = "";
    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        Bridge.Book book = (Bridge.Book) getIntent().getSerializableExtra("searchQuery");
        libId = book.account.libId;
        if (book == null) { Log.i("VideLibri", "search without book. Abort."); finish(); return; }

        searcher = new Bridge.SearcherAccess(this, book);
        waitingForDetails = -1;
        nextDetailsRequested = -1;
        setLoading(true);
        setTitle("Suche läuft...");
    }

    @Override
    protected void onResume() {
        super.onResume();

        nextDetailsRequested = -1;
    }

    @Override
    public void onSearchFirstPageComplete(final Bridge.Book[] books) {
        runOnUiThread(new Runnable() {
            @Override
            public void run() {
                bookCache.clear();
                for (Bridge.Book b : books) bookCache.add(b);
                displayBookCache(searcher.totalResultCount);
                setTitle(searcher.totalResultCount+" Treffer");
                setLoading(false);
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
                setLoading(waitingForDetails != -1);
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

                setLoading(false);

                if (details() != null) {
                    if (nextDetailsRequested == -1)
                        return;
                    if (nextDetailsRequested != oldWaitingForDetails) {
                        waitingForDetails = nextDetailsRequested;
                        setLoading(true);
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

                if (question == null || "".equals(question))
                    searcher.orderConfirmed(book);
                else if (orderConfirmationOptionTitles == null || "".equals(orderConfirmationOptionTitles)) {
                    showMessageYesNo(question, new MessageHandler() {
                        @Override
                        public void onDialogEnd(DialogInterface dialogInterface, int i) {
                            if (i == DialogInterface.BUTTON_POSITIVE) {
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
                                searcher.orderConfirmed(book);
                            }
                        }
                    });
                }
            }
        });
    }

    @Override
    public void onOrderComplete(final Bridge.Book book) {
        runOnUiThread(new Runnable() {
            @Override
            public void run() {
                showMessage("Das Buch \""+book.title+"\" wurde ohne Fehler vorbestellt.");
                if (VideLibri.instance != null)
                    if (orderingAccount == null) VideLibri.instance.displayAccount(null);
                    else VideLibri.updateAccount(orderingAccount, false, false); //full update, so the book is only shown if it worked, and canceling works
            }
        });
    }

    @Override
    public void onException() {
        //To change body of implemented methods use File | Settings | File Templates.
        setLoading(false);
    }

    public void onPlaceHolderShown(int position){
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
            searcher.details(bookCache.get(waitingForDetails));
        }
    }

    private Bridge.Account orderingAccount;

    public void orderBook(final Bridge.Book book, int choosenOrder){
        book.setProperty("choosenOrder", "" + choosenOrder);
        final java.util.ArrayList<Bridge.Account> matchingAccounts = new java.util.ArrayList();
        for (Bridge.Account acc: VideLibri.accounts)
            if (acc.libId.equals(libId) && acc.name != null && !acc.name.equals(""))
                matchingAccounts.add(acc);
        if (matchingAccounts.size() == 0) {
            showMessage("Bestellungen benötigen ein registriertes Bibliothekskonto in VideLibri.");
            return;
        }
        if (matchingAccounts.size() > 1) {
            String [] temp = new String[matchingAccounts.size()];
            for (int i=0;i<matchingAccounts.size();i++)
                temp[i] = matchingAccounts.get(i).prettyName;

            Util.chooseDialog(this, "Für welches Konto soll es bestellt werden?", temp, new MessageHandler() {
                @Override
                public void onDialogEnd(DialogInterface dialogInterface, int i) {
                    if (i >= 0 && i < matchingAccounts.size()) {
                        book.account = matchingAccounts.get(i);
                        orderingAccount = book.account; //this property is lost on roundtrip, saved it on java side
                        searcher.order(book);
                    }
                }
            });
        } else {
            book.account = matchingAccounts.get(0);
            orderingAccount = book.account;
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
                Util.chooseDialog(this, "Welches Exemplar wollen Sie vorbestellen?", versions, new MessageHandler() {
                    @Override
                    public void onDialogEnd(DialogInterface dialogInterface, int i) {
                        if (i >= 0 && i < versions.length)
                            orderBook(book, i);
                    }
                });
            }


        }
    }
}
