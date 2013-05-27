package de.benibela.videlibri;

import android.os.Bundle;
import android.util.Log;
import de.benibela.videlibri.BookListActivity;
import de.benibela.videlibri.Bridge;
import de.benibela.videlibri.VideLibriBaseActivity;


public class SearchResult extends BookListActivity implements Bridge.SearchResultDisplay  {

    Bridge.SearcherAccess searcher;
    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        Bridge.Book book = (Bridge.Book) getIntent().getSerializableExtra("searchQuery");
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

                if (BookDetails.instance != null) {
                    if (nextDetailsRequested == -1)
                        return;
                    if (nextDetailsRequested != oldWaitingForDetails) {
                        waitingForDetails = nextDetailsRequested;
                        setLoading(true);
                        searcher.details(bookCache.get(waitingForDetails));
                        return;
                    }

                    BookDetails.instance.setBook(book);
                }
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
}
