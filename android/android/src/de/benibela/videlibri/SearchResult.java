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
    }

    @Override
    public void onSearchFirstPageComplete(final Bridge.Book[] books) {
        runOnUiThread(new Runnable() {
            @Override
            public void run() {
                bookCache.clear();
                for (Bridge.Book b : books) bookCache.add(b);
                displayBookCache(searcher.totalResultCount);
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
            }
        });
    }

    @Override
    public void onSearchDetailsComplete(Bridge.Book book) {
        //To change body of implemented methods use File | Settings | File Templates.
    }

    @Override
    public void onException() {
        //To change body of implemented methods use File | Settings | File Templates.
    }

    public void onPlaceHolderShown(int position){
        if (searcher.nextPageAvailable) searcher.nextPage();
        searcher.nextPageAvailable = false;
    }
}
