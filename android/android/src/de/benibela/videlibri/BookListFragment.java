package de.benibela.videlibri;

import android.widget.*;

public class BookListFragment extends VideLibriFakeFragment{
    private ListView bookListView;

    BookListFragment(final BookListActivityOld activity){
        super(activity);

        bookListView = (ListView) findViewById(R.id.booklistview);
        if (bookListView != null) activity.registerForContextMenu(bookListView);
    }


    BookOverviewAdapter getAdapter(){
        return (BookOverviewAdapter) bookListView.getAdapter();
    }

    String exportShare(boolean html){
        BookOverviewAdapter adapter = getAdapter();
        return adapter.exportShare(html);
    }

}
