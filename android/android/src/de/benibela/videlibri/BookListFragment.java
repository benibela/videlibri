package de.benibela.videlibri;

import android.widget.*;

import java.text.DateFormat;

public class BookListFragment extends VideLibriFakeFragment{
    private ListView bookListView;
    private DateFormat dateFormatDefault;

    BookListFragment(final BookListActivity activity){
        super(activity);

        bookListView = (ListView) findViewById(R.id.booklistview);
        if (bookListView != null) activity.registerForContextMenu(bookListView);
        dateFormatDefault = android.text.format.DateFormat.getDateFormat(activity);
    }


    BookOverviewAdapter getAdapter(){
        return (BookOverviewAdapter) bookListView.getAdapter();
    }

    String exportShare(boolean html){
        BookOverviewAdapter adapter = getAdapter();
        return adapter.exportShare(html);
    }

}
