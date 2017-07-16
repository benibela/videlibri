package de.benibela.videlibri;

import android.os.Bundle;
import android.text.Editable;
import android.text.TextWatcher;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.*;

import java.text.DateFormat;
import java.util.ArrayList;

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
