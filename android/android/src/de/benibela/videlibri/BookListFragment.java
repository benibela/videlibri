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
        dateFormatDefault = android.text.format.DateFormat.getDateFormat(activity);
    }
}
