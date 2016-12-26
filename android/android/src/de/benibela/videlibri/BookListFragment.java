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

        if (activity instanceof RenewList) {
            //if (l == null) return;
            final RenewList rl = ((RenewList)activity);
            rl.button = (Button) findViewById(R.id.buttonbelowlist);
            //if (button == null) return;
            rl.button.setVisibility(View.VISIBLE);
            rl.button.setEnabled(false);
            rl.button.setText(tr(R.string.booklist_noselection));
            rl.button.setOnClickListener(new View.OnClickListener() {
                @Override
                public void onClick(View view) {
                    VideLibriApp.renewBooks((Bridge.Book[]) rl.selectedBooks.toArray(new Bridge.Book[0]));
                    rl.finish();
                }
            });
        }
        if (activity instanceof VideLibri) {
            //final VideLibri vl = (VideLibri) act;
            findViewById(R.id.searchFilterPanel).setVisibility(View.VISIBLE);
            EditText et = (EditText) findViewById(R.id.searchFilter);
            et.addTextChangedListener(new TextWatcher() {
                @Override
                public void beforeTextChanged(CharSequence charSequence, int i, int i2, int i3) {

                }

                @Override
                public void onTextChanged(CharSequence charSequence, int i, int i2, int i3) {

                }

                @Override
                public void afterTextChanged(Editable editable) {
                    ((VideLibri)activity).setFilter(editable.toString());
                }
            });
        }
    }
}
