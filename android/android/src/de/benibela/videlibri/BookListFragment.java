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

/**
 * Created with IntelliJ IDEA.
 * User: benito
 * Date: 5/28/13
 * Time: 12:54 PM
 * To change this template use File | Settings | File Templates.
 */
public class BookListFragment extends VideLibriBaseFragment {
    private ListView bookListView;
    private DateFormat dateFormatDefault;

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container,
                             Bundle savedInstanceState) {
        View view = inflater.inflate(R.layout.booklist, container, false);

        bookListView = (ListView) view.findViewById(R.id.booklistview);
        dateFormatDefault = android.text.format.DateFormat.getDateFormat(getActivity());

        final android.support.v4.app.FragmentActivity act = getActivity();
        if (act instanceof RenewList) {
            //if (l == null) return;
            final RenewList rl = ((RenewList)act);
            rl.button = (Button) view.findViewById(R.id.button);
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
        if (act instanceof VideLibri) {
            //final VideLibri vl = (VideLibri) act;
            view.findViewById(R.id.searchFilterPanel).setVisibility(View.VISIBLE);
            EditText et = (EditText) view.findViewById(R.id.searchFilter);
            et.addTextChangedListener(new TextWatcher() {
                @Override
                public void beforeTextChanged(CharSequence charSequence, int i, int i2, int i3) {

                }

                @Override
                public void onTextChanged(CharSequence charSequence, int i, int i2, int i3) {

                }

                @Override
                public void afterTextChanged(Editable editable) {
                    ((VideLibri)act).setFilter(editable.toString());
                }
            });
        }

        return view;
    }
}
