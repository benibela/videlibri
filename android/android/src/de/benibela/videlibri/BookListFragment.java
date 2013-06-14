package de.benibela.videlibri;

import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ArrayAdapter;
import android.widget.Button;
import android.widget.ListView;
import android.widget.TextView;

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
        dateFormatDefault = android.text.format.DateFormat.getDateFormat(getSherlockActivity());

        if (getActivity() instanceof RenewList) {
            //if (l == null) return;
            final RenewList rl = ((RenewList)getActivity());
            rl.button = (Button) view.findViewById(R.id.button);
            //if (button == null) return;
            rl.button.setVisibility(View.VISIBLE);
            rl.button.setEnabled(false);
            rl.button.setText("noch keine Auswahl");
            rl.button.setOnClickListener(new View.OnClickListener() {
                @Override
                public void onClick(View view) {
                    Bridge.VLBookOperation((Bridge.Book[]) rl.selectedBooks.toArray(), Bridge.BOOK_OPERATION_RENEW);
                    rl.finish();
                }
            });
        }

        return view;
    }
}
