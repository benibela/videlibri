package de.benibela.videlibri;

import android.os.Bundle;
import android.preference.PreferenceManager;
import android.view.View;
import android.widget.Button;
import android.widget.ListView;

import java.util.ArrayList;

public class RenewList extends BookListActivity {
    public Button button;
    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        setTitle(tr(R.string.renew_title_start));
        selectedBooks = new ArrayList<Bridge.Book>();
    }

    @Override
    protected void onResume() {
        super.onResume();
        bookCache = new ArrayList<Bridge.Book> ();
        noDetailsInOverview = PreferenceManager.getDefaultSharedPreferences(this).getBoolean("noLendBookDetails", false);
        for (Bridge.Book book: VideLibri.makeUpdatedBookCache(null, new ArrayList<Bridge.Book>()))
            if (!book.history) {
                Bridge.Book.StatusEnum status = book.getStatus();
                if (status == Bridge.Book.StatusEnum.Unknown || status == Bridge.Book.StatusEnum.Normal)
                    bookCache.add(book);
            }
        displayBookCache();
    }

    @Override
    public void viewDetails(int bookpos) {
        boolean deleted = false;
        for (int i=0;i<selectedBooks.size(); i++ )
            if (selectedBooks.get(i) == bookCache.get(bookpos))  {
                selectedBooks.remove(i);
                deleted = true;
                break;
            }
        if (!deleted)
            selectedBooks.add(bookCache.get(bookpos));
        if (selectedBooks.size() == 0) {
            setTitle(tr(R.string.renew_title_select));
            if (button != null) {
                button.setEnabled(false);
                button.setText(tr(R.string.renew_noselection));
            }
        } else {
            setTitle(tr(R.string.renew_title_selectionDD, selectedBooks.size(), bookCache.size()));
            if (button != null) {
                button.setEnabled(true);
                button.setText(tr(R.string.renew_renewD, selectedBooks.size()));
                //button.setBackgroundDrawable(activity.getResources().getDrawable(android.R.drawable.butt)); //http://stackoverflow.com/questions/4384890/how-to-disable-an-android-button
            }
        }
        ((BookOverviewAdapter)((ListView) findViewById(R.id.booklistview)).getAdapter()).notifyDataSetChanged();
        if (!port_mode) super.viewDetails(bookpos);
    }
}
