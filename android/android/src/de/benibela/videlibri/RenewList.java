package de.benibela.videlibri;

import android.os.Bundle;
import android.view.View;
import android.widget.Button;
import android.widget.ListView;

import java.util.ArrayList;

public class RenewList extends BookListActivity {
    public Button button;
    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        setTitle("Verlängern");
        selectedBooks = new ArrayList<Bridge.Book>();
    }

    @Override
    protected void onResume() {
        super.onResume();
        bookCache = new ArrayList<Bridge.Book> ();
        noDetailsInOverview = VideLibri.instance.noDetailsInOverview;
        for (Bridge.Book book: VideLibri.instance.bookCache)
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
            setTitle("Verlängern: Auswahl");
            if (button != null) {
                button.setEnabled(false);
                button.setText("noch keine Auswahl");
            }
        } else {
            setTitle("Verlängern: " + selectedBooks.size() + "/"+bookCache.size());
            if (button != null) {
                button.setEnabled(true);
                button.setText(selectedBooks.size()+ " verlängern");
                //button.setBackgroundDrawable(activity.getResources().getDrawable(android.R.drawable.butt)); //http://stackoverflow.com/questions/4384890/how-to-disable-an-android-button
            }
        }
        ((BookOverviewAdapter)((ListView) findViewById(R.id.booklistview)).getAdapter()).notifyDataSetChanged();
        if (!port_mode) super.viewDetails(bookpos);
    }
}
