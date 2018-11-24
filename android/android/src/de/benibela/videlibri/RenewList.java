package de.benibela.videlibri;

import android.content.SharedPreferences;
import android.os.Bundle;
import android.preference.PreferenceManager;
import android.view.View;
import android.widget.Button;
import android.widget.ListView;

import java.util.ArrayList;

import de.benibela.videlibri.jni.Bridge;

public class RenewList extends BookListActivity {
    public Button button;

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        setTitle(tr(R.string.renew_title_start));
        selectedBooks = new ArrayList<>();
        updateViewFilters();


        button = (Button) findViewById(R.id.buttonbelowlist);
        //if (button == null) return;
        button.setVisibility(View.VISIBLE);
        button.setEnabled(false);
        button.setText(tr(R.string.booklist_noselection));
        button.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View view) {
                VideLibriApp.renewBooks(selectedBooks.toArray(new Bridge.Book[0]));
                finish();
            }
        });
    }

    @Override
    protected void onResume() {
        super.onResume();
        updateViewFilters();
        displayBookCache();
    }

    @Override
    public void onBookCacheAvailable() {
        super.onBookCacheAvailable();
        updateRenewButton();
    }

    private int truecount;

    private void updateViewFilters(){
        SharedPreferences sp = PreferenceManager.getDefaultSharedPreferences(this);
        setOption(BookOverviewAdapter.DisplayEnum.NoDetails, sp.getBoolean("noLendBookDetails", false));
        setOption(BookOverviewAdapter.DisplayEnum.ShowRenewCount, sp.getBoolean("showRenewCount", true));
        sortingKey = sp.getString("sorting", "dueDate");
        groupingKey = sp.getString("grouping", "_dueWeek");
        ArrayList<Bridge.Book> oldSelection = selectedBooks;
        bookCache = LendingList.makePrimaryBookCache(false, true);
        truecount = bookCache.size();
        bookCache = LendingList.filterToSecondaryBookCache(bookCache, groupingKey, sortingKey, "", null);
        selectedBooks = new ArrayList<>(selectedBooks.size());
        for (Bridge.Book selbook: oldSelection)
            for (Bridge.Book book: bookCache)
                if (selbook.equalsBook(book)) {
                    selectedBooks.add(book);
                    break;
                }
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
        updateRenewButton();
        ((BookOverviewAdapter)((ListView) findViewById(R.id.booklistview)).getAdapter()).notifyDataSetChanged();
        if (!port_mode) super.viewDetails(bookpos);
    }


    private void updateRenewButton(){
        if (selectedBooks.size() == 0) {
            setTitle(tr(R.string.renew_title_select));
            if (button != null) {
                button.setEnabled(false);
                button.setText(tr(R.string.renew_noselection));
            }
        } else {
            setTitle(tr(R.string.renew_title_selectionDD, selectedBooks.size(), truecount));
            if (button != null) {
                button.setEnabled(true);
                button.setText(tr(R.string.renew_renewD, selectedBooks.size()));
                //button.setBackgroundDrawable(activity.getResources().getDrawable(android.R.drawable.butt)); //http://stackoverflow.com/questions/4384890/how-to-disable-an-android-button
            }
        }
    }
}
