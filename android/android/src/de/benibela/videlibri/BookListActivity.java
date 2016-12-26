package de.benibela.videlibri;

import android.app.Activity;
import android.support.v4.app.FragmentTransaction;
import android.content.Intent;
import android.os.Bundle;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.*;

import java.text.DateFormat;
import java.util.ArrayList;
import java.util.EnumSet;
import java.util.Iterator;

public class BookListActivity extends VideLibriBaseFragmentActivity{
    boolean port_mode;

    BookListFragment list;
    BookDetails details;
    View detailsPortHolder, listPortHolder;

    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        Log.d("VL", "onCreate: " + port_mode);
        setContentView(R.layout.booklistactivity);
        port_mode = getResources().getBoolean(R.bool.port_mode);
        list = new BookListFragment(this);
        details = new BookDetails(this);
        if (port_mode) {
            detailsPortHolder = findViewById(R.id.bookdetailslayout);
            listPortHolder = findViewById(R.id.booklistlayout);
            showList();
        }
    }

    @Override
    protected void onResume() {
        super.onResume();
        port_mode = getResources().getBoolean(R.bool.port_mode); //should not have changed
        if (!cacheShown)
            displayBookCache();
    }

    @Override
    void setLoading(boolean loading) {
        super.setLoading(loading || (detailsVisible() && details.loading) || list.loading);
    }

    public ArrayList<Bridge.Book> bookCache = new ArrayList<Bridge.Book>();
    public EnumSet<BookOverviewAdapter.DisplayEnum> options = java.util.EnumSet.of(BookOverviewAdapter.DisplayEnum.Grouped, BookOverviewAdapter.DisplayEnum.ShowRenewCount);
    public String sortingKey, groupingKey;

    public ArrayList<Bridge.Book> selectedBooks = null;

    public Button bookActionButton = null; //set from detail fragment

    private boolean cacheShown = false;

    void setOption(BookOverviewAdapter.DisplayEnum option, boolean on){
        if (on) options.add(option);
        else options.remove(option);
    }

    void displayBookCache(int partialSize){
        //Log.i("VL","Book count: "+partialSize);
        setOption(BookOverviewAdapter.DisplayEnum.Grouped, !"".equals(groupingKey));
        BookOverviewAdapter sa = new BookOverviewAdapter(this, bookCache, partialSize, options);
        ListView bookListView = (ListView) findViewById(R.id.booklistview);
        cacheShown = true;
        bookListView.setOnItemClickListener(new AdapterView.OnItemClickListener() {
            @Override
            public void onItemClick(AdapterView<?> adapterView, View view, int i, long l) {
                if (i >= bookCache.size() || bookCache.get(i) == null) return;
                if (groupingKey != "") {
                    Bridge.Book book = bookCache.get(i);
                    if (book.more == VideLibri.crazyHeaderHack) return; //grouping header
                }
                viewDetails(i);
            }
        });
        bookListView.setAdapter(sa);
    }
    void displayBookCache(){
        displayBookCache(bookCache.size());
    }

    void updateDisplayBookCache(){
        ListView bookListView = (ListView) findViewById(R.id.booklistview);
        BookOverviewAdapter sa = (BookOverviewAdapter) bookListView.getAdapter();
        sa.setBooks(bookCache);
    }

    public void onPlaceHolderShown(int position){}

    public void viewDetails(int bookpos){
        if (port_mode) {
            detailsPortHolder.setVisibility(View.VISIBLE);
            listPortHolder.setVisibility(View.INVISIBLE);
        }
        details.setBook(bookCache.get(bookpos));
    }

    //shows the list. returns if the list was already visible
    public boolean showList(){
        if (!port_mode) return true;
        if (detailsVisible()) {
            listPortHolder.setVisibility(View.VISIBLE);
            detailsPortHolder.setVisibility(View.INVISIBLE);
            return false;
        } else return true;
    }
    public boolean detailsVisible(){
        if (!port_mode) return true;
        return detailsPortHolder.getVisibility() == View.VISIBLE;
    }

    @Override
    public void onBackPressed() {
        if (showList())
            super.onBackPressed();
    }

    public void onBookActionButtonClicked(Bridge.Book book){} //called from detail fragment

}
