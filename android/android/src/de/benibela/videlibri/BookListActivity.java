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
    static final String FRAGMENT_TAG_LIST = "list";
    static final String FRAGMENT_TAG_DETAILS = "details";

    boolean port_mode;
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        Log.d("VL", "onCreate: " + port_mode);
        setContentView(R.layout.booklistactivity);
        port_mode = getResources().getBoolean(R.bool.port_mode);
        FragmentTransaction transaction = getSupportFragmentManager().beginTransaction();
        if (port_mode) {
            transactionAddList(transaction);
            //transaction.addToBackStack(null);
        } else {
            transactionAddList(transaction);
            transactionAddDetails(transaction);
        }
        transaction.commit();

        Log.d("VL", "  end onCreate: " + port_mode);
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
        BookDetails d = details();
        BookListFragment l = list();
        super.setLoading(loading || (d != null && d.isVisible() && d.loading) || (l != null && l.loading));
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
        if (bookListView == null && list() != null) bookListView = (ListView) (list().findViewById(R.id.booklistview));
        if (bookListView == null) return; //this might get executed before the fragment/view is loaded
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

    public Bridge.Book currentBook;
    public void viewDetails(int bookpos){
        currentBook = bookCache.get(bookpos);
        BookDetails d = details();
        if (port_mode) {
            FragmentTransaction transaction = getSupportFragmentManager().beginTransaction();
            //transaction.setCustomAnimations(android.R.anim.slide_in_left, android.R.anim.slide_out_right);
            transactionAddDetails(transaction);
            transaction.commit();
            return;
        }
        BookDetails dets = details();
        if (dets == null) return;
        dets.setBook(currentBook);
    }

    //shows the list. returns if the list was already visible
    public boolean showList(){
        BookDetails d = details();
        if (port_mode && d != null && d.isVisible()) {
            FragmentTransaction transaction = getSupportFragmentManager().beginTransaction();
            //transaction.setCustomAnimations(android.R.anim.slide_in_left, android.R.anim.slide_out_right);
            transactionAddList(transaction);
            transaction.commit();
            return false;
        } else return true;
    }

    @Override
    public void onBackPressed() {
        if (showList())
            super.onBackPressed();
    }

    public void onBookActionButtonClicked(Bridge.Book book){} //called from detail fragment

    public BookDetails details(){
        BookDetails fragment = (BookDetails) getSupportFragmentManager().findFragmentByTag(FRAGMENT_TAG_DETAILS);
        return fragment;
    }
    public BookListFragment list(){
        BookListFragment fragment = (BookListFragment) getSupportFragmentManager().findFragmentByTag(FRAGMENT_TAG_LIST);
        return fragment;
    }
    private void transactionAddList(FragmentTransaction t){
        BookListFragment l = list();
        if (l == null) l = new BookListFragment();
        //if (l.isAdded() && !)
        t.replace(port_mode ? R.id.layout : R.id.list, l, FRAGMENT_TAG_LIST);
    }
    private void transactionAddDetails(FragmentTransaction t){
        BookDetails d = details();
        if (d == null) d = new BookDetails();
        t.replace(port_mode ? R.id.layout : R.id.details, d, FRAGMENT_TAG_DETAILS);
    }

}
