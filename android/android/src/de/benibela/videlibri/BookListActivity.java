package de.benibela.videlibri;

import android.app.Activity;
import android.app.FragmentTransaction;
import android.content.Intent;
import android.os.Bundle;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.AdapterView;
import android.widget.ArrayAdapter;
import android.widget.ListView;
import android.widget.TextView;
import com.actionbarsherlock.app.SherlockFragmentActivity;

import java.text.DateFormat;
import java.util.ArrayList;

public class BookListActivity extends VideLibriBaseFragmentActivity{

    boolean port_mode;
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.booklistactivity);
        port_mode = getResources().getBoolean(R.bool.port_mode);
        if (port_mode) {

            android.support.v4.app.FragmentTransaction transaction = getSupportFragmentManager().beginTransaction();
            //transaction.setCustomAnimations(android.R.anim.slide_in_left, android.R.anim.slide_out_right);
            listFragment = new BookListFragment();
            transaction.add(R.id.layout, listFragment);
            //transaction.addToBackStack(null);
            transaction.commit();
            detailsOpened = false;
        }
    }

    @Override
    protected void onResume() {
        super.onResume();
        if (!cacheShown)
            displayBookCache();
    }

    @Override
    void setLoading(boolean loading) {
        super.setLoading(loading || (details() != null && details().loading) || (list() != null && list().loading));
    }

    public ArrayList<Bridge.Book> bookCache = new ArrayList<Bridge.Book>();
    public boolean noDetailsInOverview = false;

    public ArrayList<Bridge.Book> selectedBooks = null;

    private boolean cacheShown = false;
    void displayBookCache(int partialSize){
        //Log.i("VL","Book count: "+partialSize);
        BookOverviewAdapter sa = new BookOverviewAdapter(this, bookCache, partialSize, noDetailsInOverview);
        ListView bookListView = (ListView) findViewById(R.id.booklistview);
        if (bookListView == null && list() != null) bookListView = (ListView) (list().findViewById(R.id.booklistview));
        if (bookListView == null) return; //this might get executed before the fragment/view is loaded
        cacheShown = true;
        bookListView.setOnItemClickListener(new AdapterView.OnItemClickListener() {
            @Override
            public void onItemClick(AdapterView<?> adapterView, View view, int i, long l) {
                if (i < bookCache.size() && bookCache.get(i) != null)
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
            android.support.v4.app.FragmentTransaction transaction = getSupportFragmentManager().beginTransaction();
            //transaction.setCustomAnimations(android.R.anim.slide_in_left, android.R.anim.slide_out_right);
            transaction.hide(listFragment);
            if (lastDetails == null){
                lastDetails = new BookDetails();
                transaction.add(R.id.layout, lastDetails);
                BookDetails.bookToView = bookCache.get(bookpos);
            } else {
                transaction.show(lastDetails);
                lastDetails.setBook(bookCache.get(bookpos));
            }
            //transaction.addToBackStack(null);    back stack did not work (why ?) add/detach/remove also did not work
            transaction.commit();
            detailsOpened = true;
            return;
        }
        BookDetails dets = details();
        if (dets == null) return;
        dets.setBook(bookCache.get(bookpos));
    }

    boolean detailsOpened = false;
    @Override
    public void onBackPressed() {
        if (detailsOpened) {
            android.support.v4.app.FragmentTransaction transaction = getSupportFragmentManager().beginTransaction();
            //transaction.setCustomAnimations(android.R.anim.slide_in_left, android.R.anim.slide_out_right);
            transaction.hide(lastDetails);
            transaction.show(listFragment);
            transaction.commit();
            detailsOpened = false;
        } else super.onBackPressed();
    }

    public void onBookActionButtonClicked(Bridge.Book book){} //called from detail fragment

    BookListFragment listFragment;
    BookDetails lastDetails;
    public BookDetails details(){
        if (lastDetails != null) return lastDetails;
        BookDetails fragment = (BookDetails) getSupportFragmentManager().findFragmentById(R.id.details); //does not work in portrait mode (id is not set??)
        return fragment;
    }
    public BookListFragment list(){
        if (listFragment != null) return listFragment;
        BookListFragment fragment = (BookListFragment) getSupportFragmentManager().findFragmentById(R.id.list); //does not work in portrait mode (id is not set??)
        return fragment;
    }
}
