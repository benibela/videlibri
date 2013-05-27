package de.benibela.videlibri;

import android.app.Activity;
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

import java.text.DateFormat;
import java.util.ArrayList;

public class BookListActivity extends VideLibriBaseActivity {

    static class BookOverviewAdapter extends ArrayAdapter<Bridge.Book> {
        private final BookListActivity context;
        private ArrayList<Bridge.Book> books;
        private Bridge.Book placeHolder;
        private int completeCount;
        BookOverviewAdapter(BookListActivity context, ArrayList<Bridge.Book> books, int completeCount){
            super(context, R.layout.bookoverview, books);
            this.context = context;
            this.books = books;
            this.completeCount = completeCount;
            if (this.completeCount == 0) this.completeCount = books.size();

            placeHolder = new Bridge.Book();
            placeHolder.author = "Ergebnisse werden geladen...";

        }

        static class ViewHolder {
            public TextView caption, date, more;
        }

        private String shortened(String s){
            if (s.length() < 300) return s;
            else return s.substring(0,300) + "...";
        }

        @Override
        public View getView(int position, View convertView, ViewGroup parent) {
            View view = convertView;
            if (view == null){
                LayoutInflater inflater = context.getLayoutInflater();
                view = inflater.inflate(R.layout.bookoverview, null);
                ViewHolder viewHolder = new ViewHolder();
                viewHolder.caption = (TextView) view.findViewById(R.id.bookoverviewCaption);
                viewHolder.date = (TextView) view.findViewById(R.id.bookoverviewDate);
                viewHolder.more = (TextView) view.findViewById(R.id.bookoverviewMore);
                view.setTag(viewHolder);
            }
            ViewHolder holder = (ViewHolder) view.getTag();
            Bridge.Book book = getItem(position);
            holder.caption.setText(shortened(book.title));
            if (book == placeHolder) {
                context.onPlaceHolderShown(position);
                holder.more.setText(book.author); //not an author
            } else {
                String more = "";
                if (!book.author.trim().equals(""))
                    if (!book.author.startsWith("von") && !book.author.startsWith("by")) more = " von " + shortened(book.author);
                    else more = " " + shortened(book.author);
                String year = book.getProperty("year");
                if (year != null && !"".equals(year)) more += " ; " + year;
                String id = book.getProperty("id");
                if (id != null && !"".equals(id)) more += " ; " + id;
                holder.more.setText(more);
            }

            if (book.account != null && !book.history ) { //lend book
                switch (book.getStatus()) {
                    case Provided:  holder.date.setText("abholbereit"); break;
                    case Ordered:  holder.date.setText("vorgemerkt"); break;
                    default: holder.date.setText(book.dueDatePretty); break;
                }
                int c = book.getStatusColor();
                if (c == -1) c = VideLibri.defaultColor;
                //holder.caption.setTextColor(c);
                //holder.more.setTextColor(c);
                holder.date.setTextColor(c);
            } else holder.date.setText("");
            return view;
        }

        @Override
        public int getCount() {
            return completeCount;
        }

        @Override
        public Bridge.Book getItem(int i) {
            if (i < books.size()) return  books.get(i);
            else return placeHolder;
        }

        void setBooks(ArrayList<Bridge.Book> books){
            this.books = books;
            notifyDataSetChanged();
        }

    }


    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.booklist);
        bookListView = (ListView) findViewById(R.id.booklistview);
        dateFormatDefault = android.text.format.DateFormat.getDateFormat(this);
    }

    public ArrayList<Bridge.Book> bookCache = new ArrayList<Bridge.Book>();
    ListView bookListView;
    DateFormat dateFormatDefault;
    static int defaultColor;

    void displayBookCache(int partialSize){
        //Log.i("VL","Book count: "+partialSize);
        BookOverviewAdapter sa = new BookOverviewAdapter(this, bookCache, partialSize);
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
        BookOverviewAdapter sa = (BookOverviewAdapter) bookListView.getAdapter();
        sa.setBooks(bookCache);
    }

    public void onPlaceHolderShown(int position){}

    public void viewDetails(int bookpos){
        Intent intent = new Intent(BookListActivity.this, BookDetails.class);
        BookDetails.bookToView = bookCache.get(bookpos);
        startActivity(intent);
    }
}
