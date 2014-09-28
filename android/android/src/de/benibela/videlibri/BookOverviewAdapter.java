package de.benibela.videlibri;

import android.graphics.Color;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ArrayAdapter;
import android.widget.TextView;
import de.benibela.videlibri.BookListActivity;
import de.benibela.videlibri.Bridge;
import de.benibela.videlibri.R;
import de.benibela.videlibri.VideLibri;

import java.util.ArrayList;

class BookOverviewAdapter extends ArrayAdapter<Bridge.Book> {
    private final BookListActivity context;
    private final int defaultColor;
    private final int defaultBackgroundColor;
    private ArrayList<Bridge.Book> books;
    private Bridge.Book placeHolder;
    private int completeCount;
    private final boolean noDetailsInOverview;
    BookOverviewAdapter(BookListActivity context, ArrayList<Bridge.Book> books, int completeCount, boolean noDetailsInOverview){
        super(context, R.layout.bookoverview, books);
        this.context = context;
        this.books = books;
        this.completeCount = completeCount;
        if (this.completeCount == 0) this.completeCount = books.size();
        this.noDetailsInOverview = noDetailsInOverview;

        defaultColor = context.getResources().getColor(android.R.color.primary_text_dark);
        defaultBackgroundColor = context.getResources().getColor(android.R.color.background_dark);

        placeHolder = new Bridge.Book();
        placeHolder.author = context.getString(R.string.booklist_loading);

    }

    static class ViewHolder {
        public TextView caption, date, more;
        public View layout;
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
            viewHolder.layout = view;
            view.setTag(viewHolder);
        }
        ViewHolder holder = (ViewHolder) view.getTag();
        Bridge.Book book = getItem(position);
        holder.caption.setText(shortened(book.title));
        if (noDetailsInOverview) holder.more.setVisibility(View.GONE);
        else if (book == placeHolder) {
            context.onPlaceHolderShown(position);
            holder.more.setText(book.author); //not an author
        } else {
            String more = "";
            if (!book.author.trim().equals(""))
                if (!book.author.startsWith("von") && !book.author.startsWith("by")) more = view.getContext().getString(R.string.booklist_from) + " " + shortened(book.author);
                else more = " " + shortened(book.author);
            String year = book.getProperty("year");
            if (year != null && !"".equals(year)) more += " ; " + year;
            String id = book.getProperty("id");
            if (id != null && !"".equals(id)) more += " ; " + id;
            holder.more.setText(more);
        }

        if (context.selectedBooks != null) {
            boolean selected = false;
            for (Bridge.Book b: context.selectedBooks)
                if (b == book) { selected = true; break; }
            if (selected) holder.layout.setBackgroundColor(Color.rgb(0,0,96));
            else holder.layout.setBackgroundColor(defaultBackgroundColor);
        }

        if (book.account != null && !book.history ) { //lend book
            switch (book.getStatus()) {
                case Provided:  holder.date.setText(view.getContext().getString(R.string.booklist_status_provided)); break;
                case Ordered:  holder.date.setText(view.getContext().getString(R.string.booklist_status_ordered)); break;
                default: holder.date.setText(Util.formatDate(book.dueDate)); break;
            }
            int c = book.getStatusColor();
            if (c == -1) c = defaultColor;
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