package de.benibela.videlibri;

import android.graphics.Color;
import android.graphics.Typeface;
import android.support.annotation.NonNull;
import android.view.Gravity;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ArrayAdapter;
import android.widget.TextView;
import java.util.ArrayList;
import java.util.EnumSet;

import de.benibela.videlibri.jni.Bridge;

class BookOverviewAdapter extends ArrayAdapter<Bridge.Book> {
    private final BookListActivity context;
    private final int defaultColor, defaultColorSecondary;
    private final int defaultBackgroundColor;
    private ArrayList<Bridge.Book> books;
    private Bridge.Book placeHolder;
    private int completeCount;
    private final BookListDisplayOptions options;


    enum DisplayEnum {
        NoDetails,
        Grouped,
        ShowRenewCount
    }

    static class ViewHolder {
        public TextView caption, date, more;
        public View layout;
    }


    BookOverviewAdapter(BookListActivity context, ArrayList<Bridge.Book> books, int completeCount, BookListDisplayOptions options){
        super(context, R.layout.bookoverview, books);
        this.context = context;
        this.books = books;
        this.completeCount = completeCount;
        if (this.completeCount == 0) this.completeCount = books.size();
        this.options = options;

        defaultColor = context.getResources().getColor(android.R.color.primary_text_dark);
        defaultColorSecondary = context.getResources().getColor(android.R.color.secondary_text_dark);
        defaultBackgroundColor = context.getResources().getColor(android.R.color.background_dark);

        placeHolder = new Bridge.Book();
        placeHolder.author = context.getString(R.string.booklist_loading);


        BookFormatter.tr_booklist_from = context.getString(R.string.booklist_from);
        BookFormatter.tr_provided = context.getString(R.string.book_status_provided);
        BookFormatter.tr_ordered = context.getString(R.string.book_status_ordered);
    }


    @NonNull
    @Override
    public View getView(int position, View convertView, @NonNull ViewGroup parent) {
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
        if (book == null) return view;
        boolean isGroupingHeader = options.isGrouped() && BookFormatter.isGroupingHeaderFakeBook(book);
        holder.caption.setText(BookFormatter.shortened(book.title));
        if (options.isGrouped()) {
            if (isGroupingHeader) {
                holder.caption.setTypeface(holder.caption.getTypeface(), Typeface.BOLD);
                holder.caption.setGravity(Gravity.CENTER_HORIZONTAL);
                holder.caption.setTextColor(BookFormatter.getStatusColor(book));
            } else {
                holder.caption.setTypeface(holder.caption.getTypeface(), Typeface.NORMAL);
                holder.caption.setGravity(Gravity.NO_GRAVITY);
                holder.caption.setTextColor(defaultColorSecondary);
            }
        }

        if (options.getNoDetailsInOverview()) holder.more.setVisibility(View.GONE);
        else if (book == placeHolder) {
            context.onPlaceHolderShown(position);
            holder.more.setText(book.author); //not an author
        } else {
            if  (options.isGrouped())
                holder.more.setVisibility(isGroupingHeader ? View.GONE : View.VISIBLE);
            if (!isGroupingHeader) {
                holder.more.setText(BookFormatter.getBookMoreText(book));
            }
        }

        ArrayList<Bridge.Book> sb = context.getSelectedBooks();
        if (sb != null) {
            boolean selected = false;
            for (Bridge.Book b: sb)
                if (b == book) { selected = true; break; }
            if (selected) holder.layout.setBackgroundColor(Color.rgb(0,0,96));
            else holder.layout.setBackgroundColor(defaultBackgroundColor);
        }

        holder.date.setText(BookFormatter.getBookDateText(book, options));

        int c = BookFormatter.getStatusColor(book);
        if (c == -1) c = defaultColor;
        //holder.caption.setTextColor(c);
        //holder.more.setTextColor(c);
        holder.date.setTextColor(c);
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

    /*ArrayList<Bridge.Book> getBooks(){
        return books;
    } */

    String exportShare(boolean html){
        String newline = html ? "<br>\n" : "\n";
        StringBuilder sb = new StringBuilder();
        for (int i=0;i<books.size();i++) {
            Bridge.Book book = books.get(i);
            boolean isGroupingHeader = options.isGrouped() && BookFormatter.isGroupingHeaderFakeBook(book);
            if (isGroupingHeader && i != 0) {
                sb.append(newline);
                sb.append(newline);
            }
            sb.append(book.title);
            sb.append(" ");
            if (!isGroupingHeader && !options.getNoDetailsInOverview())
                sb.append(BookFormatter.getBookMoreText(book));
            if (!book.history) {
                sb.append(": ");
                sb.append(BookFormatter.getBookDateText(book, options));
            }
            sb.append(newline);
            sb.append(newline);
        }
        return sb.toString();
    }

}