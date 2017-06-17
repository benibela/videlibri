package de.benibela.videlibri;

import android.content.Context;
import android.graphics.Color;
import android.graphics.Typeface;
import android.view.Gravity;
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
import java.util.EnumSet;

class BookOverviewAdapter extends ArrayAdapter<Bridge.Book> {
    private final BookListActivity context;
    private final int defaultColor, defaultColorSecondary;
    private final int defaultBackgroundColor;
    private ArrayList<Bridge.Book> books;
    private Bridge.Book placeHolder;
    private int completeCount;
    private final EnumSet<DisplayEnum> options;
    private final String tr_booklist_from, tr_provided, tr_ordered;

    enum DisplayEnum {
        NoDetails,
        Grouped,
        ShowRenewCount
    };


    BookOverviewAdapter(BookListActivity context, ArrayList<Bridge.Book> books, int completeCount, EnumSet<DisplayEnum> options){
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


        tr_booklist_from = context.getString(R.string.booklist_from);
        tr_provided = context.getString(R.string.book_status_provided);
        tr_ordered = context.getString(R.string.book_status_ordered);
    }


    static class ViewHolder {
        public TextView caption, date, more;
        public View layout;
    }

    private String shortened(String s){
        if (s.length() < 300) return s;
        else return s.substring(0,300) + "...";
    }

    String getBookMoreText(Bridge.Book book){
        String more = "";
        if (!book.author.trim().equals(""))
            if (!book.author.startsWith("von") && !book.author.startsWith("by")) more = tr_booklist_from + " " + shortened(book.author);
            else more = " " + shortened(book.author);
        String year = book.getProperty("year");
        if (year != null && !"".equals(year)) more += " ; " + year;
        String id = book.getProperty("id");
        if (id != null && !"".equals(id)) more += " ; " + id;
        return more;
    }

    String getBookDateText(Bridge.Book book){
        if (book.account != null && !book.history ) { //lend book
            switch (book.getStatus()) {
                case Provided:  return tr_provided;
                case Ordered:  return tr_ordered;
                default:
                    String t = Util.formatDate(book.dueDate);
                    if (options.contains(DisplayEnum.ShowRenewCount)) {
                        String renewCount = book.getProperty("renewCount");
                        if (!"".equals(renewCount) && !"0".equals(renewCount)) t = renewCount + "V " + t;
                    }
                    return t;
            }
        } else {
            String s = "";
            switch (book.getStatus()) {
                case Available: s = "V"; break;
                case Lend: s = "X"; break;
                case Virtual: s = "?"; break;
                case Presentation: s = "X"; break;
                case InterLoan: s = "X"; break;
            }
            return s;
        }
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
        boolean isGroupingHeader = options.contains(DisplayEnum.Grouped) && book.isGroupingHeaderFakeBook();
        holder.caption.setText(shortened(book.title));
        if (options.contains(DisplayEnum.Grouped)) {
            if (isGroupingHeader) {
                holder.caption.setTypeface(holder.caption.getTypeface(), Typeface.BOLD);
                holder.caption.setGravity(Gravity.CENTER_HORIZONTAL);
                holder.caption.setTextColor(book.getStatusColor());
            } else {
                holder.caption.setTypeface(holder.caption.getTypeface(), Typeface.NORMAL);
                holder.caption.setGravity(Gravity.NO_GRAVITY);
                holder.caption.setTextColor(defaultColorSecondary);
            }
        }

        if (options.contains(DisplayEnum.NoDetails)) holder.more.setVisibility(View.GONE);
        else if (book == placeHolder) {
            context.onPlaceHolderShown(position);
            holder.more.setText(book.author); //not an author
        } else {
            if  (options.contains(DisplayEnum.Grouped))
                holder.more.setVisibility(isGroupingHeader ? View.GONE : View.VISIBLE);
            if (!isGroupingHeader) {
                holder.more.setText(getBookMoreText(book));
            }
        }

        if (context.selectedBooks != null) {
            boolean selected = false;
            for (Bridge.Book b: context.selectedBooks)
                if (b == book) { selected = true; break; }
            if (selected) holder.layout.setBackgroundColor(Color.rgb(0,0,96));
            else holder.layout.setBackgroundColor(defaultBackgroundColor);
        }

        holder.date.setText(getBookDateText(book));

        int c = book.getStatusColor();
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

    String exportShare(){
        StringBuilder sb = new StringBuilder();
        for (int i=0;i<books.size();i++) {
            Bridge.Book book = books.get(i);
            boolean isGroupingHeader = options.contains(DisplayEnum.Grouped) && book.isGroupingHeaderFakeBook();
            if (isGroupingHeader)
                sb.append("\n");
            sb.append(book.title);
            sb.append(" ");
            if (!isGroupingHeader && !options.contains(DisplayEnum.NoDetails))
                sb.append(getBookMoreText(book));
            if (!book.history) {
                sb.append(": ");
                sb.append(getBookDateText(book));
            }
            sb.append("\n");
        }
        return sb.toString();
    }

}