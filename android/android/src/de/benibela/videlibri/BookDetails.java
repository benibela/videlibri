package de.benibela.videlibri;


import android.app.Activity;
import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.graphics.Typeface;
import android.graphics.drawable.BitmapDrawable;
import android.graphics.drawable.Drawable;
import android.os.AsyncTask;
import android.os.Bundle;
import android.text.util.Linkify;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.*;

import java.io.InputStream;
import java.util.*;

public class BookDetails extends VideLibriBaseFragment {
    Bridge.Book book;

    static String trStatus = "", trDueDate = "";

    static class Details{
        String name, data;
        Details(String name, String data){
            this.name = name;
            this.data = data;
            if (name == null) this.name = "??";
            if (this.name.endsWith("!")) this.name = this.name.substring(0,this.name.length()-1);
            if (data == null) this.data = "";
        }
    }

    static class BookDetailsAdapter extends BaseAdapter{
        private final Activity context;
        private final ArrayList<Details> details;
        private final Bridge.Book book;


        final int defaultColor;
        final float scale;
        int toPx(float sp) { return (int) (sp * scale + 0.5f); }

        BookDetailsAdapter(Activity context, ArrayList<Details> details, Bridge.Book book){
            super();
            this.context = context;
            this.details = details;
            this.book = book;
            if (book.image != null) image = new BitmapDrawable(book.image);

            this.defaultColor = context.getResources().getColor(android.R.color.primary_text_dark);

            this.scale = context.getResources().getDisplayMetrics().scaledDensity;
        }

        static class ViewHolder {
            public TextView text;
        }

        @Override
        public int getCount() {
            return 2 * details.size() + 1;
        }

        @Override
        public Object getItem(int i) {
            return details.get((i-1)/2);
        }

        @Override
        public long getItemId(int i) {
            return i;
        }

        @Override
        public View getView(int position, View convertView, ViewGroup parent) {
            View view = convertView;
            if (view == null){
                LayoutInflater inflater = context.getLayoutInflater();
                view = inflater.inflate(R.layout.simpletextview, null);
                ViewHolder viewHolder = new ViewHolder();
                viewHolder.text = (TextView) view.findViewById(R.id.simpletextview);
                viewHolder.text.setAutoLinkMask(Linkify.WEB_URLS);
                view.setTag(viewHolder);
            }
            ViewHolder holder = (ViewHolder) view.getTag();
            if (position > 0) {
                Details d = details.get((position-1)/2);
                int c =  defaultColor;
                if (position % 2 == 1) {
                    holder.text.setTypeface(Typeface.DEFAULT_BOLD);
                    holder.text.setText(d.name);
                    holder.text.setPadding(toPx(10),toPx(1),toPx(10),toPx(1));
                } else {
                    holder.text.setTypeface(Typeface.DEFAULT);
                    holder.text.setText(d.data);
                    holder.text.setPadding(toPx(30),toPx(1),toPx(10),toPx(2));
                    if (trStatus.equals(d.name) || trDueDate.equals(d.name)){
                        c = book.getStatusColor();
                        if (c == -1) c = defaultColor;
                    }


                }
                holder.text.setCompoundDrawables(null, null, null, null);
                holder.text.setTextColor(c);
            } else {
                holder.text.setText("");
                holder.text.setCompoundDrawablesWithIntrinsicBounds(null, null, null, image);
            }


            return view;
        }

        Drawable image;
        void updateImage(){
            if (book == null) return;
            image = new BitmapDrawable(book.image);
            notifyDataSetChanged();
        }
    }

    ArrayList<Details> details = new ArrayList<Details>();
    void addIfExists(String displayName, String propertyName){
        String value = book.getProperty(propertyName);
        if (value == null || "".equals(value)) return;
        details.add(new Details(displayName, value));
    }

    static Bridge.Book bookToView = null; //passed as parameter to newly created detail display (do not serialize, as it crashes with books with images and is slow in any case)


    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container,
                             Bundle savedInstanceState) {
        View view = inflater.inflate(R.layout.bookdetails, container, false);


        return view;
    }

    @Override
    public void onResume() {
        super.onResume();    //To change body of overridden methods use File | Settings | File Templates.

        setBook( bookToView );
        bookToView = null;
    }

    void setBook(Bridge.Book newBook){
        if (newBook != null) book = newBook;
        if (book == null) book = new Bridge.Book();

        /*Log.i("VL",  ""+isInLayout());
        Log.i("VL", ""+getSherlockActivity());
        Log.i("VL", ""+getView());       */

        if (getSherlockActivity() == null || getView() == null || newBook == null) return;

        boolean searchedBook = book.account == null;

        ListView lv = (ListView) findViewById(R.id.bookdetailsview);

        if (lv == null) return;

        details.clear();
        String titleData = book.title;
        if (book.author != null && !book.author.equals("")) {
            if (!book.author.startsWith("von") && !book.author.startsWith("by")) titleData += "\n\n" + tr(R.string.book_from) + " " + book.author;
            else titleData += "\n\n "+book.author;
        }
        String year = book.getProperty("year");
        if (year != null && !year.equals("")) titleData += "\n " + year;
        String id = book.getProperty("id");
        if (id != null && !id.equals("")) titleData += "\n " + id;

        if (titleData != null && !"".equals(titleData))
            details.add(new Details(tr(R.string.book_titleauthor), titleData));

        trStatus = tr(R.string.book_status);
        trDueDate = tr(R.string.book_duedate);

        if ((!searchedBook && !book.history) || book.dueDate != null)
            details.add(new Details(trDueDate, Util.formatDate(book.dueDate)));

        String status = book.getProperty("status");
        if (status == null) status = "";
        if ("".equals(status))
            switch (book.getStatus()){
                case Problematic: status = tr(R.string.book_status_problematic); break;
                case Ordered: status = tr(R.string.book_status_ordered); break;
                case Provided: status = tr(R.string.book_status_provided); break;
            }
        if (!"".equals(status)) details.add(new Details(trStatus, status));

        if (book.issueDate != null)
          details.add(new Details(tr(R.string.book_lenddate), Util.formatDate(book.issueDate)));
        addIfExists(tr(R.string.book_lendat) , "libraryBranch");
        if (book.account != null) details.add(new Details(tr(R.string.book_account), book.account.prettyName));

        //addIfExists("ID", "id");
        addIfExists(tr(R.string.book_category), "category");
        //addIfExists("Jahr", "year");
        addIfExists(tr(R.string.book_publisher), "publisher");

        final List<String> above = Arrays.asList("status", "id", "category", "year", "statusId", "libraryBranch", "publisher", "orderable", "cancelable", "renewCount");

        for (int i=0;i<book.more.size();i++)
            if ( book.more.get(i).second != null && !"".equals(book.more.get(i).second) && (
                    (!searchedBook && !above.contains(book.more.get(i).first))
                    || (searchedBook && book.more.get(i).first.endsWith("!"))
            ))
                details.add(new Details(book.more.get(i).first, book.more.get(i).second));

        lv.setAdapter(new BookDetailsAdapter(getSherlockActivity(), details, book));

        boolean needToLoadImage = book.more != null && (book.hasProperty("image-url") || book.hasProperty("isbn")) && book.image == null;
        if (needToLoadImage)
            new DownloadImageTask(this, book).execute(book.getProperty("image-url"));

        setLoading(searchedBook && (!book.hasProperty("__details") || needToLoadImage ));

        String action = null;
        if (searchedBook) {
            if (book.isOrderable()) {
                action = book.getProperty("orderTitle");
                if (action == null || "".equals(action)) action = tr(R.string.book_order);
            }
        } else if (!book.history && !(getActivity() instanceof RenewList))
            switch (book.getStatus()) {
                case Unknown: action = tr(R.string.book_renew); break;
                case Normal: action = tr(R.string.book_renew); break;
                //case Problematic: break;
                case Ordered:  case Provided:
                    if (book.isCancelable()) action = tr(R.string.book_cancel);
                    else action = null;
                break;
            }
        Button actionButton = findButtonById(R.id.button);
        if (action != null) {
            actionButton.setText(action);
            actionButton.setVisibility(View.VISIBLE);
            actionButton.setOnClickListener(new View.OnClickListener() {
                public void onClick(View view){
                    if (getSherlockActivity() instanceof BookListActivity)
                        ((BookListActivity) getSherlockActivity()).onBookActionButtonClicked(book);
                }
            });
            if (getSherlockActivity() instanceof BookListActivity)
                ((BookListActivity) getSherlockActivity()).bookActionButton = actionButton;
        } else actionButton.setVisibility(View.GONE);

    }

    void updateImage(){
        setLoading(false);
        ListView lv = (ListView) findViewById(R.id.bookdetailsview);
        if (lv == null) return;
        BookDetailsAdapter adapter = (BookDetailsAdapter) lv.getAdapter();
        if (adapter == null) return;
        adapter.updateImage();
    }

    //from http://stackoverflow.com/questions/5776851/load-image-from-url
    class DownloadImageTask extends AsyncTask<String, Void, Bitmap> {
        Bridge.Book book;
        BookDetails activity;

        public DownloadImageTask(BookDetails activity, Bridge.Book book) {
            this.book = book;
            this.activity = activity;
        }

        protected Bitmap doInBackground(String... imageUrlProp) {
            String[] urls = imageUrlProp[0].split("[\r\n]]");
            Bitmap cover = null;
            for (int i=0;i<urls.length + 2 && cover == null;i++) {
                try {
                    String url;
                    if (i < urls.length) url = urls[i].trim();
                    else if (i == urls.length)
                        url = "http://covers.openlibrary.org/b/isbn/"+book.getNormalizedISBN(false,false)+"-M.jpg?default=false";
                    else
                        url = "http://vlb.de/GetBlob.aspx?strIsbn="+book.getNormalizedISBN(true,true)+"&size=M";

                    if ("".equals(url)) continue;

                    InputStream in = new java.net.URL(url).openStream();
                    cover = BitmapFactory.decodeStream(in);
                } catch (Throwable e) { //need to catch OutOfMemoryError and broken images exceptions
                    //Log.e("Error", e.getMessage());
                    e.printStackTrace();
                }
            }
            return cover;
        }

        protected void onPostExecute(Bitmap result) {
            if (result == null) return;
            book.image = result;
            if (activity != null && book == activity.book) activity.updateImage();
        }
    }
}
