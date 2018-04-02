package de.benibela.videlibri;


import android.app.Activity;
import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.graphics.Color;
import android.graphics.Typeface;
import android.graphics.drawable.BitmapDrawable;
import android.graphics.drawable.Drawable;
import android.os.AsyncTask;
import android.os.Bundle;
import android.support.annotation.Nullable;
import android.text.util.Linkify;
import android.util.DisplayMetrics;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.*;

import java.io.InputStream;
import java.util.*;

public class BookDetails extends VideLibriFakeFragment {
    Bridge.Book book;

    static String trStatus = "", trDueDate = "";

    static boolean isAdditionalDisplayProperty(String s){
        return s.endsWith("!");
    }

    static class Details{
        String name, data;
        Details(String name, String data){
            this.name = name;
            this.data = data;
            if (name == null) this.name = "??";
            if (isAdditionalDisplayProperty(this.name)) this.name = this.name.substring(0,this.name.length()-1);
            if (data == null) this.data = "";
        }

        @Override
        public String toString() {
            return name + ": " + data;
        }
    }
    static class DetailsHolding extends Details{
        final boolean orderable;
        String orderLabel;
        int holdingId;
        DetailsHolding(String name, String data, Bridge.Book holding, int holdingId, String orderLabel){
            super(name, data);
            orderable = holding.isOrderableHolding();
            this.orderLabel = orderLabel;
            this.holdingId = holdingId;
        }
    }

    static BitmapFactory.Options bitmapOpts = new BitmapFactory.Options();
    static private DisplayMetrics displayMetrics;

    static class BookDetailsAdapter extends BaseAdapter{
        private final Activity context;
        private final ArrayList<Details> details;
        private final int holdingStartPosition;
        private final Bridge.Book book;


        final int defaultColor;
        final float scale;
        int toPx(float sp) { return (int) (sp * scale + 0.5f); }

        boolean holdingOrderClickable = true;


        BookDetailsAdapter(Activity context, ArrayList<Details> details, Bridge.Book book){
            super();
            this.context = context;
            this.details = details;
            this.book = book;
            if (book.image != null) image = new BitmapDrawable(book.image);

            this.defaultColor = context.getResources().getColor(android.R.color.primary_text_dark);
            displayMetrics = context.getResources().getDisplayMetrics();
            this.scale = displayMetrics.scaledDensity;

            int hs = details.size();
            while (hs > 0 && details.get(hs-1) instanceof DetailsHolding) hs--;
            holdingStartPosition = hs * 2 + 1;
        }

        static class ViewHolder {
            public TextView text;
        }
        static class ViewHolderHolding extends ViewHolder {
            public Button button;
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

        static final private int VIEW_HEADER = 0;
        static final private int VIEW_VALUE = 1;
        static final private int VIEW_HOLDING_VALUE = 2;


        @Override
        public int getViewTypeCount() {
            return 3;
        }


        @Override
        public int getItemViewType(int position) {
            if (position == 0) return VIEW_VALUE;
            if ((position & 1) == 1) return VIEW_HEADER;
            if (position < holdingStartPosition ) return VIEW_VALUE;
            return VIEW_HOLDING_VALUE;
        }

        @Override
        public View getView(int position, View convertView, ViewGroup parent) {
            View view = convertView;
            int type = getItemViewType(position);
            if (view == null){
                ViewHolder viewHolder = null;
                LayoutInflater inflater = context.getLayoutInflater();
                switch (type) {
                    case VIEW_HOLDING_VALUE:
                        view = inflater.inflate(R.layout.holdingrow, null);
                        viewHolder = new ViewHolderHolding();
                        ((ViewHolderHolding)viewHolder).button = (Button) view.findViewById(R.id.holdingbutton);
                        break;
                    case VIEW_HEADER:
                    case VIEW_VALUE:
                    default:
                        view = inflater.inflate(R.layout.simpletextview, null);
                        viewHolder = new ViewHolder();
                        break;
                }
                viewHolder.text = (TextView) view.findViewById(R.id.simpletextview);
                viewHolder.text.setAutoLinkMask(Linkify.WEB_URLS);
                view.setTag(viewHolder);
                switch (type) {
                    case VIEW_HEADER:
                        viewHolder.text.setTypeface(Typeface.DEFAULT_BOLD);
                        viewHolder.text.setPadding(toPx(10),toPx(1),toPx(10),toPx(1));
                        break;
                    case VIEW_VALUE:
                        viewHolder.text.setTypeface(Typeface.DEFAULT);
                        viewHolder.text.setPadding(toPx(30),toPx(1),toPx(10),toPx(2));
                        break;
                    case VIEW_HOLDING_VALUE:
                        viewHolder.text.setTypeface(Typeface.DEFAULT);
                        viewHolder.text.setPadding(toPx(30),toPx(1),toPx(10),toPx(2));
                }
            }

            ViewHolder holder = (ViewHolder) view.getTag();
            if (position > 0) {
                final Details d = details.get((position-1)/2);
                switch (type){
                    case VIEW_HEADER:
                        holder.text.setText(d.name);
                        break;
                    case VIEW_VALUE:
                        holder.text.setText(d.data);
                        int c =  defaultColor;
                        if (trStatus.equals(d.name) || trDueDate.equals(d.name)){
                            c = BookFormatter.getStatusColor(book);
                            if (c == -1) c = defaultColor;
                        }
                        holder.text.setTextColor(c);
                        holder.text.setCompoundDrawables(null, null, null, null);
                        break;
                    case VIEW_HOLDING_VALUE:
                        holder.text.setText(d.data);
                        if (d instanceof DetailsHolding
                                && ((DetailsHolding)d).orderable
                                && book.account == null
                                && context instanceof SearchResult
                                ) {
                            ((ViewHolderHolding)holder).button.setText(((DetailsHolding)d).orderLabel);
                            ((ViewHolderHolding)holder).button.setVisibility(View.VISIBLE);
                            ((ViewHolderHolding)holder).button.setClickable(holdingOrderClickable);
                            ((ViewHolderHolding)holder).button.setOnClickListener(new View.OnClickListener() {
                                @Override
                                public void onClick(View view) {
                                    ((SearchResult)context).orderBookHolding(book, ((DetailsHolding)d).holdingId);
                                }
                            });
                        } else
                            ((ViewHolderHolding)holder).button.setVisibility(View.INVISIBLE);

                }
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


    BookDetails (BookListActivity activity) {
        super(activity);
        View lv = findViewById(R.id.bookdetailsview);
        if (lv != null) activity.registerForContextMenu(lv);
    }

    void setBook(Bridge.Book newBook){
        if (newBook != null) book = newBook;
        if (book == null) book = new Bridge.Book();

        /*Log.i("VL",  ""+isInLayout());
        Log.i("VL", ""+getSherlockActivity());
        Log.i("VL", ""+getView());       */

        if (newBook == null) return;

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
            details.add(new Details(book.hasOrderedStatus() ? tr(R.string.book_duedate_order) : trDueDate, Util.formatDate(book.dueDate)));

        String status = BookFormatter.getStatusText(book);
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
            if ( book.more.get(i).second != null && !"".equals(book.more.get(i).second)) {
                if  (!searchedBook && !above.contains(book.more.get(i).first)
                    || (searchedBook && isAdditionalDisplayProperty(book.more.get(i).first)))
                    details.add(new Details(book.more.get(i).first, book.more.get(i).second));
                else if ("isbn".equals(book.more.get(i).first))
                    details.add(new Details("ISBN", book.more.get(i).second));
            }


        addHoldings(book.holdings);

        lv.setAdapter(new BookDetailsAdapter(activity, details, book));

        boolean needToLoadImage = book.more != null && (book.hasProperty("image-url") || book.hasProperty("isbn")) && book.image == null;
        if (needToLoadImage) {
            new DownloadImageTask(this, book).execute(book.getProperty("image-url"));
            beginLoading(VideLibriBaseActivity.LOADING_COVER_IMAGE);
        }

        String action = null;
        if (searchedBook) {
            if (book.isOrderable()) {
                action = book.getProperty("orderTitle");
                if (action == null || "".equals(action)) action = tr(R.string.book_order);
            }
        } else if (!book.history && !(activity instanceof RenewList))
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
                    if (activity instanceof BookListActivity)
                        ((BookListActivity) activity).onBookActionButtonClicked(book);
                }
            });
            if (activity instanceof BookListActivity)
                ((BookListActivity) activity).bookActionButton = actionButton;
        } else actionButton.setVisibility(View.GONE);

    }

    private class HoldingDetailMaker{
        StringBuilder builder = new StringBuilder();
        Bridge.Book holding;
        //  String indent;
        void addPair(String name, String value){
            if (Util.isEmptyString(value)) return;
            if (builder.length()>0) builder.append("\n");
            //builder.append(indent);
            builder.append(name);
            builder.append(": ");
            builder.append(value);
        }
        void addProperty(int translation, String value){
            String v = holding.getProperty(value);
            if (Util.isEmptyString(v)) return;
            addPair(tr(translation), v);
        }
    }
    private void addHoldings(Bridge.Book holdings[]){
        if (holdings==null || holdings.length==0) return;
        HoldingDetailMaker builder = new HoldingDetailMaker();
        //builder.indent = "      ";
        String defaultOrderTitle = book.getProperty("orderTitle");
        if (Util.isEmptyString(defaultOrderTitle)) defaultOrderTitle = tr(R.string.book_order);
        for (int i=0;i<holdings.length;i++){
            builder.builder.setLength(0);
            builder.holding = holdings[i];
            builder.addPair(tr(R.string.book_title), builder.holding.title);
            builder.addPair(tr(R.string.book_author), builder.holding.author);

            final String specialProperties[] = {"id", "barcode", "category", "publisher", "libraryBranch", "libraryLocation", "year", "status", "pendingOrders"};
            final int specialPropertiesLabel[] = {R.string.book_id, R.string.book_barcode, R.string.book_category, R.string.book_publisher, R.string.book_libraryBranch, R.string.book_libraryLocation, R.string.book_year, R.string.book_status, R.string.book_pendingOrders};

            for (int j=0;j<specialPropertiesLabel.length;j++)
                builder.addProperty(specialPropertiesLabel[j], specialProperties[j]);
            for (int j=0;j<builder.holding.more.size();j++) {
                Bridge.Book.Pair pair = builder.holding.more.get(j);
                //Log.i("VIDELIBRIPAIR", pair.first+" : "+pair.second);
                if (isAdditionalDisplayProperty(pair.first))
                    builder.addPair(pair.first.substring(0,pair.first.length()-1), pair.second );
            }
            if (builder.holding.dueDate != null)
                builder.addPair(trDueDate, Util.formatDate(builder.holding.dueDate));



            String orderTitle = builder.holding.getProperty("orderTitle", defaultOrderTitle);

            details.add(new DetailsHolding("Exemplar " + (i + 1), builder.builder.toString(), builder.holding, i, orderTitle));
        }
    }

    protected BookDetailsAdapter getAdapter(View v) {
        if (v == null) return null;
        ListView lv = (ListView) v;
        return (BookDetailsAdapter) lv.getAdapter();
    };
    protected BookDetailsAdapter getAdapter() {
        return getAdapter(findViewById(R.id.bookdetailsview));
    }

    void updateImage(){
        BookDetailsAdapter adapter = getAdapter();
        if (adapter == null) return;
        adapter.updateImage();
    }


    public void setOrderButtonsClickable() {
        ListView lv = (ListView) findViewById(R.id.bookdetailsview);
        if (lv == null) return;
        final boolean clickable = !activity.isLoading(VideLibriBaseActivity.LOADING_SEARCH_ORDER_HOLDING);
        getAdapter(lv).holdingOrderClickable = clickable;
        Util.iterateChildViews(lv, new Util.ViewIterator(){
            @Override
            public void visit(View v) {
                if (v instanceof Button) v.setClickable(clickable);
            }
        });
    }


    String exportShare(boolean html){
        StringBuilder sb = new StringBuilder();
        for (int i=0;i<details.size();i++) {
            if (html) sb.append("<b>");
            sb.append(details.get(i).name);
            if (html) sb.append("</b>");
            if (details.get(i).name != null && !details.get(i).name.endsWith(":") ) sb.append(":");
            sb.append("\n");
            sb.append(details.get(i).data);
            if (html) sb.append("<br>");
            sb.append("\n\n");
        }
        return sb.toString();
    }


    //from http://stackoverflow.com/questions/5776851/load-image-from-url
    static class DownloadImageTask extends AsyncTask<String, Void, Bitmap> {
        Bridge.Book book;
        BookDetails fragment;


        public DownloadImageTask(BookDetails fragment, Bridge.Book book) {
            this.book = book;
            this.fragment = fragment;
        }

        protected Bitmap doInBackground(String... imageUrlProp) {
            String[] urls = imageUrlProp[0].split("[\r\n]");
            Bitmap cover = null;
            BitmapFactory.Options bitmapOpts = new BitmapFactory.Options();
            bitmapOpts.inDensity = DisplayMetrics.DENSITY_DEFAULT;
            bitmapOpts.inTargetDensity = displayMetrics.densityDpi;
            bitmapOpts.inScreenDensity = displayMetrics.densityDpi;
            bitmapOpts.inScaled = true;
            String normalizedISBN10 = "";

            for (int i=0;i<urls.length + 1 + 1 /*+1 disabled*/;i++) {
                try {
                    String url;
                    if (i < urls.length) {
                        url = urls[i].trim();
                        if ("".equals(url)) continue;
                    } else {
                        if (i == urls.length) {
                            normalizedISBN10 = book.getNormalizedISBN(true,Bridge.Book.ISBNNormalization.ISBN_CONVERT_TO_10);
                            if ("".equals(normalizedISBN10)) continue;
                            url = "http://covers.openlibrary.org/b/isbn/"+normalizedISBN10+"-M.jpg?default=false";
                        } else if (i == urls.length + 1) {
                            if ("".equals(normalizedISBN10)) continue;
                            url = "http://images-eu.amazon.com/images/P/" + normalizedISBN10 + ".03.L.jpg";
                        } else {
                            String isbn = book.getNormalizedISBN(true,Bridge.Book.ISBNNormalization.ISBN_CONVERT_TO_13);
                            if ("".equals(isbn)) continue;
                            url = "http://vlb.de/GetBlob.aspx?strIsbn=" + isbn + "&size=M";
                        }
                    }


                    InputStream in = new java.net.URL(url).openStream();
                    cover = BitmapFactory.decodeStream(in, null, bitmapOpts);
                    if (cover != null && cover.getWidth() > 3 && cover.getHeight() > 3) {
                        int longSide = Math.max(displayMetrics.widthPixels, displayMetrics.heightPixels);
                        int shortSide = Math.min(displayMetrics.widthPixels, displayMetrics.heightPixels);
                        //portrait: maxWidth = shortSide, maxHeight = longSide * factor
                        //landscape: maxWidth = longSide / 2, maxHeight = shortSide * factor
                        int maxWidth =  Math.max(3, Math.min(shortSide, longSide / 2) );
                        int maxHeight = Math.max(3,shortSide / 2 );
                        int minWidth = maxWidth / 4;
                        int minHeight = maxHeight / 4;
                        double scale = 1;
                        if (cover.getWidth() < minWidth || cover.getHeight() < minHeight) {
                            scale = Math.max(minWidth * 1.0 / cover.getWidth(), minHeight * 1.0 / cover.getHeight());
                        }
                        if (cover.getWidth() * scale > maxWidth || cover.getHeight() * scale > maxHeight) {
                            scale = Math.min(maxWidth * 1.0 / cover.getWidth(), maxHeight * 1.0 / cover.getHeight());
                        }
                        if (scale != 1) {
                            Bitmap oldCover = cover;
                            cover = Bitmap.createScaledBitmap(cover, (int)(cover.getWidth() * scale), (int)(cover.getHeight()  * scale), true);
                            oldCover.recycle();
                        }
                        return cover;

                    }
                } catch (Throwable e) { //need to catch OutOfMemoryError and broken images exceptions
                    //Log.e("Error", e.getMessage());
                    e.printStackTrace();
                }
            }
            return cover;
        }

        protected void onPostExecute(Bitmap result) {
            if (VideLibriApp.currentActivity != null
                    && VideLibriApp.currentActivity instanceof BookListActivity
                    ) {
                ((BookListActivity)VideLibriApp.currentActivity).endLoading(VideLibriBaseActivity.LOADING_COVER_IMAGE);
                book.image = result;
                if (VideLibriApp.currentActivity == fragment.activity && book == fragment.book) fragment.updateImage();
            }
        }
    }
}
