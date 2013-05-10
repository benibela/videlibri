package de.benibela.videlibri;


import android.app.Activity;
import android.graphics.Typeface;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ArrayAdapter;
import android.widget.BaseAdapter;
import android.widget.ListView;
import android.widget.TextView;

import java.util.*;

public class BookDetails extends Activity {
    Bridge.Book book;

    static class Details{
        String name, data;
        Details(String name, String data){
            this.name = name;
            this.data = data;
            if (name == null) this.name = "??";
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

            this.defaultColor = context.getResources().getColor(android.R.color.primary_text_dark);

            this.scale = context.getResources().getDisplayMetrics().scaledDensity;
        }

        static class ViewHolder {
            public TextView text;
        }

        @Override
        public int getCount() {
            return 2 * details.size();
        }

        @Override
        public Object getItem(int i) {
            return details.get(i/2);
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
                view.setTag(viewHolder);
            }
            ViewHolder holder = (ViewHolder) view.getTag();
            Details d = details.get(position/2);
            int c =  defaultColor;
            if (position % 2 == 0) {
                holder.text.setTypeface(Typeface.DEFAULT_BOLD);
                holder.text.setText(d.name);
                holder.text.setPadding(toPx(10),toPx(1),toPx(10),toPx(1));
            } else {
                holder.text.setTypeface(Typeface.DEFAULT);
                holder.text.setText(d.data);
                holder.text.setPadding(toPx(30),toPx(1),toPx(10),toPx(2));
                if ("Bemerkung".equals(d.name) || "Abgabefrist".equals(d.name)){
                    c = book.getStatusColor();
                    if (c == -1) c = defaultColor;
                }
            }
            holder.text.setTextColor(c);


            return view;
        }
    }

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        book = (Bridge.Book) getIntent().getSerializableExtra("book");
        if (book == null) book = new Bridge.Book();

        setContentView(R.layout.bookdetails);

        ListView lv = (ListView) findViewById(R.id.bookdetailsview);


        ArrayList<Details> details = new ArrayList<Details>();
        details.add(new Details("Titel", book.title));
        details.add(new Details("Verfasser", book.author));
        details.add(new Details("Abgabefrist", book.dueDatePretty));
        details.add(new Details("Ausleihdatum", android.text.format.DateFormat.getDateFormat(this).format(book.issueDate.getTime())));
        details.add(new Details("Bemerkung", book.more.get("status")));
        if (book.account != null) details.add(new Details("Konto", book.account.prettyName));

        details.add(new Details("ID", book.more.get("id")));
        details.add(new Details("Kategorie", book.more.get("category")));
        details.add(new Details("Jahr", book.more.get("year")));

        final List<String> above = Arrays.asList(new String[]{"status", "id", "category", "year"});

        for (Map.Entry<String, String> entry : book.more.entrySet())
            if (entry.getValue() != null && !"".equals(entry.getValue()) && !above.contains(entry.getKey()))
                details.add(new Details(entry.getKey(), entry.getValue()));

        lv.setAdapter(new BookDetailsAdapter(this, details, book));
    }
}
