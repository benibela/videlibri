package de.benibela.videlibri;

import android.os.Bundle;
import android.view.View;
import android.view.ViewGroup;
import android.widget.AdapterView;
import android.widget.ArrayAdapter;
import android.widget.ListView;
import android.widget.Spinner;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Collections;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class DebugLogViewer extends VideLibriBaseActivity implements AdapterView.OnItemSelectedListener {
    ArrayList<BookDetails.Details> details;
    private Spinner filterSpinner;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);    //To change body of overridden methods use File | Settings | File Templates.
        setVideLibriView(R.layout.bookdetails);

        filterSpinner = new Spinner(this);
        ArrayAdapter<String> filterSpinnerAdapter = new ArrayAdapter<String>(this, android.R.layout.simple_spinner_item, new String[]{
                tr(R.string.debug_log_http, tr(R.string.debug_order_new_first)),
                tr(R.string.debug_log_http, tr(R.string.debug_order_old_first)),
                tr(R.string.debug_log_videlibri, tr(R.string.debug_order_new_first)),
                tr(R.string.debug_log_videlibri, tr(R.string.debug_order_old_first)),
                tr(R.string.debug_log_all, tr(R.string.debug_order_new_first)),
                tr(R.string.debug_log_all, tr(R.string.debug_order_old_first))
        });
        filterSpinnerAdapter.setDropDownViewResource(android.R.layout.simple_spinner_dropdown_item);
        filterSpinner.setAdapter(filterSpinnerAdapter);
        filterSpinner.setOnItemSelectedListener(this);


        android.support.v7.widget.Toolbar bar = (android.support.v7.widget.Toolbar) findViewById(R.id.actionbar);
        bar.addView(filterSpinner, ViewGroup.LayoutParams.WRAP_CONTENT, ViewGroup.LayoutParams.MATCH_PARENT);

        int pos = savedInstanceState != null ? savedInstanceState.getInt("pos", 0) : 0;
        filterSpinner.setSelection(pos);
        displayLog(pos / 2, pos % 2 == 0);
    }

    @Override
    protected void onSaveInstanceState(Bundle outState) {
        super.onSaveInstanceState(outState);
        outState.putInt("pos", filterSpinner.getSelectedItemPosition());
    }

    protected void displayLog(int mode, boolean reverse){
        final int MODE_HTTP = 0;
        final int MODE_VIDELIBRI = 1;
        final int MODE_ALL = 2;
        details = new ArrayList<BookDetails.Details>();

        Bridge.Options options = Bridge.VLGetOptions();
        try {
            //http://stackoverflow.com/questions/12692103/read-logcat-programmatically-within-application
            Process process = Runtime.getRuntime().exec("logcat -d");
            BufferedReader bufferedReader = new BufferedReader(new InputStreamReader(process.getInputStream()));

            String line = "";
            Pattern pattern = Pattern.compile("([a-zA-Z]+/VideLibri.+?:)( *[-0-9:T ]+[(].*?[)] *:)?(.*)");
            Pattern patternHttp = Pattern.compile("^\\s*(GET|POST).*");
            BookDetails.Details lastDetails = null;
            while ((line = bufferedReader.readLine()) != null) {
                try {
                    Matcher m = pattern.matcher(line);
                    if (!m.matches()) {
                        if (mode == MODE_ALL)
                            details.add(new BookDetails.Details("", line));
                        lastDetails = null;
                    } else {
                        String temp = m.group(3);
                        if (temp.length() > 200000) temp = "[too large]";
                        if (lastDetails != null && (m.group(2) == null || m.group(2).isEmpty()))  {
                            if (lastDetails.data.length() < 500000)
                                lastDetails.data = lastDetails.data + "\n" + temp;
                        } else {
                            lastDetails = new BookDetails.Details(m.group(1) + m.group(2), temp);
                            if (mode >= MODE_VIDELIBRI || patternHttp.matcher(temp).matches())
                                details.add(lastDetails);
                        }
                    }
                } catch (OutOfMemoryError e) {

                }
            }

        }
        catch (IOException e) {}

        if (!options.logging)
            details.add(new BookDetails.Details("", tr(R.string.debuglog_disabled)));

        if (reverse) Collections.reverse(details);

        ListView lv = (ListView) findViewById(R.id.bookdetailsview);
        lv.setAdapter(new BookDetails.BookDetailsAdapter(this, details, new Bridge.Book()));
    }

    @Override
    public void onItemSelected(AdapterView<?> parent, View view, int position, long id) {
        displayLog(position/2, position%2 == 0);
        System.gc();
    }

    @Override
    public void onNothingSelected(AdapterView<?> parent) {

    }
}
