package de.benibela.videlibri;

import android.content.pm.PackageManager;
import android.os.Bundle;
import android.util.Xml;
import android.widget.ListView;
import org.xmlpull.v1.XmlPullParser;
import org.xmlpull.v1.XmlPullParserException;

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;

/**
 * Created with IntelliJ IDEA.
 * User: benito
 * Date: 6/6/13
 * Time: 2:36 PM
 * To change this template use File | Settings | File Templates.
 */
public class About extends VideLibriBaseActivity {
    XmlPullParser parser;
    ArrayList<BookDetails.Details> details;
    BookDetails.Details curDetails;
    String firstVersion = "";

    void parseChangelog() throws IOException, XmlPullParserException {
        parser.require(XmlPullParser.START_TAG, null, "changelog");
        while (parser.next() != XmlPullParser.END_TAG) {
            if (parser.getEventType() != XmlPullParser.START_TAG)
                continue;
            if ("build".equals(parser.getName())) parseBuild();
            else skip();
        }
    }

    void parseBuild() throws IOException, XmlPullParserException {
        String version = parser.getAttributeValue(null, "version");
        if ("".equals(firstVersion)) firstVersion = version;

        curDetails = new BookDetails.Details(tr(R.string.about_version_date,  Util.strToIntDef(version, 0) / 1000.0, parser.getAttributeValue(null, "date")), "");
        details.add(curDetails);
        while (parser.next() != XmlPullParser.END_TAG) {
            if (parser.getEventType() != XmlPullParser.START_TAG)
                continue;
            switch (parser.getName()) {
                case "add":
                    curDetails.data = curDetails.data + "\n" + " (+) " + parseText();
                    break;
                case "change":
                    curDetails.data = curDetails.data + "\n" + " (*) " + parseText();
                    break;
                case "fix":
                    curDetails.data = curDetails.data + "\n" + " (f) " + parseText();
                    break;
                default:
                    skip();
                    break;
            }
        }
    }

    String parseText() throws IOException, XmlPullParserException {
        String result = "";
        while (parser.next() != XmlPullParser.END_TAG)
            if (parser.getEventType() == XmlPullParser.TEXT)
                result += parser.getText();
        return result;
    }

    //from http://developer.android.com/training/basics/network-ops/xml.html
    private void skip() throws XmlPullParserException, IOException {
        if (parser.getEventType() != XmlPullParser.START_TAG) {
            throw new IllegalStateException();
        }
        int depth = 1;
        while (depth != 0) {
            switch (parser.next()) {
                case XmlPullParser.END_TAG:
                    depth--;
                    break;
                case XmlPullParser.START_TAG:
                    depth++;
                    break;
            }
        }
    }

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);    //To change body of overridden methods use File | Settings | File Templates.
        setVideLibriView(R.layout.bookdetails);
        ListView lv = (ListView) findViewById(R.id.bookdetailsview);

        details = new ArrayList<>();
        parser = Xml.newPullParser();
        try {
            parser.setFeature(XmlPullParser.FEATURE_PROCESS_NAMESPACES, false);
            InputStream file = this.getAssets().open("changelog.xml");
            parser.setInput(file, null);
            parser.nextTag();
            parseChangelog();
        } catch (XmlPullParserException e) {
            e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
        } catch (IOException e) {
            e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
        }


        try {
            details.add(0, new BookDetails.Details(tr(R.string.version), "VideLibri "+getPackageManager().getPackageInfo("de.benibela.videlibri", 0).versionName ));
        } catch (PackageManager.NameNotFoundException e) {
            details.add(0, new BookDetails.Details(tr(R.string.version), "VideLibri "+(Util.strToIntDef(firstVersion, 0) / 1000.0)  + " ??"));
        }
        details.add(1, new BookDetails.Details(tr(R.string.homepage), "http://www.videlibri.de"));
        details.add(2, new BookDetails.Details(tr(R.string.source), "http://sourceforge.net/p/videlibri/code/ci/trunks/tree/"));
        details.add(3, new BookDetails.Details("Covers", "http://www.openlibrary.org ; http://amazon.com ; http://www.buchhandel.de"));


        lv.setAdapter(new BookDetails.BookDetailsAdapter(this, details, new Bridge.Book()));
    }
}
