package de.benibela.videlibri;

import android.os.Bundle;
import android.util.Xml;
import android.widget.ListView;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class DebugLogViewer extends VideLibriBaseActivity {
    ArrayList<BookDetails.Details> details;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);    //To change body of overridden methods use File | Settings | File Templates.
        setContentView(R.layout.bookdetails);
        ListView lv = (ListView) findViewById(R.id.bookdetailsview);

        details = new ArrayList<BookDetails.Details>();

        Bridge.Options options = Bridge.VLGetOptions();
        if (!options.logging)
            details.add(new BookDetails.Details("", tr(R.string.debuglog_disabled)));

        try {
            //http://stackoverflow.com/questions/12692103/read-logcat-programmatically-within-application
            Process process = Runtime.getRuntime().exec("logcat -d");
            BufferedReader bufferedReader = new BufferedReader(new InputStreamReader(process.getInputStream()));

            String line = "";
            Pattern pattern = Pattern.compile("([a-zA-Z]+/VideLibri.+?:)( *[-0-9:T ]+[(].*?[)] *:)?(.*)");
            BookDetails.Details lastDetails = null;
            while ((line = bufferedReader.readLine()) != null) {
                Matcher m = pattern.matcher(line);
                if (!m.matches()) {
                    details.add(new BookDetails.Details("", line));
                    lastDetails = null;
                } else if (lastDetails != null && (m.group(2) == null || m.group(2).isEmpty()))  {
                    lastDetails.data += lastDetails.data + "\n" + m.group(3);
                } else {
                    lastDetails = new BookDetails.Details(m.group(1) + m.group(2), m.group(3));
                    details.add(lastDetails);
                }
            }

        }
        catch (IOException e) {}

        /*
        D/SherlockFragmentActivity( 1833): [onPreparePanel] returning true
I/VideLibri( 1833): 2016-03-30:11:46:45:726 (3087219496):  ** Read variable: "raise()" = "string: Systemmeldung
I/VideLibri( 1833):
I/VideLibri( 1833): Ihre Anforderung konnte aus den folgenden Gründen nicht durchgeführt werden: Fehler
I/VideLibri( 1833): Sie haben vergessen einen Suchbegriff einzugeben."
I/VideLibri( 1833): 2016-03-30:11:46:45:849 (3087219496):  createErrorMessageStr: Exception: EBookListReader:
I/VideLibri( 1833):
I/VideLibri( 1833): Systemmeldung
I/VideLibri( 1833):
I/VideLibri( 1833): Ihre Anforderung konnte aus den folgenden Gründen nicht durchgeführt werden: Fehler
I/VideLibri( 1833): Sie haben vergessen einen Suchbegriff einzugeben.
I/VideLibri( 1833):       Details:
I/VideLibri( 1833): Detaillierte Informationen über die entsprechende Quellcodestelle:
I/VideLibri( 1833):   $A87B4C9C
I/VideLibri( 1833):   $A87B3F08
I/VideLibri( 1833):   $A87B7FF4
I/VideLibri( 1833):   $A87BB090
I/VideLibri( 1833):   $A87BA524
I/VideLibri( 1833):   $A87BB090
I/VideLibri( 1833):   $A87BAA68
I/VideLibri( 1833):   $A87BCA94
I/VideLibri( 1833):   $A87BCA00
I/VideLibri( 1833):   $A88BCEC8
I/VideLibri( 1833):   $A88BB370
I/VideLibri( 1833):   $A85032FC
I/VideLibri( 1833): 2016-03-30:11:46:45:852 (3087219496):  TLibrarySearcherAccess.threadException called
I/VideLibri( 1833): 2016-03-30:11:46:45:857 (3087219496):  Searcher thread: wait for message


I/VideLibri( 1833): 2016-03-30:11:46:43:495 (3087219496):  ** Read variable: "f" = "
         */

        lv.setAdapter(new BookDetails.BookDetailsAdapter(this, details, new Bridge.Book()));
    }
}
