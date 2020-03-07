package de.benibela.videlibri

import android.content.pm.PackageManager
import android.os.Bundle
import android.util.Xml
import android.view.View
import android.widget.ListView
import de.benibela.videlibri.BookDetails.BookDetailsAdapter
import de.benibela.videlibri.jni.Bridge
import org.xmlpull.v1.XmlPullParser
import org.xmlpull.v1.XmlPullParserException
import java.io.IOException
import java.util.*

class About : VideLibriBaseActivity() {
    lateinit var parser: XmlPullParser
    val details = arrayListOf<BookDetails.Details>()
    var firstVersion = ""
    @Throws(IOException::class, XmlPullParserException::class)
    fun parseChangelog() {
        parser.require(XmlPullParser.START_TAG, null, "changelog")
        while (parser.next() != XmlPullParser.END_TAG) {
            if (parser.eventType != XmlPullParser.START_TAG) continue
            if ("build" == parser.name) parseBuild() else skip()
        }
    }

    @Throws(IOException::class, XmlPullParserException::class)
    fun parseBuild() {
        val version = parser.getAttributeValue(null, "version")
        if ("" == firstVersion) firstVersion = version
        val curDetails = BookDetails.Details(tr(R.string.about_version_date, Util.strToIntDef(version, 0) / 1000.0, parser.getAttributeValue(null, "date")), "")
        details.add(curDetails)
        while (parser.next() != XmlPullParser.END_TAG) {
            if (parser.eventType != XmlPullParser.START_TAG) continue
            when (parser.name) {
                "add" -> curDetails.data = curDetails.data.toString() + "\n" + " (+) " + parseText()
                "change" -> curDetails.data = curDetails.data.toString() + "\n" + " (*) " + parseText()
                "fix" -> curDetails.data = curDetails.data.toString() + "\n" + " (f) " + parseText()
                else -> skip()
            }
        }
    }

    @Throws(IOException::class, XmlPullParserException::class)
    fun parseText(): String {
        var result = ""
        while (parser.next() != XmlPullParser.END_TAG) if (parser.eventType == XmlPullParser.TEXT) result += parser.text
        return result
    }

    //from http://developer.android.com/training/basics/network-ops/xml.html
    @Throws(XmlPullParserException::class, IOException::class)
    private fun skip() {
        check(parser.eventType == XmlPullParser.START_TAG)
        var depth = 1
        while (depth != 0) {
            when (parser.next()) {
                XmlPullParser.END_TAG -> depth--
                XmlPullParser.START_TAG -> depth++
            }
        }
    }

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState) //To change body of overridden methods use File | Settings | File Templates.
        setVideLibriView(R.layout.bookdetails)
        val lv = findViewById<View>(R.id.bookdetailsview) as ListView
        details.clear()
        parser = Xml.newPullParser()
        try {
            parser.setFeature(XmlPullParser.FEATURE_PROCESS_NAMESPACES, false)
            val file = this.assets.open("changelog.xml")
            parser.setInput(file, null)
            parser.nextTag()
            parseChangelog()
        } catch (e: XmlPullParserException) {
            e.printStackTrace()
        } catch (e: IOException) {
            e.printStackTrace()
        }
        try {
            details.add(0, BookDetails.Details(tr(R.string.version), "VideLibri " + packageManager.getPackageInfo("de.benibela.videlibri", 0).versionName))
        } catch (e: PackageManager.NameNotFoundException) {
            details.add(0, BookDetails.Details(tr(R.string.version), "VideLibri " + Util.strToIntDef(firstVersion, 0) / 1000.0 + " ??"))
        }
        details.add(1, BookDetails.Details(tr(R.string.homepage), "http://www.videlibri.de"))
        details.add(2, BookDetails.Details(tr(R.string.about_manual), "http://www.videlibri.de/help"))
        details.add(3, BookDetails.Details(tr(R.string.source), "https://sourceforge.net/p/videlibri/code/ci/trunks/tree/"))
        details.add(4, BookDetails.Details(tr(R.string.about_bugtracker), "https://sourceforge.net/p/videlibri/tickets/"))
        details.add(5, BookDetails.Details("Covers", "http://www.openlibrary.org ; http://amazon.com ; http://www.buchhandel.de"))
        lv.adapter = BookDetailsAdapter(this, details, Bridge.Book())
    }
}