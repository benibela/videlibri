package de.benibela.videlibri

import android.content.pm.PackageManager
import android.os.Bundle
import android.util.Xml
import android.view.View
import android.widget.ListView
import androidx.recyclerview.widget.RecyclerView
import de.benibela.videlibri.BookDetails.BookDetailsAdapter
import de.benibela.videlibri.jni.Bridge
import org.xmlpull.v1.XmlPullParser
import org.xmlpull.v1.XmlPullParserException
import java.io.IOException
import java.io.InputStream
import kotlin.collections.ArrayList


class ChangeLogParser(input: InputStream) {
    private val parser: XmlPullParser = Xml.newPullParser()
    var firstVersion = ""
    val versions = arrayListOf<Triple<Float, String, ArrayList<String>>>()
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
        val date = parser.getAttributeValue(null, "date")
        if ("" == firstVersion) firstVersion = version
        val changes = arrayListOf<String>()
        while (parser.next() != XmlPullParser.END_TAG) {
            if (parser.eventType != XmlPullParser.START_TAG) continue
            when (parser.name) {
                "add" -> changes += " (+) ${parseText()}"
                "change" -> changes += " (*) ${parseText()}"
                "fix" -> changes += " (f) ${parseText()}"
                else -> skip()
            }
        }
        versions += Triple((version.toIntOrNull() ?: 0) / 1000.0f, date, changes)
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
                XmlPullParser.END_DOCUMENT -> return
            }
        }
    }

    init {
        try {
            parser.setFeature(XmlPullParser.FEATURE_PROCESS_NAMESPACES, false)
            parser.setInput(input, null)
            parser.nextTag()
            parseChangelog()
        } catch (e: XmlPullParserException) {
            e.printStackTrace()
        } catch (e: IOException) {
            e.printStackTrace()
        }
    }
}

class About : VideLibriBaseActivity() {
    val details = arrayListOf<BookDetails.Details>()
    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState) //To change body of overridden methods use File | Settings | File Templates.
        setVideLibriView(R.layout.bookdetails)
        val changelog = ChangeLogParser(this.assets.open("changelog.xml"))
        details.clear()
        try {
            details.add(0, BookDetails.Details(getString(R.string.version), "VideLibri " + packageManager.getPackageInfo("de.benibela.videlibri", 0).versionName))
        } catch (e: PackageManager.NameNotFoundException) {
            details.add(0, BookDetails.Details(getString(R.string.version), "VideLibri " + (changelog.firstVersion.toIntOrNull() ?: 0) / 1000.0 + " ??"))
        }
        details.add(1, BookDetails.Details(getString(R.string.homepage), "http://www.videlibri.de"))
        details.add(2, BookDetails.Details(getString(R.string.about_manual), "http://www.videlibri.de/help"))
        details.add(3, BookDetails.Details(getString(R.string.source), "https://sourceforge.net/p/videlibri/code/ci/trunks/tree/"))
        details.add(4, BookDetails.Details(getString(R.string.about_bugtracker), "https://sourceforge.net/p/videlibri/tickets/"))
        details.add(5, BookDetails.Details("Covers", "http://www.openlibrary.org ; http://amazon.com ; http://www.buchhandel.de"))

        changelog.versions.forEach { (version, date, changes) ->
            details.add(BookDetails.Details(getString(R.string.about_version_date, version, date), changes.joinToString ("\n")))
        }


        findViewById<RecyclerView>(R.id.bookDetailsRecyclerView).adapter = BookDetailsAdapter(this, details, Bridge.Book())
    }
}