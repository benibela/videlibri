package de.benibela.videlibri.activities

import android.os.Bundle
import android.view.View
import android.view.ViewGroup
import android.widget.AdapterView
import android.widget.ArrayAdapter
import android.widget.Spinner
import androidx.recyclerview.widget.LinearLayoutManager
import androidx.recyclerview.widget.RecyclerView
import de.benibela.videlibri.R
import de.benibela.videlibri.components.BookDetails
import de.benibela.videlibri.jni.Bridge
import de.benibela.videlibri.utils.useLines
import java.io.IOException
import java.util.*
import java.util.regex.Pattern

private class DebugLog{
    private val list = ArrayList<BookDetails.Details>()
    private var lastDetails: BookDetails.Details? = null
    private val builder = StringBuilder()
    fun appendNewEntry(header: String, data: String){
        closeLastEntry()
        lastDetails = BookDetails.Details(header, data).also { list.add(it) }
    }
    fun appendToLastEntry(data: String, alwaysShow: Boolean) {
        if (lastDetails == null) {
            if (alwaysShow)
                appendNewEntry("", data)
        } else {
            if (builder.isEmpty())
                builder.append(lastDetails?.data)
            builder.append('\n')
            builder.append(data)
        }
    }
    fun closeLastEntry(){
        if (builder.isNotEmpty()) {
            lastDetails?.data = builder.toString()
            builder.clear()
        }
        lastDetails = null
    }
    fun toArrayList(): ArrayList<BookDetails.Details> {
        closeLastEntry()
        return list
    }
}

class DebugLogViewer : VideLibriBaseActivity(), AdapterView.OnItemSelectedListener {
    internal var details = ArrayList<BookDetails.Details>()
    private lateinit var filterSpinner: Spinner

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setVideLibriView(R.layout.bookdetails)

        filterSpinner = Spinner(this)
        filterSpinner.onItemSelectedListener = this
        filterSpinner.adapter = ArrayAdapter(this, android.R.layout.simple_spinner_item,
                arrayOf(R.string.debug_log_http, R.string.debug_log_videlibri, R.string.debug_log_all).flatMap {
                    listOf(getString(it, getString(R.string.debug_order_new_first)), getString(it, getString(R.string.debug_order_old_first)))
                }
        ).apply {
            setDropDownViewResource(android.R.layout.simple_spinner_dropdown_item)
        }


        findViewById<androidx.appcompat.widget.Toolbar>(R.id.actionbar).addView(filterSpinner, ViewGroup.LayoutParams.WRAP_CONTENT, ViewGroup.LayoutParams.MATCH_PARENT)

        val pos = savedInstanceState?.getInt("pos", 0) ?: 0
        filterSpinner.setSelection(pos)
        displayLog(pos / 2, pos % 2 == 0)
    }

    override fun onSaveInstanceState(outState: Bundle) {
        super.onSaveInstanceState(outState)
        outState.putInt("pos", filterSpinner.selectedItemPosition)
    }

    @Suppress("LocalVariableName")
    private fun displayLog(mode: Int, reverse: Boolean) {
        val MODE_HTTP = 0
        val MODE_VIDELIBRI = 1
        val MODE_ALL = 2

        val log = DebugLog()
        val pattern = Pattern.compile("^.*?(Vide[Ll]ibri.*?:)( *[-0-9:T ]+[(].*?[)] *:)?(.*)")
        val patternHttp = Pattern.compile("^\\s*(GET|POST).*")

        try {
            //http://stackoverflow.com/questions/12692103/read-logcat-programmatically-within-application
            Runtime.getRuntime().exec("logcat -d").inputStream.useLines { line ->
                val m = pattern.matcher(line)
                if (!m.matches()) {
                    if (mode == MODE_ALL)
                        log.appendNewEntry("", line)
                    else
                        log.closeLastEntry()
                } else {
                    var data = m.group(3) ?: ""
                    if (data.length > 200000) data = data.substring(0, 200000)
                    if (m.group(2).isNullOrEmpty())
                        log.appendToLastEntry(data, mode == MODE_VIDELIBRI)
                    else if (mode == MODE_HTTP && !patternHttp.matcher(data).matches())
                        log.closeLastEntry()
                    else
                        log.appendNewEntry(( m.group(1) ?: "" )+ m.group(2), data)
                }
            }

        } catch (e: IOException) {
            log.appendNewEntry("ERROR", "Failed to start logcat.\nOn a rooted device you can view the logcat with any other logcat viewing app.")
        } catch (e: OutOfMemoryError) {
            System.gc()
            log.appendNewEntry("ERROR", "Out of memory")
        }
        if (android.os.Build.VERSION.SDK_INT < android.os.Build.VERSION_CODES.JELLY_BEAN)
            log.appendNewEntry("ERROR", "You need at least Android 4.1 to view the debug log on a non-rooted device.")

        if (!Bridge.VLGetOptions().logging)
            log.appendNewEntry("", tr(R.string.debuglog_disabled))


        val details = log.toArrayList()
        if (reverse)
            details.reverse()

        val lv = findViewById<RecyclerView>(R.id.bookDetailsRecyclerView)
        lv.layoutManager = LinearLayoutManager(this)
        lv.adapter = BookDetails.BookDetailsAdapter(this, details, Bridge.Book())
    }

    override fun onItemSelected(parent: AdapterView<*>?, view: View?, position: Int, id: Long) {
        displayLog(position / 2, position % 2 == 0)
        System.gc()
    }

    override fun onNothingSelected(parent: AdapterView<*>?) {

    }
}
