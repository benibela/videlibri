package de.benibela.videlibri.activities

import android.os.Bundle
import android.util.Pair
import android.view.LayoutInflater
import android.view.View
import android.widget.*
import android.widget.AdapterView.OnItemSelectedListener
import de.benibela.videlibri.R
import de.benibela.videlibri.jni.Bridge
import de.benibela.videlibri.jni.Bridge.LibraryDetails
import de.benibela.videlibri.jni.Bridge.TemplateDetails
import de.benibela.videlibri.utils.Util
import de.benibela.videlibri.utils.showMessage
import java.util.*

class NewLibrary : VideLibriBaseActivity() {
    var details: LibraryDetails? = null
    var variables = HashMap<String, Pair<String, EditText?>>()
    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState) //To change body of overridden methods use File | Settings | File Templates
        setVideLibriView(R.layout.newlib)
        findButtonById(R.id.install).setOnClickListener {
            Bridge.VLInstallLibrary(getTextViewText(R.id.url))
            beginLoading(LOADING_INSTALL_LIBRARY)
        }
        val templates = Bridge.VLGetTemplates()
        val templatesSpinner = findViewById<View>(R.id.template) as Spinner
        templatesSpinner.adapter = makeAdapterStrings(templates)
        templatesSpinner.onItemSelectedListener = object : OnItemSelectedListener {
            override fun onItemSelected(adapterView: AdapterView<*>?, view: View, i: Int, l: Long) {
                if (i < 0 || i >= templates.size) return
                selectTemplate(templates[i])
            }

            override fun onNothingSelected(adapterView: AdapterView<*>?) {}
        }
        val mode = intent.getIntExtra("mode", 0)
        if (mode == MODE_LIBRARY_MODIFY) {
            val id = intent.getStringExtra("libId") ?: return
            details = Bridge.VLGetLibraryDetails(id)
            if (details == null) return
            for (i in details!!.variableNames.indices) variables[details!!.variableNames[i]] = Pair(details!!.variableValues[i], null)
            for (i in templates.indices) if (details!!.templateId == templates[i]) {
                templatesSpinner.setSelection(i)
                break
            }
            //selectTemplate(details.templateId);
            findViewById<View>(R.id.createGroup).visibility = View.GONE
            findButtonById(R.id.create).setText(R.string.change)
            setTextViewText(R.id.id, id)
            setTextViewText(R.id.name, details!!.prettyName)
            //(
            /*ArrayAdapter<CharSequence> adapter = ArrayAdapter.createFromResource(this,
        R.array.planets_array, android.R.layout.simple_spinner_item);
// Specify the layout to use when the list of choices appears
adapter.
// Apply the adapter to the spinner
spinner.setAdapter(adapter);*/findViewById<View>(R.id.create).setOnClickListener {
                var newId = getTextViewText(R.id.id)
                val sublocs = newId.split("_").toTypedArray().size
                for (i in sublocs..3) newId = "-_$newId"
                details!!.prettyName = getTextViewText(R.id.name)
                if (!Util.equalStrings(newId, id)) Bridge.VLSetLibraryDetails(id, null)
                Bridge.VLSetLibraryDetails(newId, details)
                finish()
            }
            findViewById<View>(R.id.deleteButton).setOnClickListener {
                Bridge.VLSetLibraryDetails(id, null)
                finish()
            }
        } else setTextViewText(R.id.id, "user" + (Math.random() * 1000).toInt())
        findViewById<View>(R.id.create).setOnClickListener {
            var newId = getTextViewText(R.id.id)
            val sublocs = newId.split("_").toTypedArray().size
            for (i in sublocs..3) newId = "-_$newId"
            if (details == null) details = LibraryDetails()
            details!!.prettyName = getTextViewText(R.id.name)
            if (mode == MODE_LIBRARY_MODIFY && intent.getStringExtra("libId") != null) {
                val id = intent.getStringExtra("libId")
                if (newId != id) Bridge.VLSetLibraryDetails(id, null)
            }
            details!!.id = newId
            details!!.templateId = templatesSpinner.selectedItem.toString()
            details!!.variableNames = arrayOfNulls(variables.size)
            details!!.variableValues = arrayOfNulls(variables.size)
            var i = 0
            for ((key, value) in variables) {
                details!!.variableNames[i] = key
                if (value.second == null) details!!.variableValues[i] = value.first else details!!.variableValues[i] = value.second!!.text.toString()
                i += 1
            }
            Bridge.VLSetLibraryDetails(newId, details)
            finish()
        }
        if (mode == MODE_LIBRARY_ENTER_NEW_DATA || mode == MODE_LIBRARY_MODIFY) {
            val sv = findViewById<View>(R.id.scrollView) as ScrollView
            val target = findViewById<View>(R.id.name)
            target.requestFocus()
            //sv.requestChildFocus(target, target);
        }
    }

    fun addTemplateVariable(linearLayout: LinearLayout, inflater: LayoutInflater, name: String, desc: String?, value: String?): EditText {
        var value = value
        val option = inflater.inflate(R.layout.newliboption, null)
        val text = option.findViewById<View>(R.id.text) as TextView
        text.text = if (desc == null) name else "$name ( $desc)"
        val edit = option.findViewById<View>(R.id.edit) as EditText
        if (value == null) value = ""
        edit.setText(value)
        linearLayout.addView(option)
        variables[name] = Pair(value, edit)
        return edit
    }

    fun selectTemplate(template: String?) {
        var details: TemplateDetails? = null
        try {
            details = Bridge.VLGetTemplateDetails(template!!)
        } catch (e: Bridge.InternalError) {
            showMessage(e.localizedMessage)
        }
        if (details == null) return
        val oldVariables = variables
        variables = HashMap()
        val oldValues = HashMap<String, String>()
        for ((key, value) in oldVariables) if (value.second == null) oldValues[key] = value.first else if (value.second!!.text.toString() != value.first) oldValues[key] = value.second!!.text.toString()
        val linearLayout = findViewById<View>(R.id.options) as LinearLayout
        linearLayout.removeAllViews()
        val inflater = layoutInflater
        for (i in details.variablesNames.indices) {
            val et = addTemplateVariable(linearLayout, inflater, details.variablesNames[i], details.variablesDescription[i], details.variablesDefault[i])
            if (oldValues.containsKey(details.variablesNames[i])) et.setText(oldValues[details.variablesNames[i]])
        }
        for ((key, value) in oldValues) if (!variables.containsKey(key)) addTemplateVariable(linearLayout, inflater, key, null, "").setText(value)
    }

    companion object {
        const val MODE_LIBRARY_MODIFY = 1237
        const val MODE_LIBRARY_ENTER_NEW_DATA = 1238
    }
}