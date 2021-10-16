package de.benibela.videlibri.activities

import android.os.Bundle
import android.view.LayoutInflater
import android.view.View
import android.widget.AdapterView
import android.widget.AdapterView.OnItemSelectedListener
import android.widget.EditText
import android.widget.LinearLayout
import de.benibela.videlibri.R
import de.benibela.videlibri.databinding.NewlibBinding
import de.benibela.videlibri.databinding.NewliboptionBinding
import de.benibela.videlibri.jni.Bridge
import de.benibela.videlibri.jni.Bridge.LibraryDetails
import de.benibela.videlibri.utils.countOf
import de.benibela.videlibri.utils.showMessage
import de.benibela.videlibri.utils.showMessageYesNo
import de.benibela.videlibri.utils.withActivity
import kotlinx.android.synthetic.main.newlib.*

data class LibraryVariable(val defaultValue: String, val editText: EditText?)

class NewLibrary : VideLibriBaseActivity() {
    protected lateinit var binding: NewlibBinding

    var details: LibraryDetails? = null
    var variables = HashMap<String, LibraryVariable>()
    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        binding = setVideLibriView(NewlibBinding::inflate)
        val templates = Bridge.VLGetTemplates()
        binding.templateSpinner.adapter = makeAdapterStrings(templates)
        binding.templateSpinner.onItemSelectedListener = object : OnItemSelectedListener {
            override fun onItemSelected(adapterView: AdapterView<*>?, view: View, i: Int, l: Long) {
                templates.getOrNull(i)?.let(::selectTemplate)
            }
            override fun onNothingSelected(adapterView: AdapterView<*>?) {}
        }
        val mode = intent.getIntExtra("mode", 0)
        if (mode == MODE_LIBRARY_MODIFY) {
            val id = intent.getStringExtra("libId") ?: return
            details = Bridge.VLGetLibraryDetails(id)
            val details = details ?: return
            for (i in details.variableNames.indices) variables[details.variableNames[i]] = LibraryVariable(details.variableValues[i], null)
            val templatePos = templates.indexOf(details.templateId)
            if (templatePos >= 0) templateSpinner.setSelection(templatePos)

            binding.create.setText(R.string.change)
            binding.id.setText(id)
            binding.name.setText(details.prettyName)
            binding.deleteButton.setOnClickListener {
                showMessageYesNo(getString(R.string.delete_library_confirmation, binding.name.text.toString())) {
                    Bridge.VLSetLibraryDetails(id, null)
                    withActivity<NewLibrary> {  finish() }
                }
            }
        } else {
            binding.id.setText( "user${(Math.random() * 1000).toInt()}")
            binding.deleteButton.visibility = View.GONE
        }
        binding.create.setOnClickListener {
            val oldId = if (mode == MODE_LIBRARY_MODIFY) intent.getStringExtra("libId") else null
            val newId = getNewId()
            val details = this.details ?: LibraryDetails()
            details.prettyName = binding.name.text.toString()
            details.id = newId
            details.templateId = binding.templateSpinner.selectedItem.toString()
            details.variableNames = arrayOfNulls(variables.size)
            details.variableValues = arrayOfNulls(variables.size)
            var i = 0
            for ((key, value) in variables) {
                details.variableNames[i] = key
                details.variableValues[i] = value.editText?.text?.toString() ?: value.defaultValue
                i += 1
            }
            Bridge.VLSetLibraryDetails(newId, details)
            if (newId != oldId && oldId != null) Bridge.VLSetLibraryDetails(oldId, null)
            finish()
        }
        //binding.name.requestFocus() //or use scrollView.requestChildFocus(target, target); ??
    }

    fun getNewId() =
        binding.id.text.toString().let { newId ->
            (newId.countOf('_')..3).map{"-_"}.joinToString("") + newId
        }

    fun addTemplateVariable(linearLayout: LinearLayout, inflater: LayoutInflater, name: String, desc: String?, defaultValue: String?): EditText {
        val value = defaultValue ?: ""
        val option = NewliboptionBinding.inflate(inflater, linearLayout, true)
        option.text.text = desc?.let { "$name ( $desc)" } ?: name
        option.edit.setText(value)
        variables[name] = LibraryVariable(value, option.edit)
        return option.edit
    }

    fun selectTemplate(template: String) {
        val details =
            try {
                Bridge.VLGetTemplateDetails(template) ?: return
            } catch (e: Bridge.InternalError) {
                showMessage(e.localizedMessage)
                return
            }

        val oldVariables = variables
        variables = HashMap()
        val oldValues = HashMap<String, String>()
        for ((key, value) in oldVariables)
            if (value.editText == null) oldValues[key] = value.defaultValue
            else if (value.editText.text.toString() != value.defaultValue) oldValues[key] = value.editText.text.toString()
        val linearLayout = binding.options
        linearLayout.removeAllViews()
        val inflater = layoutInflater
        for (i in details.variablesNames.indices) {
            val et = addTemplateVariable(linearLayout, inflater, details.variablesNames[i], details.variablesDescription[i], details.variablesDefault[i])
            if (details.variablesNames[i] in oldValues) et.setText(oldValues[details.variablesNames[i]])
        }
        for ((key, value) in oldValues) if (key !in variables) addTemplateVariable(linearLayout, inflater, key, null, "").setText(value)
    }

    companion object {
        const val MODE_LIBRARY_MODIFY = 1237
    }
}