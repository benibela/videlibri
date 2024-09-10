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
import de.benibela.videlibri.jni.LibraryDetails
import de.benibela.videlibri.utils.*

data class LibraryVariable(val defaultValue: String, val editText: EditText?)

class NewLibrary : VideLibriBaseActivity() {
    private lateinit var binding: NewlibBinding

    var details: LibraryDetails? = null
    private var trChooseATemplate: String? = null
    private var variables = HashMap<String, LibraryVariable>()
    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        binding = setVideLibriView(NewlibBinding::inflate)
        trChooseATemplate = getString(R.string.lay_newlib_list_choosesystem)
        val templates = Bridge.VLGetTemplates().toMutableList().run { add(0, trChooseATemplate); toTypedArray() }
        binding.templateSpinner.setItems(templates)
        binding.templateSpinner.onItemSelectedListener = object : OnItemSelectedListener {
            override fun onItemSelected(adapterView: AdapterView<*>?, view: View?, i: Int, l: Long) {
                templates.getOrNull(i)?.let(::selectTemplate)
            }
            override fun onNothingSelected(adapterView: AdapterView<*>?) {}
        }
        binding.checkBoxSetId.setOnCheckedChangeListener { _, checked -> binding.idLayout.isVisibleNotGone = checked }
        val mode = intent.getIntExtra("mode", 0)
        if (mode == MODE_LIBRARY_MODIFY) {
            val id = intent.getStringExtra("libId") ?: return
            details = Bridge.VLGetLibraryDetails(id)
            val details = details ?: return
            details.variables.forEach { variables[it.name] = LibraryVariable(it.value, null) }
            val templatePos = templates.indexOf(details.templateId)
            if (templatePos >= 0) binding.templateSpinner.setSelection(templatePos)

            binding.create.setText(R.string.change)
            viewId = id
            binding.name.setText(details.prettyName)
            binding.deleteButton.setOnClickListener {
                showMessageYesNo(getString(R.string.delete_library_confirmation, binding.name.text.toString())) {
                    Bridge.VLSetLibraryDetails(id, null)
                    withActivity<NewLibrary> {  finishWithResult() }
                }
            }
        } else {
            viewId = ""
            binding.deleteButton.visibility = View.GONE
        }
        binding.create.setOnClickListener {
            val templateId = binding.templateSpinner.selectedItem.toString()
            if (templateId == trChooseATemplate) showMessage(R.string.lay_newlib_error_choosesystem)
            else if (variables.isNotEmpty() && variables.all { it.value.editText?.text?.isBlank() ?: true }) showMessage(R.string.lay_newlib_error_no_parameters)
            else {
                val oldId = if (mode == MODE_LIBRARY_MODIFY) intent.getStringExtra("libId") else null
                val newId = viewId
                val details = this.details ?: LibraryDetails()
                details.prettyName = binding.name.text.toString()
                details.id = newId
                details.templateId = templateId
                details.variables = variables.map { (key, value) -> de.benibela.videlibri.jni.LibraryVariable(key, value.editText?.text?.toString() ?: value.defaultValue) }.toTypedArray()
                Bridge.VLSetLibraryDetails(newId, details)
                if (newId != oldId && oldId != null) Bridge.VLSetLibraryDetails(oldId, null)
                finishWithResult()
            }
        }
        //binding.name.requestFocus() //or use scrollView.requestChildFocus(target, target); ??
    }

    private var viewId
        get() = arrayOf(binding.idCountry, binding.idState, binding.idCity, binding.idId).joinToString("_") {
            it.text.toString().replace('_', '-').trim().takeNonEmpty() ?: "-"
        }
        set(value){
            val sid = value.split("_").toMutableList()
            if (sid.size > 4) sid[3] = sid.drop(3).joinToString("_")
            while (sid.size < 4) sid.add(0, "")
            if (sid[3].isEmpty()) {
                val existingUserLibraries = Bridge.VLGetOptions().userLibIds
                sid[3] = if (existingUserLibraries.isEmpty()) "user"
                else run {
                    val oldIds = existingUserLibraries.map { it.substringAfterLast("user").toIntOrNull() ?: 0 }
                    "user${(oldIds.maxOrNull() ?: existingUserLibraries.size) + 1}"
                }
            }
            arrayOf(binding.idCountry, binding.idState, binding.idCity, binding.idId).zip(sid).forEach { it.first.setText(it.second) }
        }

    override fun onRestoreInstanceState(savedInstanceState: Bundle) {
        super.onRestoreInstanceState(savedInstanceState)
        binding.idLayout.isVisibleNotGone = binding.checkBoxSetId.isChecked
    }

    private fun addTemplateVariable(linearLayout: LinearLayout, inflater: LayoutInflater, name: String, desc: String?, defaultValue: String?): EditText {
        val value = defaultValue ?: ""
        val option = NewliboptionBinding.inflate(inflater, linearLayout, true)
        option.text.text = desc?.let { "$name ( $desc)" } ?: name
        option.edit.setText(value)
        variables[name] = LibraryVariable(value, option.edit)
        return option.edit
    }

    fun selectTemplate(template: String) {
        if (template == trChooseATemplate) return
        val details =
            try {
                Bridge.VLGetTemplateDetails(template) ?: return
            } catch (e: de.benibela.videlibri.jni.InternalError) {
                showMessage(e.localizedMessage)
                return
            }

        binding.textViewTemplateDescription.text = details.description
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