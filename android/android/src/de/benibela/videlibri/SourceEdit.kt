package de.benibela.videlibri

import android.os.Bundle
import android.view.View
import android.widget.AdapterView
import android.widget.AdapterView.OnItemSelectedListener
import android.widget.EditText
import android.widget.Spinner
import android.widget.TextView
import de.benibela.videlibri.jni.Bridge
import java.io.*

class SourceEdit : VideLibriBaseActivity() {
    private val BASEDIR_TEMPLATES = "libraries/templates/"
    private val BASEDIR_LIBRARIES = "libraries/"

    private lateinit var libraryIds: List<String>
    private lateinit var templateIds: Array<String>
    private lateinit var selection1: List<String>
    private var selection2: Array<String> = arrayOf()

    private var baseDir = ""
    private var fileName: String = ""

    internal var restoredStateBundle: Bundle? = null

    private var fileNameShownAsUserdefined: Boolean = false

    private fun makeTemplateIds(){
        templateIds = Bridge.VLGetTemplates()
        selection1 = (0 until templateIds.size + 2).map { when (it) {
            0 -> getString(R.string.lay_source_edit_library_list)
            templateIds.size + 1 -> tr(R.string.source_edit_new_directory)
            else -> getString(R.string.lay_source_edit_template, templateIds[it - 1])
        } }
        findViewById<Spinner>(R.id.spinner).setItems(selection1.toTypedArray())
    }

    @JvmOverloads
    internal fun showSelection2(positionOfSelection1: Int, defaultSelectionText: String? = null) {
        when (positionOfSelection1) {
            0 -> {
                baseDir = BASEDIR_LIBRARIES
                selection2 = libraryIds.toTypedArray()
            }
            selection1.size - 1 -> {
                showDialog {
                    message( R.string.source_edit_new_dialog_filename)
                    onCancel = { findViewById<Spinner>(R.id.spinner).setSelection(0) }
                    editWithOkButton {text ->
                        val emptyDefaultSystem = """<?xml version="1.0" encoding="UTF-8"?>
<actions>
  <action id="update-all">
  </action>

  <action id="renew-list">
  </action>



  <action id="search">
  </action>

  <action id="search-next-page">
  </action>

</actions>
"""
                        writeToNewFile("$BASEDIR_TEMPLATES$text/template", emptyDefaultSystem)
                        makeTemplateIds()
                    }
                }
                return
            }
            else -> {
                selection2 = arrayOf()
                baseDir = BASEDIR_TEMPLATES + templateIds[positionOfSelection1 - 1]
                val files = assets.list(baseDir).toMutableList()
                try {
                    val userFiles = userFile(baseDir).takeIf { it.exists() }?.list()?.filter { !files.contains(it)  }
                    userFiles?.let { files.addAll(userFiles) }
                } catch (ignored: IOException) {
                }
                files.add(tr(R.string.source_edit_new_file))
                selection2 = files.toTypedArray()
                baseDir += "/"
            }
        }

        val fileSpinner = findViewById<Spinner>(R.id.spinnerfile)
        fileSpinner.setItems(selection2)
        if (positionOfSelection1 == restoredStateBundle?.getInt("base"))
            fileSpinner.setSelection(restoredStateBundle?.getInt("file") ?: 0)
        else if (positionOfSelection1 > 0 || defaultSelectionText != null)
            fileSpinner.setSelection(defaultSelectionText ?: "template", selection2)
    }

    internal fun showSelection2(defaultSelectionText: String) {
        showSelection2(findViewById<Spinner>(R.id.spinner).selectedItemPosition, defaultSelectionText)
    }

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setVideLibriView(R.layout.sourceedit)
        setTitle(R.string.lay_source_edit)
        restoredStateBundle = savedInstanceState


        libraryIds = Bridge.VLGetLibraryIds().map { "$it.xml" } + getString(R.string.source_edit_new_library)
        makeTemplateIds()

        findViewById<Spinner>(R.id.spinner).onItemSelectedListener = object : OnItemSelectedListener {
            override fun onNothingSelected(parent: AdapterView<*>?) {}

            override fun onItemSelected(parent: AdapterView<*>?, view: View?, position: Int, id: Long) =
                showSelection2(position)

        }

        findViewById<Spinner>(R.id.spinnerfile).onItemSelectedListener = object : OnItemSelectedListener {
            override fun onItemSelected(parent: AdapterView<*>, view: View, position: Int, id: Long) {
                restoredStateBundle?.let { oldState ->
                    if (position == oldState.getInt("file") && findViewById<Spinner>(R.id.spinner).selectedItemPosition == oldState.getInt("base")) {
                        restoreEditText(oldState)
                        restoredStateBundle = null
                        return
                    }
                }
                if (position == selection2.size - 1) {
                    showDialog {
                        message(R.string.source_edit_new_dialog_filename)
                         onCancel = { currentActivity<SourceEdit>()?.findViewById<Spinner>(R.id.spinnerfile)?.setSelection(0) }
                        editWithOkButton {text ->
                            withActivity<SourceEdit> {
                                if (findViewById<Spinner>(R.id.spinner).selectedItemPosition == 0) {
                                    //new library
                                    if (text.split("_".toRegex()).size != 4) {
                                        showMessage(R.string.source_edit_invalid_library_id)
                                        instance.onCancel?.invoke(this@editWithOkButton)
                                    } else {
                                        val libid = text.replace(".xml", "")
                                        Bridge.VLSetLibraryDetails(libid, Bridge.LibraryDetails().apply{
                                            prettyName = tr(R.string.source_edit_new_library_default_name)
                                            templateId = "sru"
                                        })
                                        showSelection2("$libid.xml")
                                    }
                                } else {
                                    //new file for template
                                    writeToNewFile("$baseDir/$text", "")
                                    showSelection2(text)
                                }
                            }
                        }
                    }
                    return
                }
                loadFile(baseDir + selection2[position])
            }

            override fun onNothingSelected(parent: AdapterView<*>) {}
        }

        findButtonById(R.id.save).setOnClickListener {
            try {
                writeToFile(fileName, findViewById<EditText>(R.id.edit).text.toString())
                val pos = findViewById<Spinner>(R.id.spinner).selectedItemPosition
                try {
                    if (pos == 0)
                        Bridge.VLReloadLibrary(selection2[findViewById<Spinner>(R.id.spinnerfile).selectedItemPosition].replace(".xml", ""))
                    else
                        Bridge.VLReloadTemplate(templateIds[pos - 1])
                } catch (e: Bridge.InternalError) {
                    showMessage(e.localizedMessage)
                }

            } catch (e: IOException) {
                showMessage(tr(R.string.source_edit_filewritefailed, e.localizedMessage))
            }
            showFileName(true)
        }

        findButtonById(R.id.reset).setOnClickListener {
            if (assets.exists(fileName)) {
                val f = userFile(fileName)
                if (f.exists())
                    if (f.delete())
                        showToast(R.string.source_edit_deleted)
            } else
                showToast(R.string.source_edit_nodelete)
            loadFile()
        }
    }

    private fun restoreEditText(bundle: Bundle) {
        findViewById<EditText>(R.id.edit).apply {
            setText(bundle.getString("content"))
            setSelection(bundle.getInt("contentSelectionStart"), bundle.getInt("contentSelectionEnd"))
        }
        fileName = bundle.getString("filename") ?: ""
        showFileName(bundle.getBoolean("userdefined"))
    }

    override fun onSaveInstanceState(outState: Bundle?) {
        super.onSaveInstanceState(outState)
        outState ?: return
        outState.putInt("base", findViewById<Spinner>(R.id.spinner).selectedItemPosition)
        outState.putInt("file", findViewById<Spinner>(R.id.spinnerfile).selectedItemPosition)
        outState.putString("filename", fileName)
        outState.putBoolean("userdefined", fileNameShownAsUserdefined)
        findViewById<EditText>(R.id.edit).apply {
            outState.putString("content", text.toString())
            outState.putInt("contentSelectionStart", selectionStart)
            outState.putInt("contentSelectionEnd", selectionEnd)
        }
    }

    override fun onRestoreInstanceState(savedInstanceState: Bundle) {
        super.onRestoreInstanceState(savedInstanceState)
        findViewById<Spinner>(R.id.spinner).setSelection(savedInstanceState.getInt("base", 0))
        findViewById<Spinner>(R.id.spinnerfile).setSelection(savedInstanceState.getInt("file", 0))
        restoreEditText(savedInstanceState)
    }

    private fun userFile(fn: String?): File {
        return File(VideLibriApp.userPath(this) + "/" + fn)
    }

    internal fun loadFile(fileName: String) {
        this.fileName = fileName
        loadFile()
    }

    private fun loadFile() {
        val f = userFile(fileName)
        try {
            if (f.exists()) {
                loadFile(FileInputStream(f), true)
            } else {
                loadFile(assets.open(fileName), false)
            }
        } catch (e: IOException) {
            findViewById<EditText>(R.id.edit).setText("Failed to load source code")
        }

    }

    @Throws(IOException::class)
    internal fun loadFile(stream: InputStream, userDefined: Boolean) {
        findViewById<EditText>(R.id.edit).setText(stream.readAllText())
        showFileName(userDefined)
    }

    private fun showFileName(userDefined: Boolean) {
        findViewById<TextView>(R.id.filename).text = tr(R.string.source_edit_filename, fileName) + if (userDefined) "\n" + tr(R.string.source_edit_userdefined) else ""
        fileNameShownAsUserdefined = userDefined
    }

    @Throws(IOException::class)
    internal fun writeToFile(fileName: String?, text: String) {
        val f = userFile(fileName)
        f.parentFile.mkdirs()
        FileWriter(f).use { it.write(text) }
        showToast(R.string.source_edit_saved)
    }

    @Throws(IOException::class)
    internal fun writeToNewFile(fileName: String, text: String) {
        if (userFile(fileName).exists())
            showMessage(R.string.source_edit_new_file_exists)
        else
            writeToFile(fileName, text)
    }
}
