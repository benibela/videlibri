package de.benibela.videlibri.activities

import android.Manifest
import android.annotation.SuppressLint
import android.app.Activity
import android.content.Context
import android.content.Intent
import android.net.Uri
import android.os.Build
import android.os.Bundle
import android.os.Environment
import android.os.Parcelable
import android.preference.PreferenceManager
import android.provider.DocumentsContract
import android.util.AttributeSet
import android.util.SparseBooleanArray
import android.view.MotionEvent
import android.view.View
import android.view.inputmethod.InputMethodManager
import android.widget.AbsListView
import android.widget.ArrayAdapter
import android.widget.ListView
import android.widget.ScrollView
import androidx.annotation.StringRes
import androidx.core.app.ActivityCompat
import de.benibela.videlibri.Accounts
import de.benibela.videlibri.R
import de.benibela.videlibri.accounts
import de.benibela.videlibri.databinding.ImportexportBinding
import de.benibela.videlibri.jni.Bridge
import de.benibela.videlibri.utils.*
import kotlinx.android.parcel.Parcelize
import java.io.File
import java.util.*
import kotlin.math.abs

//https://stackoverflow.com/a/22751867
class MaxHeightListView : ListView {
    constructor(context: Context) : super(context)
    constructor(context: Context, attrs: AttributeSet) : super(context, attrs)
    constructor(context: Context, attrs: AttributeSet, defStyle: Int) : super(context, attrs, defStyle)

    public override fun onMeasure(widthMeasureSpec: Int, heightMeasureSpec: Int) {
        super.onMeasure(widthMeasureSpec, MeasureSpec.makeMeasureSpec(Integer.MAX_VALUE shr 2, MeasureSpec.AT_MOST))
    }

    fun checkAllSet(@Suppress("SameParameterValue") checked: Boolean) {
        choiceMode = AbsListView.CHOICE_MODE_MULTIPLE
        val count = count
        for (i in 0 until count)
            setItemChecked(i, checked)
    }

    fun checkAll() {
        checkAllSet(true)
    }
}



@SuppressLint("Registered")
open class ImportExportBase : VideLibriBaseActivity() {

    lateinit var OPTIONS: Array<String>

    protected lateinit var flagAdapter: ArrayAdapter<String>
    internal var data: Bridge.ImportExportData? = null


    protected lateinit var binding: ImportexportBinding

    fun hasPermissionReadExternalFiles() = true
    fun hasPermissionWriteExternalFiles() = Build.VERSION.SDK_INT >= Build.VERSION_CODES.KITKAT


    @Suppress("unused") //used in layout
    class ScrollViewInterceptor(context: Context, attrs: AttributeSet) : ScrollView(context, attrs) {
        private var startY: Float = 0.toFloat()

        override fun onInterceptTouchEvent(e: MotionEvent): Boolean {
            onTouchEvent(e)
            if (e.action == MotionEvent.ACTION_DOWN) startY = e.y
            return e.action == MotionEvent.ACTION_MOVE && abs(startY - e.y) > 50
        }
    }


    protected fun getSelectedImportExportFlags(): Int = run {
        val flagListView = binding.listView1
        var flags = 0
        var optionI = 0
        for (i in 0 until flagAdapter.count)
            if (flagListView.isItemChecked(i)) {
                while (OPTIONS[optionI] != flagAdapter.getItem(i)) {
                    optionI++
                }
                //assert i == optionI ???
                flags = flags or (1 shl optionI)
            }
        if (flags and Bridge.ImportExportData.PASSWORD != 0)
            flags = flags or Bridge.ImportExportData.CONFIG
        flags
    }

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        binding = setVideLibriView(ImportexportBinding::inflate)
        OPTIONS = arrayOf(getString(R.string.lay_options_option_current), getString(R.string.history), getString(R.string.configuration), getString(R.string.passwords))



        flagAdapter = ArrayAdapter(this, android.R.layout.simple_list_item_multiple_choice, OPTIONS)
        binding.listView1.let {
            it.adapter = flagAdapter
            it.checkAll()
            it.setItemChecked(flagAdapter.count - 1, false)
        }
    }

    protected fun startChooseFileNameActivity(forWriting: Boolean){
        val intent = Intent().apply {
            if (Build.VERSION.SDK_INT < Build.VERSION_CODES.KITKAT) {
                action = Intent.ACTION_GET_CONTENT
                putExtra(Intent.EXTRA_LOCAL_ONLY, true)
            } else {
                if (forWriting) {
                    action = Intent.ACTION_CREATE_DOCUMENT
                    flags = Intent.FLAG_GRANT_WRITE_URI_PERMISSION
                } else {
                    action = Intent.ACTION_OPEN_DOCUMENT
                    flags = Intent.FLAG_GRANT_READ_URI_PERMISSION
                }
                if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
                    val initialUri = PreferenceManager.getDefaultSharedPreferences(this@ImportExportBase).
                                            getString("importExportFileName", "")?.
                                            takeNonEmpty() ?:
                                            File(getExternalFilesDir(Environment.DIRECTORY_DOCUMENTS), "videlibri.xml").absolutePath
                    putExtra(DocumentsContract.EXTRA_INITIAL_URI, initialUri)
                }
                putExtra(Intent.EXTRA_MIME_TYPES, arrayOf("text/xml", "application/xml", "*/*"));
            }
            addCategory(Intent.CATEGORY_OPENABLE)
            type = "*/*"
        }
        startActivityForResult(intent, FILENAME_ACTIVITY_REQUEST_CODE)
    }

    open fun onFileNameChosen(uri: Uri){
        PreferenceManager.getDefaultSharedPreferences(this).edit().let {
            it.putString("importExportFileName", uri.toString())
            it.apply()
        }
    }

    override fun onActivityResult(requestCode: Int, resultCode: Int, data: Intent?) {
        val uri = data?.data
        if (requestCode == FILENAME_ACTIVITY_REQUEST_CODE && resultCode != Activity.RESULT_CANCELED && uri != null) {
            onFileNameChosen(uri)
            return
        }
        super.onActivityResult(requestCode, resultCode, data)
    }


    protected open fun showFinalMessage(@StringRes message: Int){
        showDialog {
            message(message)
            onDismiss = {
                currentActivity<ImportExportBase>()?.finish()
            }
        }
    }

    companion object {
        const val FILENAME_ACTIVITY_REQUEST_CODE = 30017
        //https://stackoverflow.com/questions/1109022/close-hide-the-android-soft-keyboard
        fun hideKeyboard(activity: Activity) {
            val imm = activity.getSystemService(Activity.INPUT_METHOD_SERVICE) as? InputMethodManager ?: return
            val view = activity.currentFocus ?: View(activity)
            imm.hideSoftInputFromWindow(view.windowToken, 0)
        }
    }
}












class Import : ImportExportBase() {
    enum class ImportPhase {Init, RequestedPermission, OpenedFileChooser, SelectedFile, Done}
    @Parcelize
    class State(
            var phase: ImportPhase = ImportPhase.Init,
            var fileName: String = "",
            var fileIsTemporary: Boolean = false,

            var listViewCheckedItemPositions: SparseBooleanArray? = null,
            var listView1CheckedItemPositions: SparseBooleanArray? = null
    ): Parcelable

    var state = State()

    override fun showFinalMessage(message: Int) {
        super.showFinalMessage(message)
        state.phase = ImportPhase.Done
    }

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)

        registerState(::state)

        listOf(binding.textView, binding.textView1, binding.listView, binding.listView1).forEach{ it.visibility = View.GONE }
        title = getString(R.string.import_)
        binding.button.text = getString(R.string.import_)
        binding.textView.setText(R.string.import_accounts)
        binding.textView1.setText(R.string.import_properties)

        binding.button.setOnClickListener(::importNow)
    }


    override fun onPostResume() {
        super.onPostResume()
        if (Accounts.filterWithRunningUpdate().size > 0) {
            showFinalMessage(R.string.import_not_while_update_runs)
            return
        }
        when (state.phase) {
            ImportPhase.Init ->
                if (!hasPermissionReadExternalFiles()) {
                    state.phase = ImportPhase.RequestedPermission
                    ActivityCompat.requestPermissions(this@Import, arrayOf(Manifest.permission.READ_EXTERNAL_STORAGE), 1)
                } else startChooseImportFileActivity()
            ImportPhase.RequestedPermission -> {} //for these two we wait for the on... event.  Makes the phase enum rather pointless
            ImportPhase.OpenedFileChooser -> {}
            ImportPhase.SelectedFile -> showPreparedImport(state.fileName)
            ImportPhase.Done -> {}
        }
    }

    override fun onActivityResult(requestCode: Int, resultCode: Int, data: Intent?) {
        super.onActivityResult(requestCode, resultCode, data)
        if (requestCode == FILENAME_ACTIVITY_REQUEST_CODE && resultCode == Activity.RESULT_CANCELED)
            finish()
    }

    override fun onRequestPermissionsResult(requestCode: Int, permissions: Array<out String>, grantResults: IntArray) {
        super.onRequestPermissionsResult(requestCode, permissions, grantResults)
        if (!hasPermissionReadExternalFiles())
            showFinalMessage(R.string.needReadExternalStoragePermissionForImport)
        else
            startChooseImportFileActivity()

    }

    fun startChooseImportFileActivity(){
        startChooseFileNameActivity(forWriting = false)
        state.phase = ImportPhase.OpenedFileChooser
    }

    override fun onFileNameChosen(uri: Uri) {
        super.onFileNameChosen(uri)
        try {
            contentResolver.openInputStream(uri)?.use { source ->
                val tempFile = File(cacheDir, "tmpImport.xml")
                tempFile.outputStream().use { sink ->
                    source.copyTo(sink)
                }
                state.fileName = tempFile.absolutePath
                state.fileIsTemporary = true
            }
        } catch (e: java.lang.Exception) {
            showMessage(getString(R.string.import_file_open_failed) + ":\n" + e.message)
        }
        state.phase = ImportPhase.SelectedFile
    }


    private fun showPreparedImport(filename: String) {
        if (data == null)
            try {
                data = Bridge.VLImportAccountsPrepare(filename)
            } catch (e: Bridge.InternalError){
                showMessage(getString(R.string.import_file_open_failed) + ":\n" + e.message)
                data = null
            }
        val data = data ?: return

        binding.listView.let { lv ->
            val accounts: Array<String> = data.accountsToImport!!
            lv.adapter = ArrayAdapter(this@Import, android.R.layout.simple_list_item_multiple_choice, accounts)
            lv.checkAll()
        }

        val newOptions = ArrayList<String>()
        for (i in OPTIONS.indices)
            if (data.flags and (1 shl i) != 0)
                newOptions.add(OPTIONS[i])
        flagAdapter = ArrayAdapter(this@Import, android.R.layout.simple_list_item_multiple_choice, newOptions)
        binding.listView1.let { lv ->
            lv.adapter = flagAdapter
            lv.checkAll()
        }

        listOf(binding.textView, binding.textView1, binding.listView, binding.listView1).forEach { it.visibility = View.VISIBLE }
        hideKeyboard(this)

        state.listViewCheckedItemPositions?.let { binding.listView.setCheckedItemPositions(it) }
        state.listView1CheckedItemPositions?.let { binding.listView1.setCheckedItemPositions(it) }

    }

    private fun importNow(@Suppress("UNUSED_PARAMETER") v: View){
        val accountListView = binding.listView
        val data = data ?: return
        try {
            val oldWithEmptyPass = accounts.count { it.name.isNotEmpty() && it.pass.isEmpty() }
            data.accountsToImport = data.accountsToImport?.filterIndexed { i, _ -> accountListView.isItemChecked(i) }?.toTypedArray()
            data.flags = getSelectedImportExportFlags()
            Bridge.VLImportAccounts(data)
            Accounts.refreshAll()
            val newWithEmptyPass = accounts.count { it.name.isNotEmpty() && it.pass.isEmpty() }
            showFinalMessage(if (newWithEmptyPass > oldWithEmptyPass) R.string.import_done_has_empty_pass else R.string.import_done)
            if (state.fileIsTemporary && state.fileName.isNotEmpty())
                File(state.fileName).delete()
            state.fileName = ""
            this.data = null
        } catch (e: Bridge.InternalError) {
            showMessage(e.message)
        }
    }

    override fun onSaveInstanceState(outState: Bundle) {
        state.listViewCheckedItemPositions = binding.listView.checkedItemPositions
        state.listView1CheckedItemPositions = binding.listView1.checkedItemPositions
        super.onSaveInstanceState(outState)
    }

    override fun onDestroy() {
        data?.let {
            it.flags = 0
            Bridge.VLImportAccounts(it)
        }
        super.onDestroy()
    }
}











class Export : ImportExportBase() {
    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)


        title = getString(R.string.export)
        binding.textView.setText(R.string.export_accounts)
        binding.textView1.setText(R.string.export_properties)

        val accounts = Bridge.VLGetAccounts()
        val accountNames = accounts.map { it.prettyName }
        binding.listView.let {
            it.adapter = ArrayAdapter(this, android.R.layout.simple_list_item_multiple_choice, accountNames)
            it.checkAll()
        }
        setButtonText()

        flagAdapter = ArrayAdapter(this, android.R.layout.simple_list_item_multiple_choice, OPTIONS)
        binding.listView1.let {
            it.adapter = flagAdapter
            it.checkAll()
            it.setItemChecked(flagAdapter.count - 1, false)
        }

        binding.button.setOnClickListener(View.OnClickListener { _ ->
            if (!hasPermissionWriteExternalFiles()) {
                ActivityCompat.requestPermissions(this@Export, arrayOf(Manifest.permission.WRITE_EXTERNAL_STORAGE), 1)
                return@OnClickListener
            }

            startChooseFileNameActivity(forWriting = true)

        })
    }

    override fun onFileNameChosen(uri: Uri) {
        super.onFileNameChosen(uri)
        val tempFile = File(cacheDir, "tmpImport.xml")
        val tempFileName = tempFile.absolutePath

        val accountListView = binding.listView
        try {
            val flags = getSelectedImportExportFlags()
            val exports = Bridge.VLGetAccounts().filterIndexed { i, _ -> accountListView.isItemChecked(i)  }.toTypedArray()
            Bridge.VLExportAccounts(tempFileName, exports, flags)


            contentResolver.openOutputStream(uri)?.use { sink ->
                tempFile.inputStream().use { source ->
                    source.copyTo(sink)
                }
            }

            tempFile.delete()
            showFinalMessage(R.string.export_done)



        } catch (e: java.lang.Exception) {
            showMessage(getString(R.string.import_file_open_failed)+":\n"+ e.message)
        }
    }


    fun setButtonText() {
        binding.button.setText(if (!hasPermissionWriteExternalFiles())
                R.string.export_need_permission
            else
                R.string.export
        )
    }

    override fun onRequestPermissionsResult(requestCode: Int, permissions: Array<String>, grantResults: IntArray) {
        super.onRequestPermissionsResult(requestCode, permissions, grantResults)
        setButtonText()
    }

}