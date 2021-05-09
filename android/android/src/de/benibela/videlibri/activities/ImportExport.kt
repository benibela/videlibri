package de.benibela.videlibri.activities

import android.Manifest
import android.annotation.SuppressLint
import android.app.Activity
import android.content.Context
import android.content.Intent
import android.content.pm.PackageManager
import android.os.Build
import android.os.Bundle
import android.os.Environment
import android.preference.PreferenceManager
import android.util.AttributeSet
import android.view.MotionEvent
import android.view.View
import android.view.inputmethod.InputMethodManager
import android.widget.AbsListView
import android.widget.ArrayAdapter
import android.widget.ListView
import android.widget.ScrollView
import androidx.annotation.StringRes
import androidx.core.app.ActivityCompat
import androidx.core.content.ContextCompat
import de.benibela.videlibri.Accounts
import de.benibela.videlibri.R
import de.benibela.videlibri.accounts
import de.benibela.videlibri.databinding.ImportexportBinding
import de.benibela.videlibri.jni.Bridge
import de.benibela.videlibri.utils.*
import de.benibela.videlibri.utils.Util.UriToPath.getPath
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
}


@SuppressLint("Registered")
open class ImportExportBase : VideLibriBaseActivity() {

    lateinit var OPTIONS: Array<String>

    protected lateinit var flagAdapter: ArrayAdapter<String>
    internal var data: Bridge.ImportExportData? = null


    protected lateinit var binding: ImportexportBinding


    @Suppress("unused") //used in layout
    class ScrollViewInterceptor(context: Context, attrs: AttributeSet) : ScrollView(context, attrs) {
        private var startY: Float = 0.toFloat()

        override fun onInterceptTouchEvent(e: MotionEvent): Boolean {
            onTouchEvent(e)
            if (e.action == MotionEvent.ACTION_DOWN) startY = e.y
            return e.action == MotionEvent.ACTION_MOVE && abs(startY - e.y) > 50
        }
    }

    protected  fun checkAllSet(lv: ListView, @Suppress("SameParameterValue") checked: Boolean) {
        lv.choiceMode = AbsListView.CHOICE_MODE_MULTIPLE
        val count = lv.count
        for (i in 0 until count)
            lv.setItemChecked(i, checked)
    }

    protected fun checkAll(lv: ListView) {
        checkAllSet(lv, true)
    }

    protected open fun setButtonText() {
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
        OPTIONS = arrayOf(tr(R.string.lay_options_option_current), tr(R.string.history), tr(R.string.configuration), tr(R.string.passwords))

        var fileName = PreferenceManager.getDefaultSharedPreferences(this).getString("importExportFileName", "")
        if (fileName.isNullOrEmpty()) {
            val dir = Environment.getExternalStorageDirectory()
            fileName = File(dir, "videlibri.xml").absolutePath
        }
        binding.edit.setText(fileName)

        flagAdapter = ArrayAdapter(this, android.R.layout.simple_list_item_multiple_choice, OPTIONS)
        binding.listView1.let {
            it.adapter = flagAdapter
            checkAll(it)
            it.setItemChecked(flagAdapter.count - 1, false)
        }

        binding.buttonChoose.setOnClickListener {
            val intent = Intent()
            intent.action = Intent.ACTION_GET_CONTENT
            intent.type = "*/*"
            //intent.putExtra(Intent.CAT, true);
            intent.putExtra("android.intent.extra.LOCAL_ONLY", true)
            startActivityForResult(intent, 30017)
        }
    }

    protected fun rememberFileNameInPreferences() {
        PreferenceManager.getDefaultSharedPreferences(this).edit().let {
            it.putString("importExportFileName", binding.edit.text.toString())
            it.apply()
        }
    }



    override fun onActivityResult(requestCode: Int, resultCode: Int, data: Intent?) {
        if (requestCode == 30017 && resultCode != Activity.RESULT_CANCELED && data != null && data.data != null) {
            getPath(this, data.data)?.let {
                binding.edit.setText(it)
            }
            return
        }
        super.onActivityResult(requestCode, resultCode, data)
    }

    override fun onRequestPermissionsResult(requestCode: Int, permissions: Array<String>, grantResults: IntArray) {
        setButtonText()
        super.onRequestPermissionsResult(requestCode, permissions, grantResults)
    }

    protected fun showFinalMessage(@StringRes message: Int){
        showDialog {
            message(message)
            onDismiss = {
                currentActivity<ImportExportBase>()?.finish()
            }
        }
    }

    companion object {
        //https://stackoverflow.com/questions/1109022/close-hide-the-android-soft-keyboard
        fun hideKeyboard(activity: Activity) {
            val imm = activity.getSystemService(Activity.INPUT_METHOD_SERVICE) as? InputMethodManager ?: return
            val view = activity.currentFocus ?: View(activity)
            imm.hideSoftInputFromWindow(view.windowToken, 0)
        }
    }
}


class Import : ImportExportBase() {
    override fun setButtonText() {
        binding.button.setText(
                if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.JELLY_BEAN && ContextCompat.checkSelfPermission(this, Manifest.permission.READ_EXTERNAL_STORAGE) != PackageManager.PERMISSION_GRANTED)
                    R.string.import_export_need_permission
                else
                    R.string.import_load
        )
    }

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)

        listOf(binding.textView, binding.textView1, binding.listView, binding.listView1).forEach{ it.visibility = View.GONE }
        title = tr(R.string.import_)
        binding.textView.setText(R.string.import_accounts)
        binding.textView1.setText(R.string.import_properties)
        binding.textView2.setText(R.string.import_file)
        if (Accounts.filterWithRunningUpdate().size > 0) {
            showFinalMessage(R.string.import_not_while_update_runs)
            return
        }
        setButtonText()

        binding.button.setOnClickListener(View.OnClickListener { _ ->
            if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.JELLY_BEAN)
                if (ContextCompat.checkSelfPermission(this@Import, Manifest.permission.READ_EXTERNAL_STORAGE) != PackageManager.PERMISSION_GRANTED) {
                    ActivityCompat.requestPermissions(this@Import, arrayOf(Manifest.permission.READ_EXTERNAL_STORAGE), 1)
                    return@OnClickListener
                }

            val accountListView = binding.listView
            try {
                if (data == null) {
                    showPreparedImport(binding.edit.text.toString())
                } else {
                    data?.let { data ->
                        val oldWithEmptyPass = accounts.count { it.name.isNotEmpty() && it.pass.isEmpty() }
                        data.accountsToImport = data.accountsToImport?.filterIndexed { i, _ -> accountListView.isItemChecked(i) }?.toTypedArray()
                        data.flags = getSelectedImportExportFlags()
                        Bridge.VLImportAccounts(data)
                        Accounts.refreshAll()
                        val newWithEmptyPass = accounts.count { it.name.isNotEmpty() && it.pass.isEmpty() }
                        showFinalMessage(if (newWithEmptyPass > oldWithEmptyPass) R.string.import_done_has_empty_pass else R.string.import_done)
                    }
                    data = null
                }
            } catch (e: Bridge.InternalError) {
                showMessage(e.message)
            }
        })

    }

    private fun showPreparedImport(filename: String) {
        data = Bridge.VLImportAccountsPrepare(filename)
        val data = data ?: return
        rememberFileNameInPreferences()

        binding.listView.let { lv ->
            val accounts: Array<String> = data.accountsToImport!!
            lv.adapter = ArrayAdapter(this@Import, android.R.layout.simple_list_item_multiple_choice, accounts)
            checkAll(lv)
        }

        val newOptions = ArrayList<String>()
        for (i in OPTIONS.indices)
            if (data.flags and (1 shl i) != 0)
                newOptions.add(OPTIONS[i])
        flagAdapter = ArrayAdapter(this@Import, android.R.layout.simple_list_item_multiple_choice, newOptions)
        binding.listView1.let { lv ->
            lv.adapter = flagAdapter
            checkAll(lv)
        }

        binding.button.text = tr(R.string.import_)
        listOf(binding.textView, binding.textView1, binding.listView, binding.listView1).forEach { it.visibility = View.VISIBLE }
        listOf(binding.edit, binding.buttonChoose, binding.textView2).forEach{ it.visibility = View.GONE }
        hideKeyboard(this)
    }

    override fun onSaveInstanceState(outState: Bundle) {
        super.onSaveInstanceState(outState)
        outState.putString("activeImportFileName", binding.edit.text.toString())
        outState.putSparseBooleanArray("listViewChecked", binding.listView.checkedItemPositions)
        outState.putSparseBooleanArray("listView1Checked", binding.listView1.checkedItemPositions)
    }

    override fun onRestoreInstanceState(savedInstanceState: Bundle?) {
        super.onRestoreInstanceState(savedInstanceState)
        savedInstanceState ?: return
        val fn = savedInstanceState.getString("activeImportFileName")?.takeNonEmpty() ?: return
        try {
            showPreparedImport(fn)
            savedInstanceState.getSparseBooleanArray("listViewChecked")?.let { binding.listView.setCheckedItemPositions(it) }
            savedInstanceState.getSparseBooleanArray("listView1Checked")?.let { binding.listView1.setCheckedItemPositions(it) }
        } catch (e: Exception) { //this will happen when someone has removed the SD card or our permissions
            showToast(e.localizedMessage?:"")
        }
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


        title = tr(R.string.export)
        binding.textView.setText(R.string.export_accounts)
        binding.textView1.setText(R.string.export_properties)
        binding.textView2.setText(R.string.export_file)

        val accounts = Bridge.VLGetAccounts()
        val accountNames = accounts.map { it.prettyName }
        binding.listView.let {
            it.adapter = ArrayAdapter(this, android.R.layout.simple_list_item_multiple_choice, accountNames)
            checkAll(it)
        }
        setButtonText()

        flagAdapter = ArrayAdapter(this, android.R.layout.simple_list_item_multiple_choice, OPTIONS)
        binding.listView1.let {
            it.adapter = flagAdapter
            checkAll(it)
            it.setItemChecked(flagAdapter.count - 1, false)
        }

        binding.button.setOnClickListener(View.OnClickListener { _ ->
            if (ContextCompat.checkSelfPermission(this@Export, Manifest.permission.WRITE_EXTERNAL_STORAGE) != PackageManager.PERMISSION_GRANTED) {
                ActivityCompat.requestPermissions(this@Export, arrayOf(Manifest.permission.WRITE_EXTERNAL_STORAGE), 1)
                return@OnClickListener
            }

            val accountListView = binding.listView
            try {
                val flags = getSelectedImportExportFlags()
                val exports = Bridge.VLGetAccounts().filterIndexed { i, _ -> accountListView.isItemChecked(i)  }.toTypedArray()
                Bridge.VLExportAccounts(binding.edit.text.toString(), exports, flags)
                rememberFileNameInPreferences()
                showFinalMessage(R.string.export_done)
            } catch (e: Bridge.InternalError) {
                showMessage(e.message)
            }
        })
    }

    override fun setButtonText() {
        binding.button.setText(if (ContextCompat.checkSelfPermission(this, Manifest.permission.WRITE_EXTERNAL_STORAGE) != PackageManager.PERMISSION_GRANTED)
                R.string.import_export_need_permission
            else
                R.string.export
        )
    }

}