package de.benibela.videlibri

import android.Manifest
import android.app.Activity
import android.content.Context
import android.content.Intent
import android.content.pm.PackageManager
import android.os.Build
import android.os.Bundle
import android.os.Environment
import android.preference.PreferenceManager
import androidx.annotation.StringRes
import androidx.core.app.ActivityCompat
import androidx.core.content.ContextCompat
import android.util.AttributeSet
import android.view.MotionEvent
import android.view.View
import android.view.inputmethod.InputMethodManager
import android.widget.*
import de.benibela.videlibri.jni.Bridge
import java.io.File
import java.util.*
import kotlin.math.abs

class ImportExport : VideLibriBaseActivity() {

    private lateinit var OPTIONS: Array<String>

    internal var mode: Int = 0
    private lateinit var flagAdapter: ArrayAdapter<String>
    internal var data: Bridge.ImportExportData? = null

    //https://stackoverflow.com/a/22751867
    class MaxHeightListView : ListView {
        constructor(context: Context) : super(context)
        constructor(context: Context, attrs: AttributeSet) : super(context, attrs)
        constructor(context: Context, attrs: AttributeSet, defStyle: Int) : super(context, attrs, defStyle)

        public override fun onMeasure(widthMeasureSpec: Int, heightMeasureSpec: Int) {
            super.onMeasure(widthMeasureSpec, MeasureSpec.makeMeasureSpec(Integer.MAX_VALUE shr 2, MeasureSpec.AT_MOST))
        }
    }

    class ScrollViewInterceptor(context: Context, attrs: AttributeSet) : ScrollView(context, attrs) {
        private var startY: Float = 0.toFloat()

        override fun onInterceptTouchEvent(e: MotionEvent): Boolean {
            onTouchEvent(e)
            if (e.action == MotionEvent.ACTION_DOWN) startY = e.y
            return e.action == MotionEvent.ACTION_MOVE && abs(startY - e.y) > 50
        }
    }

    private fun checkAllSet(lv: ListView, checked: Boolean) {
        lv.choiceMode = AbsListView.CHOICE_MODE_MULTIPLE
        val count = lv.count
        for (i in 0 until count)
            lv.setItemChecked(i, checked)
    }

    private fun checkAll(lv: ListView) {
        checkAllSet(lv, true)
    }

    private fun setButtonText() {
        findViewById<Button>(R.id.button).setText(when (mode) {
            MODE_IMPORT -> if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.JELLY_BEAN && ContextCompat.checkSelfPermission(this, Manifest.permission.READ_EXTERNAL_STORAGE) != PackageManager.PERMISSION_GRANTED)
                                R.string.import_export_need_permission
                           else
                                R.string.import_load
            else -> if (ContextCompat.checkSelfPermission(this, Manifest.permission.WRITE_EXTERNAL_STORAGE) != PackageManager.PERMISSION_GRANTED)
                        R.string.import_export_need_permission
                    else
                        R.string.export
        })
    }

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setVideLibriView(R.layout.importexport)
        mode = intent.getIntExtra("mode", MODE_IMPORT)
        OPTIONS = arrayOf(tr(R.string.lay_options_option_current), tr(R.string.history), tr(R.string.configuration), tr(R.string.passwords))

        var fileName = PreferenceManager.getDefaultSharedPreferences(this).getString("importExportFileName", "")
        if (fileName.isNullOrEmpty()) {
            val dir = Environment.getExternalStorageDirectory()
            fileName = File(dir, "videlibri.xml").absolutePath
        }
        findViewById<EditText>(R.id.edit).setText(fileName)


        if (mode == MODE_IMPORT) {
            forEachView<View>(R.id.textView, R.id.textView1, R.id.listView, R.id.listView1) { it.visibility = View.GONE }
            title = tr(R.string.import_)
            findViewById<TextView>(R.id.textView).setText(R.string.import_accounts)
            findViewById<TextView>(R.id.textView1).setText(R.string.import_properties)
            findViewById<TextView>(R.id.textView2).setText(R.string.import_file)
            if (Accounts.filterWithRunningUpdate().size > 0) {
                showFinalMessage(R.string.import_not_while_update_runs)
                return
            }
        } else {
            title = tr(R.string.export)
            findViewById<TextView>(R.id.textView).setText(R.string.export_accounts)
            findViewById<TextView>(R.id.textView1).setText(R.string.export_properties)
            findViewById<TextView>(R.id.textView2).setText(R.string.export_file)

            val accounts = Bridge.VLGetAccounts()
            val accountNames = accounts.map { it.prettyName }
            findViewById<ListView>(R.id.listView).let {
                it.adapter = ArrayAdapter(this, android.R.layout.simple_list_item_multiple_choice, accountNames)
                checkAll(it)
            }
        }
        setButtonText()

        flagAdapter = ArrayAdapter(this, android.R.layout.simple_list_item_multiple_choice, OPTIONS)
        findViewById<ListView>(R.id.listView1).let {
            it.adapter = flagAdapter
            checkAll(it)
            it.setItemChecked(flagAdapter.count - 1, false)
        }

        findViewById<Button>(R.id.button).setOnClickListener(View.OnClickListener { _ ->
            if (mode == MODE_IMPORT) {
                if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.JELLY_BEAN)
                    if (ContextCompat.checkSelfPermission(this@ImportExport, Manifest.permission.READ_EXTERNAL_STORAGE) != PackageManager.PERMISSION_GRANTED) {
                        ActivityCompat.requestPermissions(this@ImportExport, arrayOf(Manifest.permission.READ_EXTERNAL_STORAGE), 1)
                        return@OnClickListener
                    }
            } else {
                if (ContextCompat.checkSelfPermission(this@ImportExport, Manifest.permission.WRITE_EXTERNAL_STORAGE) != PackageManager.PERMISSION_GRANTED) {
                    ActivityCompat.requestPermissions(this@ImportExport, arrayOf(Manifest.permission.WRITE_EXTERNAL_STORAGE), 1)
                    return@OnClickListener
                }
            }


            val flagListView = findViewById<ListView>(R.id.listView1)
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


            val accountListView = findViewById<ListView>(R.id.listView)
            try {
                if (mode == MODE_IMPORT) {
                    if (data == null) {
                        showPreparedImport(findViewById<EditText>(R.id.edit).text.toString())
                    } else {
                        data?.let { data ->
                            val oldWithEmptyPass = accounts.count { it.name.isNotEmpty() && it.pass.isEmpty() }
                            data.accountsToImport = data.accountsToImport?.filterIndexed { i, _ -> accountListView.isItemChecked(i) }?.toTypedArray()
                            data.flags = flags
                            Bridge.VLImportAccounts(data)
                            Accounts.refreshAll()
                            val newWithEmptyPass = accounts.count { it.name.isNotEmpty() && it.pass.isEmpty() }
                            showFinalMessage(if (newWithEmptyPass > oldWithEmptyPass) R.string.import_done_has_empty_pass else R.string.import_done)
                        }
                        data = null
                    }
                } else {
                    val exports = Bridge.VLGetAccounts().filterIndexed { i, _ -> accountListView.isItemChecked(i)  }.toTypedArray()
                    Bridge.VLExportAccounts(findViewById<EditText>(R.id.edit).text.toString(), exports, flags)
                    rememberFileNameInPreferences()
                    showFinalMessage(R.string.export_done)
                }
            } catch (e: Bridge.InternalError) {
                showMessage(e.message)
            }
        })

        findViewById<Button>(R.id.buttonChoose).setOnClickListener {
            val intent = Intent()
            intent.action = Intent.ACTION_GET_CONTENT
            intent.type = "*/*"
            //intent.putExtra(Intent.CAT, true);
            intent.putExtra("android.intent.extra.LOCAL_ONLY", true)
            startActivityForResult(intent, 30017)
        }
    }

    private fun rememberFileNameInPreferences() {
        PreferenceManager.getDefaultSharedPreferences(this).edit().let {
            it.putString("importExportFileName", findViewById<EditText>(R.id.edit).text.toString())
            it.apply()
        }
    }

    private fun showPreparedImport(filename: String) {
        data = Bridge.VLImportAccountsPrepare(filename)
        val data = data ?: return
        rememberFileNameInPreferences()

        findViewById<ListView>(R.id.listView).let { lv ->
            lv.adapter = ArrayAdapter(this@ImportExport, android.R.layout.simple_list_item_multiple_choice, data.accountsToImport)
            checkAll(lv)
        }

        val newOptions = ArrayList<String>()
        for (i in OPTIONS.indices)
            if (data.flags and (1 shl i) != 0)
                newOptions.add(OPTIONS[i])
        flagAdapter = ArrayAdapter(this@ImportExport, android.R.layout.simple_list_item_multiple_choice, newOptions)
        findViewById<ListView>(R.id.listView1).let { lv ->
            lv.adapter = flagAdapter
            checkAll(lv)
        }

        findViewById<Button>(R.id.button).text = tr(R.string.import_)
        forEachView<View>(R.id.textView, R.id.textView1, R.id.listView, R.id.listView1) { it.visibility = View.VISIBLE }
        forEachView<View>(R.id.edit, R.id.buttonChoose, R.id.textView2) { it.visibility = View.GONE }
        hideKeyboard(this)
    }

    override fun onSaveInstanceState(outState: Bundle) {
        super.onSaveInstanceState(outState)
        if (mode == MODE_IMPORT && data != null) {
            outState.putString("activeImportFileName", findViewById<EditText>(R.id.edit).text.toString())
            outState.putSparseBooleanArray("listViewChecked", findViewById<ListView>(R.id.listView).checkedItemPositions)
            outState.putSparseBooleanArray("listView1Checked", findViewById<ListView>(R.id.listView1).checkedItemPositions)
        }
    }

    override fun onRestoreInstanceState(savedInstanceState: Bundle?) {
        super.onRestoreInstanceState(savedInstanceState)
        savedInstanceState ?: return
        val fn = savedInstanceState.getString("activeImportFileName")?.takeNonEmpty() ?: return
        try {
            showPreparedImport(fn)
            savedInstanceState.getSparseBooleanArray("listViewChecked")?.let { findViewById<ListView>(R.id.listView).setCheckedItemPositions(it) }
            savedInstanceState.getSparseBooleanArray("listView1Checked")?.let { findViewById<ListView>(R.id.listView1).setCheckedItemPositions(it) }
        } catch (e: Exception) { //this will happen when someone has removed the SD card or our permissions
            showToast(e.localizedMessage)
        }
    }

    override fun onDestroy() {
        data?.let {
            it.flags = 0
            Bridge.VLImportAccounts(it)
        }
        super.onDestroy()
    }

    override fun onActivityResult(requestCode: Int, resultCode: Int, data: Intent?) {
        if (requestCode == 30017 && resultCode != Activity.RESULT_CANCELED && data != null && data.data != null) {
            Util.UriToPath.getPath(this, data.data)?.let {
                findViewById<EditText>(R.id.edit).setText(it)
            }
            return
        }
        super.onActivityResult(requestCode, resultCode, data)
    }

    override fun onRequestPermissionsResult(requestCode: Int, permissions: Array<String>, grantResults: IntArray) {
        setButtonText()
        super.onRequestPermissionsResult(requestCode, permissions, grantResults)
    }

    private fun showFinalMessage(@StringRes message: Int){
        showDialog {
            message(message)
            onDismiss = {
                currentActivity<ImportExport>()?.finish()
            }
        }
    }

    companion object {
        const val MODE_IMPORT = 0
        const val MODE_EXPORT = 1


        //https://stackoverflow.com/questions/1109022/close-hide-the-android-soft-keyboard
        fun hideKeyboard(activity: Activity) {
            val imm = activity.getSystemService(Activity.INPUT_METHOD_SERVICE) as? InputMethodManager ?: return
            val view = activity.currentFocus ?: View(activity)
            imm.hideSoftInputFromWindow(view.windowToken, 0)
        }
    }
}
