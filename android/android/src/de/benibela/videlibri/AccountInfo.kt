package de.benibela.videlibri

import android.annotation.SuppressLint
import android.app.Activity
import android.content.Intent
import android.graphics.Paint
import android.os.Bundle
import android.text.Editable
import android.text.TextWatcher
import android.view.View
import android.widget.*
import de.benibela.videlibri.jni.Bridge

internal open class EmptyTextWatcher : TextWatcher {
    override fun beforeTextChanged(charSequence: CharSequence, i: Int, i2: Int, i3: Int) {}

    override fun onTextChanged(charSequence: CharSequence, i: Int, i2: Int, i3: Int) {}

    override fun afterTextChanged(editable: Editable) {}
}

class AccountInfo : VideLibriBaseActivity() {

    private var mode: Int = 0
    private var libdetails: Bridge.LibraryDetails? = null
    private var libshortname: String = ""

    private lateinit var lib: TextView
    private lateinit var accountId: EditText
    private lateinit var accountPassword: EditText
    private lateinit var accountPrettyName: EditText

    private val oldAccount: Bridge.Account
        get() = intent.getSerializableExtra("account") as? Bridge.Account ?: accounts.toArray[0] ?: Bridge.Account()
    private val accountAutoExtend: Boolean
        get() = findViewById<CheckBox>(R.id.autoExtendButton).isChecked
    private val accountAutoExtendDays: Int
        get() = findViewById<EditText>(R.id.autoExtendDaysEdit).text.toString().toIntOrNull() ?: 7

    private fun setActiveLibrary(libid: String?): Bridge.LibraryDetails? = libid?.let { id ->
        Bridge.VLGetLibraryDetails(id)?.also {
            libshortname = it.prettyNameShort
            lib.text = it.prettyName
            findViewById<View>(R.id.typeLayout).visibility = if (it.segregatedAccounts) View.VISIBLE else View.GONE
            libdetails = it
        }
    }

    public override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setVideLibriView(R.layout.accountinfo)

        lib = findViewById(R.id.libraryTextView)
        accountId = findViewById(R.id.accountId)
        accountPassword = findViewById(R.id.accountPassword)
        accountPrettyName = findViewById(R.id.accountPrettyName)

        mode = intent.getIntExtra("mode", MODE_ACCOUNT_CREATION)
        setActiveLibrary(savedInstanceState?.getString("libId") ?: intent.getStringExtra("libId") ?: "") ?:
            setActiveLibrary(LibraryList.lastSelectedFallbackLibraryId())


        findViewById<CheckBox>(R.id.autoExtendButton).setOnCheckedChangeListener { _, b ->
            findViewById<View>(R.id.autoExtendDaysEdit).isEnabled = b
        }


        if (mode == MODE_ACCOUNT_MODIFY) {
            val oldAccount = oldAccount
            setActiveLibrary(oldAccount.libId)

            accountId.setText(oldAccount.name)
            accountPassword.setText(oldAccount.pass)
            accountPrettyName.setText(oldAccount.prettyName)
            findViewById<CheckBox>(R.id.autoExtendButton).isChecked = oldAccount.extend
            findViewById<EditText>(R.id.autoExtendDaysEdit).setText(oldAccount.extendDays.toString())
            findViewById<CheckBox>(R.id.saveHistoryButton).isChecked = oldAccount.history
            if (libdetails?.segregatedAccounts == true)
                findViewById<RadioButton>(if (oldAccount.type == 2) R.id.radioButtonExtern else R.id.radioButtonIntern).isChecked = true

            findViewById<View>(R.id.deleteButton).setOnClickListener {
                showMessageYesNo(R.string.account_delete) {
                    withActivity<AccountInfo> {deleteAccountNow()}
                }
            }
            findButtonById(R.id.completeAccountButton).text = tr(R.string.change)
            findViewById<View>(R.id.completeAccountButton).setOnClickListener {changeAccountNow()}
        } else {
            lib.paintFlags = lib.paintFlags or Paint.UNDERLINE_TEXT_FLAG
            lib.setOnClickListener { updateLibrary() }

            findViewById<View>(R.id.deleteButton).visibility = View.GONE
            findViewById<View>(R.id.completeAccountButton).setOnClickListener(View.OnClickListener {_ ->
                if (libdetails == null) {
                    showMessage(R.string.error_nolibselected)
                    return@OnClickListener
                }

                if (accountId.text.isEmpty()) {
                    addAccountNow()
                    return@OnClickListener
                }

                showDialog {
                    message(if (accountAutoExtend)
                        resources.getQuantityString(R.plurals.warning_autorenewal_onD, accountAutoExtendDays, accountAutoExtendDays)
                    else
                        tr(R.string.warning_autorenewal_off))
                    negativeButton(R.string.cancel)
                    positiveButton(R.string.ok) {
                        withActivity<AccountInfo> {
                            libdetails?.let {
                                if (it.prettyName.contains("(nur Suche getestet)"))
                                    showDialog {
                                        message(R.string.warning_alphalib)
                                        onDismiss = { withActivity<AccountInfo> { addAccountNow() } }
                                    }
                                else addAccountNow()
                            }
                        }
                    }
                }
            })
            accountPrettyName.setText(libshortname)
        }

        if (mode != MODE_ACCOUNT_MODIFY || accountId.text.toString() + " " + libshortname == accountPrettyName.text.toString())
            accountId.addTextChangedListener(object : EmptyTextWatcher() {
                @SuppressLint("SetTextI18n")
                override fun afterTextChanged(editable: Editable) {
                    accountPrettyName.setText(accountId.text.toString() + " " + libshortname)
                }
            })

    }


    override fun onResume() {
        super.onResume()

        libdetails ?: setActiveLibrary(LibraryList.lastSelectedFallbackLibraryId()) ?: updateLibrary()
    }

    override fun onSaveInstanceState(outState: Bundle?) {
        super.onSaveInstanceState(outState)
        outState?.putString("libId", libdetails?.id)
    }

    private fun updateLibrary() {
        findViewById<View>(R.id.libraryTextView).postDelayed({
            startActivityForResult<LibraryList>(
                    REQUEST_LIBRARY_FOR_ACCOUNT_CREATION,
                    "reason" to getString( if (mode == MODE_ACCOUNT_CREATION_INITIAL) R.string.account_createinitial else R.string.account_create )
            )
        }, 300)
    }

    private fun inputToAccount(): Bridge.Account? {
        val acc = Bridge.Account()
        acc.libId = libdetails?.id ?: return null
        acc.name = accountId.text.toString()
        acc.pass = accountPassword.text.toString()
        acc.prettyName = accountPrettyName.text.toString()
        acc.extend = accountAutoExtend
        acc.extendDays = accountAutoExtendDays
        acc.history = findViewById<CompoundButton>(R.id.saveHistoryButton).isChecked
        acc.type = if (findViewById<CompoundButton>(R.id.radioButtonExtern).isChecked) 2 else 1
        return acc
    }


    override fun onActivityResult(requestCode: Int, resultCode: Int, data: Intent?) {
        if (requestCode == REQUEST_LIBRARY_FOR_ACCOUNT_CREATION) {
            if (resultCode == Activity.RESULT_OK) {
                setActiveLibrary(LibraryList.lastSelectedLibId) ?: return
                accountPrettyName.setText(libshortname)
            } else if (libdetails == null)
                if (mode == MODE_ACCOUNT_CREATION_INITIAL && accounts.isNullOrEmpty()) {
                }//    updateLibrary();
                else
                    finish()
        }
    }

    private fun addAccountNow() {
        inputToAccount()?.let {
            accounts.add(it)
            finishWithResult()
        }
    }
    private fun changeAccountNow() {
        inputToAccount()?.let {
            accounts.change(oldAccount, it)
            finishWithResult()
        }
    }
    private fun deleteAccountNow() {
        accounts.delete(oldAccount)
        finishWithResult()
    }


    companion object {
        @JvmField internal val MODE_ACCOUNT_CREATION = 134390
        @JvmField internal val MODE_ACCOUNT_CREATION_INITIAL = 134391
        @JvmField internal val MODE_ACCOUNT_MODIFY = 134392
        private val REQUEST_LIBRARY_FOR_ACCOUNT_CREATION = 1236
    }
}
