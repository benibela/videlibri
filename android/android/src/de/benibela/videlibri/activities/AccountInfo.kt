package de.benibela.videlibri.activities

import android.annotation.SuppressLint
import android.app.Activity
import android.content.Intent
import android.graphics.Paint
import android.os.Bundle
import android.text.Editable
import android.text.TextWatcher
import android.view.View
import android.widget.*
import de.benibela.videlibri.*
import de.benibela.videlibri.databinding.AccountinfoBinding
import de.benibela.videlibri.jni.*
import de.benibela.videlibri.utils.*

internal open class EmptyTextWatcher : TextWatcher {
    override fun beforeTextChanged(charSequence: CharSequence, i: Int, i2: Int, i3: Int) {}

    override fun onTextChanged(charSequence: CharSequence, i: Int, i2: Int, i3: Int) {}

    override fun afterTextChanged(editable: Editable) {}
}

class AccountInfo : VideLibriBaseActivity() {

    private var mode: Int = 0
    private var libdetails: LibraryDetails? = null
    private var libshortname: String = ""

    protected lateinit var binding: AccountinfoBinding



    private val oldAccount: Bridge.Account
        get() = intent.getSerializableExtra("account") as? Bridge.Account ?: accounts[0] ?: Bridge.Account()
    private val accountAutoExtend: Boolean
        get() = binding.autoExtendButton.isChecked
    private val accountAutoExtendDays: Int
        get() = binding.autoExtendDaysEdit.text.toString().toIntOrNull() ?: 7

    private fun setActiveLibrary(libid: String?): LibraryDetails? = libid?.let { id ->
        Bridge.VLGetLibraryDetails(id)?.also {
            libshortname = it.prettyNameShort
            binding.libraryTextView.text = it.prettyName
            binding.typeLayout.isVisibleNotGone = it.segregatedAccounts
            libdetails = it
            val accountComment = binding.textViewAccountComment
            accountComment.isVisibleNotGone = it.accountComment.isNotEmpty()
            accountComment.text = it.accountComment
        }
    }

    public override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        binding = setVideLibriView(AccountinfoBinding::inflate)

        mode = intent.getIntExtra("mode", MODE_ACCOUNT_CREATION)
        setActiveLibrary(savedInstanceState?.getString("libId") ?: intent.getStringExtra("libId") ?: "") ?:
            setActiveLibrary(LibraryList.lastSelectedFallbackLibraryId())


        binding.autoExtendButton.setOnCheckedChangeListener { _, b ->
            binding.autoExtendDaysEdit.isEnabled = b
        }


        if (mode == MODE_ACCOUNT_MODIFY) {
            val oldAccount = oldAccount
            setActiveLibrary(oldAccount.libId)

            binding.accountId.setText(oldAccount.name)
            binding.accountPassword.setText(oldAccount.pass)
            binding.accountPrettyName.setText(oldAccount.prettyName)
            binding.autoExtendButton.isChecked = oldAccount.extend
            binding.autoExtendDaysEdit.setText(oldAccount.extendDays.toString())
            binding.saveHistoryButton.isChecked = oldAccount.history
            if (libdetails?.segregatedAccounts == true)
                (if (oldAccount.type == 2) binding.radioButtonExtern else binding.radioButtonIntern).isChecked = true

            binding.deleteButton.setOnClickListener {
                showMessageYesNo(R.string.account_delete) {
                    withActivity<AccountInfo> { deleteAccountNow() }
                }
            }
            binding.completeAccountButton.text = getString(R.string.change)
            binding.completeAccountButton.setOnClickListener {
                if (!checkInputConstraints())
                    return@setOnClickListener
                possiblyWarnAboutShortExtendDays {
                    withActivity<AccountInfo> { changeAccountNow() }
                }

            }
        } else {
            binding.libraryTextView.paintFlags = binding.libraryTextView.paintFlags or Paint.UNDERLINE_TEXT_FLAG
            binding.libraryTextView.setOnClickListener { updateLibrary() }

            binding.deleteButton.visibility = View.GONE
            binding.completeAccountButton.setOnClickListener(View.OnClickListener { _ ->
                if (!checkInputConstraints())
                    return@OnClickListener
                if (binding.accountId.text.isNullOrEmpty())
                    addAccountNow()
                else {
                    if (Accounts.get(libdetails?.id
                                    ?: "", binding.accountId.text.toString()) != null) {
                        showMessage(getString(R.string.warning_duplicate_account))
                        return@OnClickListener
                    }
                    warnAboutAutoExtend {
                        withActivity<AccountInfo> {
                            possiblyWarnAboutShortExtendDays {
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
                }
            })
            binding.accountPrettyName.setText(libshortname)
        }

        if (mode != MODE_ACCOUNT_MODIFY || binding.accountId.text.toString() + " " + libshortname == binding.accountPrettyName.text.toString())
            binding.accountId.addTextChangedListener(object : EmptyTextWatcher() {
                @SuppressLint("SetTextI18n")
                override fun afterTextChanged(editable: Editable) {
                    binding.accountPrettyName.setText(binding.accountId.text.toString() + " " + libshortname)
                }
            })

    }


    override fun onResume() {
        super.onResume()

        if (!globalOptionsAndroid.hasBeenStartedAtLeastOnce) {
            globalOptionsAndroid.hasBeenStartedAtLeastOnce = true
            globalOptionsAndroid.save()
        }

        libdetails ?: setActiveLibrary(LibraryList.lastSelectedFallbackLibraryId()) ?: updateLibrary()
    }

    override fun onSaveInstanceState(outState: Bundle) {
        super.onSaveInstanceState(outState)
        outState.putString("libId", libdetails?.id)
    }

    private fun updateLibrary() {
        binding.libraryTextView.postDelayed({
            startActivityForResult<LibraryList>(
                    REQUEST_LIBRARY_FOR_ACCOUNT_CREATION,
                    "reason" to getString( if (mode == MODE_ACCOUNT_CREATION_INITIAL) R.string.account_createinitial else R.string.account_create)
            )
        }, 300)
    }

    private fun inputToAccount(): Bridge.Account? {
        val acc = Bridge.Account()
        acc.libId = libdetails?.id ?: return null
        acc.name = binding.accountId.text.toString()
        acc.pass = binding.accountPassword.text.toString()
        acc.prettyName = binding.accountPrettyName.text.toString()
        acc.extend = accountAutoExtend
        acc.extendDays = accountAutoExtendDays
        acc.history = binding.saveHistoryButton.isChecked
        acc.type = if (binding.radioButtonExtern.isChecked) 2 else 1
        return acc
    }

    private fun checkInputConstraints(): Boolean {
        val libdetails = this.libdetails
        val errorMessage =
            if (libdetails == null)
                R.string.error_nolibselected
            else if (binding.accountId.text.isNullOrEmpty()) {
                if (!binding.accountPassword.text.isNullOrEmpty())
                    R.string.warning_unnecessary_password
                else if (!libdetails.searchMightWork)
                    R.string.warning_search_is_broken
                else
                    null
            } else if (!libdetails.accountMightWork)
                R.string.warning_account_is_broken
            else if (binding.accountPassword.text.isNullOrEmpty())
                R.string.warning_need_password
            else if (binding.accountPassword.text?.matches(Regex("^[ \t\n\r].*|.*[ \t\n\r]$"))?:false)
                R.string.warning_whitespace_password
            else null
        errorMessage?.let { showMessage(it) }
        return errorMessage == null
    }

    private fun warnAboutAutoExtend(callback: () -> Unit){
        showDialog {
            message(if (accountAutoExtend)
                resources.getQuantityString(R.plurals.warning_autorenewal_onD, accountAutoExtendDays, accountAutoExtendDays)
            else
                getString(R.string.warning_autorenewal_off))
            negativeButton(R.string.cancel)
            positiveButton(R.string.ok) { callback() }
        }
    }

    private fun possiblyWarnAboutShortExtendDays(callback: () -> Unit){
        val minSafeExtendDays = 3
        if (!accountAutoExtend || accountAutoExtendDays >= minSafeExtendDays) callback()
        else {
            showDialog {
                message(R.string.warning_autorenewal_short)
                negativeButton(R.string.cancel)
                positiveButton(R.string.ok) { callback() }
            }
        }
    }

    override fun onActivityResult(requestCode: Int, resultCode: Int, data: Intent?) {
        if (requestCode == REQUEST_LIBRARY_FOR_ACCOUNT_CREATION) {
            if (resultCode == Activity.RESULT_OK) {
                setActiveLibrary(LibraryList.lastSelectedLibId) ?: return
                binding.accountPrettyName.setText(libshortname)
            } else if (libdetails == null) {
                if (mode == MODE_ACCOUNT_CREATION_INITIAL && accounts.isEmpty()) {
                    //    updateLibrary();
                    return
                }
                finish()
            }
        } else super.onActivityResult(requestCode, resultCode, data)
    }

    private fun addAccountNow() {
        inputToAccount()?.let {
            Accounts.add(it)
            globalOptionsAndroid.accountCountBackup = Accounts.size
            globalOptionsAndroid.save()
            finishWithResult()
        }
    }
    private fun changeAccountNow() {
        inputToAccount()?.let {
            Accounts.change(oldAccount, it)
            finishWithResult()
        }
    }
    private fun deleteAccountNow() {
        Accounts.delete(oldAccount)
        globalOptionsAndroid.accountCountBackup = Accounts.size
        globalOptionsAndroid.save()
        finishWithResult()
    }


    companion object {
        internal const val MODE_ACCOUNT_CREATION = 134390
        internal const val MODE_ACCOUNT_CREATION_INITIAL = 134391
        internal const val MODE_ACCOUNT_MODIFY = 134392
        private const val REQUEST_LIBRARY_FOR_ACCOUNT_CREATION = 1236
    }
}
