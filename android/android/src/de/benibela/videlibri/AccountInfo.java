package de.benibela.videlibri;

import android.content.DialogInterface;
import android.content.Intent;
import android.graphics.Paint;
import android.os.Bundle;
import android.text.Editable;
import android.text.TextWatcher;
import android.view.View;
import android.widget.*;

import de.benibela.videlibri.jni.Bridge;

class EmptyTextWatcher implements TextWatcher{
    @Override
    public void beforeTextChanged(CharSequence charSequence, int i, int i2, int i3) {}

    @Override
    public void onTextChanged(CharSequence charSequence, int i, int i2, int i3) {
    }

    @Override
    public void afterTextChanged(Editable editable) {
    }
}

public class AccountInfo extends VideLibriBaseActivity {

    int mode;
    Bridge.LibraryDetails libdetails;
    String libshortname;

    TextView lib;
    EditText accountId, accountPassword, accountPrettyName;

    static final int MODE_ACCOUNT_CREATION = 134390 ;
    static final int MODE_ACCOUNT_CREATION_INITIAL = 134391 ;
    static final int MODE_ACCOUNT_MODIFY = 134392 ;
    static final int REQUEST_LIBRARY_FOR_ACCOUNT_CREATION = 1236;

    private Bridge.Account getOldAccount(){
        Bridge.Account account = (Bridge.Account) getIntent().getSerializableExtra("account");
        if (account != null) return account;
        if (VideLibriApp.accounts.length > 0 ) return VideLibriApp.accounts[0];
        return new Bridge.Account(); //modify with null account would crash
    }

    public void setActiveLibrary(String libid){
        libdetails = Bridge.VLGetLibraryDetails(libid);
        if (libdetails != null){
            libshortname = libdetails.prettyNameShort;
            lib.setText(libdetails.prettyName);
            findViewById(R.id.typeLayout).setVisibility(libdetails.segregatedAccounts ? View.VISIBLE : View.GONE);
        }
    }

    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setVideLibriView(R.layout.accountinfo);

        lib = (TextView) findViewById(R.id.libraryTextView);
        accountId = (EditText) findViewById(R.id.accountId);
        accountPassword = (EditText) findViewById(R.id.accountPassword);
        accountPrettyName = (EditText) findViewById(R.id.accountPrettyName);

        mode = getIntent().getIntExtra("mode", MODE_ACCOUNT_CREATION);

        if (savedInstanceState != null)
            setActiveLibrary(savedInstanceState.getString("libId"));
        else
            setActiveLibrary(getStringExtraSafe("libId")); //todo: this is not used? like Bridge.library.putinintent


        if (libdetails == null && (System.currentTimeMillis() - LibraryList.lastSelectedTime) < LibraryList.SELECTION_REUSE_TIME)
            setActiveLibrary(LibraryList.lastSelectedLibId);


        if (mode != MODE_ACCOUNT_MODIFY || (accountId + " "+libshortname).equals(accountPrettyName.getText().toString()) )
            accountId.addTextChangedListener(new EmptyTextWatcher() {
                @Override
                public void afterTextChanged(Editable editable) {
                    accountPrettyName.setText(accountId.getText() + " " + libshortname);
                }
            });

        ((CheckBox) findViewById(R.id.autoExtendButton)).setOnCheckedChangeListener(new CompoundButton.OnCheckedChangeListener() {
            @Override
            public void onCheckedChanged(CompoundButton compoundButton, boolean b) {
                findViewById(R.id.autoExtendDaysEdit).setEnabled(b);
            }
        });



        if (mode == MODE_ACCOUNT_MODIFY) {
            final Bridge.Account oldAccount = getOldAccount();
            setActiveLibrary(oldAccount.libId);

            accountId.setText(oldAccount.name);
            accountPassword.setText(oldAccount.pass);
            accountPrettyName.setText(oldAccount.prettyName);
            ((CheckBox) findViewById(R.id.autoExtendButton)).setChecked(oldAccount.extend);
            ((EditText) findViewById(R.id.autoExtendDaysEdit)).setText("" + oldAccount.extendDays);
            ((CheckBox) findViewById(R.id.saveHistoryButton)).setChecked(oldAccount.history);
            if (libdetails != null && libdetails.segregatedAccounts)
                ((RadioButton) findViewById(oldAccount.type == 2 ? R.id.radioButtonExtern : R.id.radioButtonIntern)).setChecked(true);

            findViewById(R.id.deleteButton).setOnClickListener(new View.OnClickListener() {
                @Override
                public void onClick(View view) {
                    Util.showMessageYesNo(DialogId.ACCOUNT_DELETE_CONFIRM, tr(R.string.account_delete));
                }
            });
            findButtonById(R.id.completeAccountButton).setText(tr(R.string.change));
            findViewById(R.id.completeAccountButton).setOnClickListener(new View.OnClickListener() {
                @Override
                public void onClick(View view) {
                    if (libdetails == null) return;
                    VideLibriApp.changeAccount(oldAccount, inputToAccount());
                    setResult(RESULT_OK, new Intent());
                    AccountInfo.this.finish();
                }
            });
        } else {
            lib.setPaintFlags(lib.getPaintFlags() | Paint.UNDERLINE_TEXT_FLAG);
            lib.setOnClickListener(new View.OnClickListener() {
                @Override
                public void onClick(View view) {
                    updateLibrary();
                }
            });

            findViewById(R.id.deleteButton).setVisibility(View.GONE);
            findViewById(R.id.completeAccountButton).setOnClickListener(new View.OnClickListener() {
                @Override
                public void onClick(View view) {
                    if (libdetails == null) {
                        Util.showMessage(tr(R.string.error_nolibselected));
                        return;
                    }

                    if (accountId.getText().length() == 0) {
                        onDialogResult(DialogId.ACCOUNT_ADD_NOW, DialogInterface.BUTTON_POSITIVE, null);
                        return;
                    }

                    Util.showMessageNegPos(DialogId.ACCOUNT_ADD_RENEWAL_MODE_CONFIRM,
                        getAccountAutoExtend()
                                ? getResources().getQuantityString(R.plurals.warning_autorenewal_onD, getAccountAutoExtendDays(), getAccountAutoExtendDays())
                                : tr(R.string.warning_autorenewal_off),
                            R.string.cancel,
                            R.string.ok
                        );
                }
            });
            accountPrettyName.setText(libshortname);
        }

    }

    @Override
    protected void onResume() {
        super.onResume();

        if (libdetails == null && (System.currentTimeMillis() - LibraryList.lastSelectedTime) < LibraryList.SELECTION_REUSE_TIME)
            setActiveLibrary(LibraryList.lastSelectedLibId);
        if (libdetails == null) updateLibrary();
    }

    @Override
    protected void onSaveInstanceState(Bundle outState) {
        super.onSaveInstanceState(outState);

        if (libdetails != null) {
            outState.putString("libId", libdetails.id);
        }
    }

    void updateLibrary(){
        findViewById(R.id.libraryTextView).postDelayed(new Runnable() {
            @Override
            public void run() {
                Intent intent = new Intent(AccountInfo.this, LibraryList.class);
                if (mode == MODE_ACCOUNT_CREATION_INITIAL) {
                    intent.putExtra("reason", tr(R.string.account_createinitial));
                    //        intent.putExtra("initial", true);
                } else
                    intent.putExtra("reason", tr(R.string.about_create));
                startActivityForResult(intent, REQUEST_LIBRARY_FOR_ACCOUNT_CREATION);
            }
        }, 300);
    }

    private boolean getAccountAutoExtend(){ return  ((CheckBox) findViewById(R.id.autoExtendButton)).isChecked(); }
    private int getAccountAutoExtendDays(){ return Util.strToIntDef( ((EditText) findViewById(R.id.autoExtendDaysEdit)).getText().toString(), 7); }

    Bridge.Account inputToAccount(){
        Bridge.Account acc = new Bridge.Account();
        acc.libId = libdetails.id;
        acc.name = accountId.getText().toString();
        acc.pass = accountPassword.getText().toString();
        acc.prettyName = accountPrettyName.getText().toString();
        acc.extend = getAccountAutoExtend();
        acc.extendDays = getAccountAutoExtendDays();
        acc.history = ((CheckBox) findViewById(R.id.saveHistoryButton)).isChecked();
        acc.type = ((RadioButton) findViewById(R.id.radioButtonExtern)).isChecked() ? 2 : 1;
        return acc;
    }


    @Override
    boolean onDialogResult(int dialogId, int buttonId, Bundle more) {
        switch (dialogId) {
            case DialogId.ACCOUNT_DELETE_CONFIRM:
                if (buttonId == DialogInterface.BUTTON_POSITIVE) {
                    VideLibriApp.deleteAccount(getOldAccount());
                    setResult(RESULT_OK, new Intent());
                    AccountInfo.this.finish();
                }
                return true;
            case DialogId.ACCOUNT_ADD_RENEWAL_MODE_CONFIRM:
                if (buttonId == DialogInterface.BUTTON_POSITIVE) {
                    if (libdetails == null) break;
                    if (libdetails.prettyName.contains("(nur Suche getestet)"))
                        Util.showMessage(DialogId.ACCOUNT_ADD_NOW, tr(R.string.warning_alphalib));
                    else
                        onDialogResult(DialogId.ACCOUNT_ADD_NOW, DialogInterface.BUTTON_POSITIVE, null);
                }
                return true;
            case DialogId.ACCOUNT_ADD_NOW:
                if (libdetails == null) break;
                VideLibriApp.addAccount(inputToAccount());
                setResult(RESULT_OK, new Intent());
                AccountInfo.this.finish();
                return true;
        }
        return super.onDialogResult(dialogId, buttonId, more);
    }

    @Override
    protected void onActivityResult(int requestCode, int resultCode, Intent data) {
        if (requestCode == REQUEST_LIBRARY_FOR_ACCOUNT_CREATION) {
            if (resultCode == LibraryList.RESULT_OK) {
                setActiveLibrary(LibraryList.lastSelectedLibId);
                if (libdetails != null)
                    accountPrettyName.setText(libshortname);
            } else if (libdetails == null)
                if (mode == MODE_ACCOUNT_CREATION_INITIAL && (VideLibriApp.accounts == null || VideLibriApp.accounts.length == 0))
                {}//    updateLibrary();
                else
                    finish();
        }
    }
}
