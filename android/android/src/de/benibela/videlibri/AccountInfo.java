package de.benibela.videlibri;

import android.app.Activity;
import android.app.AlertDialog;
import android.content.DialogInterface;
import android.content.Intent;
import android.graphics.Paint;
import android.os.Bundle;
import android.text.Editable;
import android.text.TextWatcher;
import android.util.Log;
import android.view.View;
import android.widget.*;
import com.actionbarsherlock.view.Menu;

import java.util.*;

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
    String libId,libShortName,libName;

    TextView lib;
    EditText accountId, accountPassword, accountPrettyName;

    Bridge.Account oldAccount;

    static final int MODE_ACCOUNT_CREATION = 134390 ;
    static final int MODE_ACCOUNT_CREATION_INITIAL = 134391 ;
    static final int MODE_ACCOUNT_MODIFY = 134392 ;
    static final int REQUEST_LIBRARY_FOR_ACCOUNT_CREATION = 1236;

    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.accountinfo);

        lib = (TextView) findViewById(R.id.libraryTextView);
        accountId = (EditText) findViewById(R.id.accountId);
        accountPassword = (EditText) findViewById(R.id.accountPassword);
        accountPrettyName = (EditText) findViewById(R.id.accountPrettyName);

        mode = getIntent().getIntExtra("mode", MODE_ACCOUNT_CREATION);


        libName = getStringExtraSafe("libName");
        libShortName = getStringExtraSafe("libShortName");
        libId = getStringExtraSafe("libId");


        if (mode != MODE_ACCOUNT_MODIFY || accountPrettyName.equals(accountId + " "+libShortName) )
            accountId.addTextChangedListener(new EmptyTextWatcher() {
                @Override
                public void afterTextChanged(Editable editable) {
                    accountPrettyName.setText(accountId.getText() + " " + libShortName);
                }
            });

        ((CheckBox) findViewById(R.id.autoExtendButton)).setOnCheckedChangeListener(new CompoundButton.OnCheckedChangeListener() {
            @Override
            public void onCheckedChanged(CompoundButton compoundButton, boolean b) {
                ((EditText) findViewById(R.id.autoExtendDaysEdit)).setEnabled(b);
            }
        });



        if (mode == MODE_ACCOUNT_MODIFY) {
            final Bridge.Account oldAccount = (Bridge.Account) getIntent().getSerializableExtra("account");
            Bridge.Library lib = oldAccount.getLibrary();

            libName = lib.namePretty;
            libShortName = lib.nameShort;
            libId = lib.id;

            accountId.setText(oldAccount.name);
            accountPassword.setText(oldAccount.pass);
            accountPrettyName.setText(oldAccount.prettyName);
            ((CheckBox) findViewById(R.id.autoExtendButton)).setChecked(oldAccount.extend);
            ((EditText) findViewById(R.id.autoExtendDaysEdit)).setText("" + oldAccount.extendDays);
            ((CheckBox) findViewById(R.id.saveHistoryButton)).setChecked(oldAccount.history);

            findViewById(R.id.deleteButton).setOnClickListener(new View.OnClickListener() {
                @Override
                public void onClick(View view) {
                    showMessageYesNo("Soll das Konto wirklich gelöscht werden?", new MessageHandler() {
                        @Override
                        public void onDialogEnd(DialogInterface dialogInterface, int i) {
                            if (i == DialogInterface.BUTTON_POSITIVE) {
                                VideLibriApp.deleteAccount(oldAccount);
                                setResult(RESULT_OK, new Intent());
                                AccountInfo.this.finish();
                            }
                        }
                    });
                }
            });
            findButtonById(R.id.completeAccountButton).setText("ändern");
            findViewById(R.id.completeAccountButton).setOnClickListener(new View.OnClickListener() {
                @Override
                public void onClick(View view) {
                    VideLibriApp.changeAccount(oldAccount, inputToAccount());
                    setResult(RESULT_OK, new Intent());
                    AccountInfo.this.finish();
                }
            });
        } else {
            lib.setPaintFlags(lib.getPaintFlags() | Paint.UNDERLINE_TEXT_FLAG);
            if (libId.equals("")) updateLibrary();
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
                    MessageHandler temp = new MessageHandler() {
                        @Override
                        public void onDialogEnd(DialogInterface dialogInterface, int i) {
                            VideLibriApp.addAccount(inputToAccount());
                            setResult(RESULT_OK, new Intent());
                            AccountInfo.this.finish();
                        }
                    };
                    if (libName.contains("(alpha)") && accountId.getText().length() > 0) {
                        showMessage(
                                "Hinweis: Für diese Bibliothek wurden der Kontozugriff noch nicht getestet. Bitte berichten Sie, ob das Konto gleich angezeigt wird oder nicht.",
                                temp);
                    } else temp.onDialogEnd(null, 0);
                }
            });
            accountPrettyName.setText(libShortName);
        }

        lib.setText(libName);
    }

    /*@Override
    public boolean onPrepareOptionsMenu(Menu menu) {
        boolean x = super.onPrepareOptionsMenu(menu);
        if (mode == MODE_ACCOUNT_CREATION_INITIAL) menu.findItem(R.id.search).setVisible(false);
        return x;
    } */

    void updateLibrary(){
        Intent intent = new Intent(this, LibraryList.class);
        if (mode == MODE_ACCOUNT_CREATION_INITIAL) {
            intent.putExtra("reason", "Wählen Sie Ihre Bibliothek, um in ihrem Katalog zu suchen oder auf Ihr Bibliothekskonto zuzugreifen: ");
         //        intent.putExtra("initial", true);
        } else
            intent.putExtra("reason", "Wählen Sie Ihre Bibliothek, um ein neues Konto zu registrieren: ");
        startActivityForResult(intent, REQUEST_LIBRARY_FOR_ACCOUNT_CREATION);
    }

    Bridge.Account inputToAccount(){
        Bridge.Account acc = new Bridge.Account();
        acc.libId = libId;
        acc.name = accountId.getText().toString();
        acc.pass = accountPassword.getText().toString();
        acc.prettyName = accountPrettyName.getText().toString();
        acc.extend = ((CheckBox) findViewById(R.id.autoExtendButton)).isChecked();
        acc.extendDays = Integer.parseInt( ((EditText) findViewById(R.id.autoExtendDaysEdit)).getText().toString());
        acc.history = ((CheckBox) findViewById(R.id.saveHistoryButton)).isChecked();
        return acc;
    }


    @Override
    protected void onActivityResult(int requestCode, int resultCode, Intent data) {
        if (requestCode == REQUEST_LIBRARY_FOR_ACCOUNT_CREATION) {
            if (resultCode == LibraryList.RESULT_OK) {
                libName = data.getStringExtra("libName");
                libShortName = data.getStringExtra("libShortName");
                libId = data.getStringExtra("libId");
                lib.setText(libName);
                accountPrettyName.setText(libShortName);
            } else if (libId.equals(""))
                if (mode == MODE_ACCOUNT_CREATION_INITIAL)
                    updateLibrary();
                else
                    finish();
        }
    }
}
