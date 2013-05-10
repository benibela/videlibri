package de.benibela.videlibri;

import android.app.Activity;
import android.app.AlertDialog;
import android.content.Intent;
import android.os.Bundle;
import android.text.Editable;
import android.text.TextWatcher;
import android.view.View;
import android.widget.*;

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

public class AccountInfo extends Activity {

    String libId;
    EditText accountId, accountPassword, accountPrettyName;

    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.accountinfo);

        TextView lib = (TextView) findViewById(R.id.libraryTextView);
        accountId = (EditText) findViewById(R.id.accountId);
        accountPassword = (EditText) findViewById(R.id.accountPassword);
        accountPrettyName = (EditText) findViewById(R.id.accountPrettyName);

        lib.setText(getIntent().getStringExtra("libName"));
        final String libShortName =           getIntent().getStringExtra("libShortName");
        accountPrettyName.setText(libShortName);
        libId = getIntent().getStringExtra("libId");


        accountId.addTextChangedListener(new EmptyTextWatcher(){
            @Override
            public void afterTextChanged(Editable editable) {
                accountPrettyName.setText(accountId.getText()+" "+libShortName);
            }
        });

        findViewById(R.id.completeAccountButton).setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View view) {
                Bridge.Account acc = new Bridge.Account();
                acc.libId = libId;
                acc.name = accountId.getText().toString();
                acc.pass = accountPassword.getText().toString();
                acc.prettyName = accountPrettyName.getText().toString();
                acc.extend = ((RadioButton) findViewById(R.id.autoExtendButton)).isChecked();
                acc.extendDays = Integer.parseInt( ((EditText) findViewById(R.id.autoExtendDaysEdit)).getText().toString());
                acc.history = ((RadioButton) findViewById(R.id.saveHistoryButton)).isChecked();
                VideLibri.addAccount(acc);
                setResult(RESULT_OK, new Intent());
                AccountInfo.this.finish();
            }
        });

    }
}
