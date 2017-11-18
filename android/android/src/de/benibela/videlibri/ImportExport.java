package de.benibela.videlibri;

import android.content.DialogInterface;
import android.content.Intent;
import android.os.Bundle;
import android.os.Environment;
import android.view.View;
import android.widget.AbsListView;
import android.widget.ArrayAdapter;
import android.widget.ListView;

import java.io.File;
import java.util.ArrayList;

public class ImportExport extends VideLibriBaseActivity {
    public static final int MODE_IMPORT = 0;
    public static final int MODE_EXPORT = 1;

    int mode;
    ArrayAdapter<String> accountAdapter;
    ArrayAdapter<String> flagAdapter;
    Bridge.ImportExportData data;

    private void checkAll(ListView lv) {
        lv.setChoiceMode(AbsListView.CHOICE_MODE_MULTIPLE);
        int count = lv.getCount();
        for (int i=0;i<count;i++)
            lv.setItemChecked(i, true);
    }

    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);    //To change body of overridden methods use File | Settings | File Templates.
        setVideLibriView(R.layout.importexport);
        mode = getIntent().getIntExtra("mode", MODE_IMPORT);

        File dir = Environment.getExternalStorageDirectory();
        File export = new File(dir , "videlibri.xml" );
        setEditTextText(R.id.edit, export.getAbsolutePath());


        if (mode == MODE_IMPORT) {
            findViewById(R.id.textView).setVisibility(View.GONE);
            findViewById(R.id.textView1).setVisibility(View.GONE);
            findViewById(R.id.listView).setVisibility(View.GONE);
            findViewById(R.id.listView1).setVisibility(View.GONE);
            setTitle(tr(R.string.import_));
            findButtonById(R.id.button).setText(tr(R.string.import_));
            findButtonById(R.id.button).setText(tr(R.string.import_load));
            setTextViewText(R.id.textView, tr(R.string.import_accounts));
            setTextViewText(R.id.textView1, tr(R.string.import_properties));
            setTextViewText(R.id.textView2, tr(R.string.import_file));
            if (VideLibriApp.runningUpdates.size() > 0) {
                Util.showMessage(DialogId.IMPORTEXPORT_DONE, tr(R.string.import_not_while_update_runs));
                return;
            }
        } else {
            setTitle(tr(R.string.export));
            findButtonById(R.id.button).setText(tr(R.string.export));
            setTextViewText(R.id.textView, tr(R.string.export_accounts));
            setTextViewText(R.id.textView1, tr(R.string.export_properties));
            setTextViewText(R.id.textView2, tr(R.string.export_file));

            Bridge.Account[] accounts = Bridge.VLGetAccounts();
            String[] accountNames = new String[accounts.length];
            for (int i=0;i<accounts.length;i++) accountNames[i] = accounts[i].prettyName;
            accountAdapter = new ArrayAdapter<String>(this, android.R.layout.simple_list_item_multiple_choice, accountNames);
            ListView lv = ((ListView)findViewById(R.id.listView));
            lv.setAdapter(accountAdapter);
            checkAll(lv);
        }
        final String[] options = new String[]{tr(R.string.lay_options_option_current), tr(R.string.history), tr(R.string.configuration), tr(R.string.passwords)};
        flagAdapter = new ArrayAdapter<String>(this, android.R.layout.simple_list_item_multiple_choice, options);
        ListView lv = ((ListView)findViewById(R.id.listView1));
        lv.setAdapter(flagAdapter);
        checkAll(lv);
        lv.setItemChecked(flagAdapter.getCount()-1, false);


        findButtonById(R.id.button).setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View view) {
                ListView flagListView = (ListView)findViewById((R.id.listView1));
                int flags = 0;
                int optionI = 0;
                for (int i=0;i<flagAdapter.getCount();i++)
                    if (flagListView.isItemChecked(i)) {
                        for (;!options[optionI].equals(flagAdapter.getItem(i));optionI++)
                            ;
                        flags |= 1 << optionI;
                    }
                if ((flags & Bridge.ImportExportData.PASSWORD) != 0)
                    flags |= Bridge.ImportExportData.CONFIG;


                ListView accountListView = (ListView)findViewById((R.id.listView));
                try {
                    if (mode == MODE_IMPORT) {
                        if (data == null) {
                            data = Bridge.VLImportAccountsPrepare(getEditTextText(R.id.edit));

                            accountAdapter = new ArrayAdapter<String>(ImportExport.this, android.R.layout.simple_list_item_multiple_choice, data.accountsToImport);
                            ListView lv = (ListView)findViewById(R.id.listView);
                            lv.setAdapter(accountAdapter);
                            checkAll(lv);

                            ArrayList<String> newOptions = new ArrayList<String>();
                            for (int i=0;i<options.length;i++)
                                if ((data.flags & (1 << i)) != 0)
                                    newOptions.add(options[i]);
                            flagAdapter = new ArrayAdapter<String>(ImportExport.this, android.R.layout.simple_list_item_multiple_choice, newOptions);
                            lv = ((ListView)findViewById(R.id.listView1));
                            lv.setAdapter(flagAdapter);
                            checkAll(lv);

                            findButtonById(R.id.button).setText(tr(R.string.import_));
                            findViewById(R.id.textView).setVisibility(View.VISIBLE);
                            findViewById(R.id.textView1).setVisibility(View.VISIBLE);
                            findViewById(R.id.listView).setVisibility(View.VISIBLE);
                            findViewById(R.id.listView1).setVisibility(View.VISIBLE);
                            findViewById(R.id.textView2).setVisibility(View.GONE);
                            findViewById(R.id.edit).setVisibility(View.GONE);
                        } else {
                            ArrayList<String> choosen = new ArrayList<String>();
                            for (int i=0;i<accountAdapter.getCount();i++)
                                if (accountListView.isItemChecked(i))
                                    choosen.add(data.accountsToImport[i]);
                            data.accountsToImport = new String[choosen.size()];
                            for (int i=0;i<data.accountsToImport.length;i++)
                                data.accountsToImport[i] = choosen.get(i);
                            data.flags = flags;
                            Bridge.VLImportAccounts(data);
                            VideLibriApp.refreshAccountList();
                            VideLibriApp.refreshDisplayedLendBooks();
                            Util.showMessage(DialogId.IMPORTEXPORT_DONE, tr(R.string.import_done));
                            data = null;
                        }
                    }   else {
                        Bridge.Account[] accounts = Bridge.VLGetAccounts();
                        ArrayList<Bridge.Account> choosen = new ArrayList<Bridge.Account>();
                        for (int i=0;i<accounts.length;i++)
                            if (accountListView.isItemChecked(i))
                                choosen.add(accounts[i]);
                        accounts = new Bridge.Account[choosen.size()];
                        for (int i=0;i<accounts.length;i++)
                            accounts[i] = choosen.get(i);
                        Bridge.VLExportAccounts(getEditTextText(R.id.edit), accounts, flags);
                        Util.showMessage(DialogId.IMPORTEXPORT_DONE, tr(R.string.export_done));
                    }
                } catch (Bridge.InternalError e) {
                    Util.showMessage(e.getMessage());
                }
            }
        });

    }

    @Override
    protected void onDestroy() {
        if (data != null) {
            data.flags = 0;
            Bridge.VLImportAccounts(data);
        }
        super.onDestroy();
    }

    @Override
    boolean onDialogResult(int dialogId, int buttonId, Bundle more) {
        switch (dialogId) {
            case DialogId.IMPORTEXPORT_DONE:
                finish();
                return true;
        }
        return super.onDialogResult(dialogId, buttonId, more);
    }
}
