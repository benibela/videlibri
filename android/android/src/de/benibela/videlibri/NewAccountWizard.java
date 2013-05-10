package de.benibela.videlibri;

import android.app.Activity;
import android.app.AlertDialog;
import android.content.Intent;
import android.os.Bundle;
import android.view.View;
import android.widget.*;

import java.util.*;


public class NewAccountWizard extends Activity {

    void makeLibView(ExpandableListView lv){
        String[] libs = Bridge.VLGetLibraries();

        final List<Map<String, String>> cities = new ArrayList<Map<String, String>>();
        final List<List<Map<String, String>>> localLibs = new ArrayList<List<Map<String, String>>>();
        final TreeMap<String, String> shortNames = new TreeMap<String, String>();
        final TreeMap<String, String> ids = new TreeMap<String, String>();
        for (String s : libs) {
            String[] temp = s.split("\\|");
            if (cities.isEmpty() || !cities.get(cities.size()-1).get("NAME").equals(temp[1])) {
                cities.add(new TreeMap<String, String>());
                cities.get(cities.size()-1).put("NAME", temp[1]);
                localLibs.add(new ArrayList<Map<String, String>>());
            }
            localLibs.get(localLibs.size()-1).add(new TreeMap<String, String>());;
            localLibs.get(localLibs.size()-1).get(localLibs.get(localLibs.size()-1).size()-1).put("NAME", temp[2]);
            shortNames.put(temp[2], temp[3]);
            ids.put(temp[2], temp[0]);
        }

        SimpleExpandableListAdapter adapter = new SimpleExpandableListAdapter(
                this,
                cities, android.R.layout.simple_expandable_list_item_1, new String[]{"NAME"}, new int[]{android.R.id.text1},
                localLibs,  android.R.layout.simple_expandable_list_item_2, new String[]{"NAME"}, new int[]{android.R.id.text2});
        lv.setAdapter(adapter);

        lv.setOnChildClickListener(new ExpandableListView.OnChildClickListener() {
            @Override
            public boolean onChildClick(ExpandableListView expandableListView, View view, int i, int i2, long l) {
                //VideLibri.showMessage(NewAccountWizard.this, localLibs.get(i).get(i2).get("NAME"));

                Intent intent = new Intent(NewAccountWizard.this, AccountInfo.class);
                intent.putExtra("libName", localLibs.get(i).get(i2).get("NAME"));
                intent.putExtra("libShortName", shortNames.get(localLibs.get(i).get(i2).get("NAME")));
                intent.putExtra("libId", ids.get(localLibs.get(i).get(i2).get("NAME")));
                startActivityForResult(intent, 123);
                return true;
            }
        });

        lv.setOnItemClickListener(new AdapterView.OnItemClickListener() {
            @Override
            public void onItemClick(AdapterView<?> adapterView, View view, int i, long l) {
                AlertDialog.Builder builder = new AlertDialog.Builder(NewAccountWizard.this);
                builder.setMessage(i + " " + l);
                builder.setTitle("VideLibri");
                                 /*
                DialogInterface.OnClickListener dialogClickListener = new DialogInterface.OnClickListener()
                {
                    @Override
                    public void onClick(DialogInterface dialog, int which)
                    {
                        switch (which)
                        {
                            case DialogInterface.BUTTON_POSITIVE:
                                LCLOnMessageBoxFinished(lclbutton1, 0);
                                break;
                            case DialogInterface.BUTTON_NEUTRAL:
                                LCLOnMessageBoxFinished(lclbutton2, 0);
                                break;
                            case DialogInterface.BUTTON_NEGATIVE:
                                LCLOnMessageBoxFinished(lclbutton3, 0);
                                break;
                        };
                    }
                };

                DialogInterface.OnCancelListener dialogCancelListener = new DialogInterface.OnCancelListener()
                {
                    @Override
                    public void onCancel(DialogInterface dialog)
                    {
                        // The Cancel button number matches for LCLIntf.MessageBox and LCLIntf.PromptDialog
                        LCLOnMessageBoxFinished(idButtonCancel, 0);
                    }
                };
                if (lclbutton1 >= 0) builder.setPositiveButton(lclbutton1str, dialogClickListener);
                if (lclbutton2 >= 0) builder.setNeutralButton(lclbutton2str, dialogClickListener);
                if (lclbutton3 >= 0) builder.setNegativeButton(lclbutton3str, dialogClickListener);
                builder.show().setOnCancelListener(dialogCancelListener);                          */
                builder.show();
            }
        });
    }

    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.chooselib);

        ExpandableListView lv = (ExpandableListView) findViewById(R.id.libListView);
        makeLibView(lv);
    }

    @Override
    protected void onActivityResult(int requestCode, int resultCode, Intent data) {
        if (requestCode == 123)
            finish(); //account created (if resultCode == REQUEST_OK)
    }
}
