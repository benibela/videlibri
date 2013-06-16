package de.benibela.videlibri;

import android.app.Activity;
import android.app.AlertDialog;
import android.content.Intent;
import android.os.Bundle;
import android.util.Log;
import android.view.View;
import android.widget.*;

import java.util.*;


public class LibraryList extends VideLibriBaseActivity {

    void makeLibView(ExpandableListView lv){
        Bridge.initialize();
        Bridge.Library[] libs = Bridge.getLibraries();

        final List<Map<String, String>> cities = new ArrayList<Map<String, String>>();
        final List<List<Map<String, String>>> localLibs = new ArrayList<List<Map<String, String>>>();
        //final TreeMap<String, String> shortNames = new TreeMap<String, String>();
        //final TreeMap<String, String> ids = new TreeMap<String, String>();

        int autoExpand = 0;
        if (searchMode && VideLibri.instance != null && VideLibri.instance.accounts != null && VideLibri.instance.accounts.length > 0) {
            ArrayList<String> used = new ArrayList<String>();
            autoExpand = 1;
            cities.add(new TreeMap<String, String>());
            cities.get(cities.size()-1).put("NAME", "mit Konten");
            localLibs.add(new ArrayList<Map<String, String>>());
            for (Bridge.Account account: VideLibri.instance.accounts) {
                if (used.contains(account.libId)) continue;
                used.add(account.libId);

                TreeMap map = new TreeMap<String, String>();
                localLibs.get(localLibs.size()-1).add(map);;
                for (Bridge.Library lib: libs)
                    if (lib.id.equals(account.libId)) {
                        map.put("NAME", lib.namePretty);
                        map.put("ID", lib.id);
                        map.put("SHORT", lib.nameShort);
                        break;
                    }
            }
        }

        for (Bridge.Library lib : libs) {
            if (cities.isEmpty() || !cities.get(cities.size()-1).get("NAME").equals(lib.locationPretty)) {
                cities.add(new TreeMap<String, String>());
                cities.get(cities.size()-1).put("NAME", lib.locationPretty);
                if ("-".equals(lib.locationPretty) && autoExpand < 2) autoExpand+=1;
                localLibs.add(new ArrayList<Map<String, String>>());
            }
            TreeMap<String,String> map = new TreeMap<String, String>();
            localLibs.get(localLibs.size()-1).add(map);;
            map.put("NAME", lib.namePretty);
            map.put("SHORT", lib.nameShort);
            map.put("ID", lib.id);
        }

        SimpleExpandableListAdapter adapter = new SimpleExpandableListAdapter(
                this,
                cities, android.R.layout.simple_expandable_list_item_1, new String[]{"NAME"}, new int[]{android.R.id.text1},
                localLibs,  R.layout.libraryinlistview, new String[]{"NAME"}, new int[]{android.R.id.text1});
        lv.setAdapter(adapter);

        lv.setOnChildClickListener(new ExpandableListView.OnChildClickListener() {
            @Override
            public boolean onChildClick(ExpandableListView expandableListView, View view, int i, int i2, long l) {
                //VideLibri.showMessage(NewAccountWizard.this, localLibs.get(i).get(i2).get("NAME"));
                Intent result = new Intent();
                result.putExtra("libName", localLibs.get(i).get(i2).get("NAME"));
                result.putExtra("libShortName", localLibs.get(i).get(i2).get("SHORT"));
                result.putExtra("libId", localLibs.get(i).get(i2).get("ID"));

                setResult(RESULT_OK, result);
                LibraryList.this.finish();
                return true;
            }
        });

        for (int i=0;i<autoExpand;i++)
            lv.expandGroup(i);
    }

    boolean searchMode;

    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.chooselib);

        String reason = getIntent().getStringExtra("reason");
        if (reason != null && !"".equals(reason))
            setTextViewText(R.id.textView, reason);

        searchMode = getIntent().getBooleanExtra("search", false);


        ExpandableListView lv = (ExpandableListView) findViewById(R.id.libListView);
        makeLibView(lv);
    }


}
