package de.benibela.videlibri;

import android.app.Activity;
import android.content.Context;
import android.content.Intent;
import android.content.SharedPreferences;
import android.os.Bundle;
import android.preference.PreferenceManager;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.*;
import org.acra.ACRA;

import java.util.ArrayList;

/**
 * Created with IntelliJ IDEA.
 * User: benito
 * Date: 5/20/13
 * Time: 5:33 PM
 * To change this template use File | Settings | File Templates.
 */
public class Options extends VideLibriBaseActivity{
    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);    //To change body of overridden methods use File | Settings | File Templates.
        setVideLibriView(R.layout.options);




    }

    static final int NEW_ACCOUNT_CREATION_RESULT = 1235;

    @Override
    protected void onResume() {
        super.onResume();    //To change body of overridden methods use File | Settings | File Templates.

        Bridge.Options options = Bridge.VLGetOptions();

        setEditTextText(R.id.notificationsTimeDelta,""+options.nearTime);
        setEditTextText(R.id.refreshInterval,""+options.refreshInterval);
        setCheckBoxChecked(R.id.logging, options.logging);

        SharedPreferences sp = PreferenceManager.getDefaultSharedPreferences(this);
        setCheckBoxChecked(R.id.notifications, sp.getBoolean("notifications", true));
        setEditTextText(R.id.notificationsServiceDelay, ""+sp.getInt("notificationsServiceDelay", 15));

        ((CheckBox)findViewById(R.id.noLendBookDetails)).setChecked(sp.getBoolean("noLendBookDetails", false));
        ((CheckBox)findViewById(R.id.showRenewCount)).setChecked(sp.getBoolean("showRenewCount", true));
        ((CheckBox)findViewById(R.id.alwaysFilterOnHistory)).setChecked(sp.getBoolean("alwaysFilterOnHistory", true));
        final String[] filterKeys = getResources().getStringArray(R.array.filterable_properties);
        String filtering = sp.getString("filtering", "");
        setSpinnerSelection ((Spinner)findViewById(R.id.searchFilter), filterKeys, filtering);



        SharedPreferences acraprefs = ACRA.getACRASharedPreferences();
        setCheckBoxChecked(R.id.loggingSend, acraprefs.getBoolean(ACRA.PREF_ENABLE_SYSTEM_LOGS, true));

        LayoutInflater inflater = getLayoutInflater();


        LinearLayout linearLayout = (LinearLayout) findViewById(R.id.accounts);
        linearLayout.removeAllViews();

        for (final Bridge.Account acc: VideLibriApp.accounts) if (acc != null) {
            Button btn = (Button) inflater.inflate(R.layout.insetbutton, null);
            btn.setText(acc.prettyName);
            linearLayout.addView(btn);
            btn.setOnClickListener(new View.OnClickListener() {
                @Override
                public void onClick(View view) {
                    Intent intent = new Intent(Options.this, AccountInfo.class);
                    intent.putExtra("mode", AccountInfo.MODE_ACCOUNT_MODIFY);
                    intent.putExtra("account", acc);
                    startActivity(intent);
                }
            });
        }




        findButtonById(R.id.newaccount).setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View view) {
                Intent intent = new Intent(Options.this, AccountInfo.class);
                intent.putExtra("mode", AccountInfo.MODE_ACCOUNT_CREATION) ;
                startActivityForResult(intent, NEW_ACCOUNT_CREATION_RESULT);
            }
        });


        linearLayout = (LinearLayout) findViewById(R.id.libraries);
        linearLayout.removeAllViews();
        for (final String userLibId: options.roUserLibIds) if (userLibId != null) {
            final Bridge.LibraryDetails details = Bridge.VLGetLibraryDetails(userLibId);
            if (details == null) continue;
            Button btn = (Button) inflater.inflate(R.layout.insetbutton, null);
            btn.setText(details.prettyName);
            linearLayout.addView(btn);
            btn.setOnClickListener(new View.OnClickListener() {
                @Override
                public void onClick(View view) {
                    Intent intent = new Intent(Options.this, NewLibrary.class);
                    intent.putExtra("mode", NewLibrary.MODE_LIBRARY_MODIFY);
                    intent.putExtra("libId", userLibId);
                    startActivity(intent);
                }
            });
        }


        findButtonById(R.id.newlib).setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View view) {
                startActivity(new Intent(Options.this, NewLibrary.class));
            }
        });
    }

    @Override
    protected void onActivityResult(int requestCode, int resultCode, Intent data) {
        if (requestCode == NEW_ACCOUNT_CREATION_RESULT && resultCode == AccountInfo.RESULT_OK)
            finish();
        else super.onActivityResult(requestCode, resultCode, data);
    }

    @Override
    protected void onPause() {
        super.onPause();    //To change body of overridden methods use File | Settings | File Templates.


        Bridge.Options options = new Bridge.Options();
        options.nearTime = Util.strToIntDef(getEditTextText(R.id.notificationsTimeDelta), 3);
        options.refreshInterval = Util.strToIntDef(getEditTextText(R.id.refreshInterval), 1);
        options.logging = getCheckBoxChecked(R.id.logging);
        Bridge.VLSetOptions(options);


        SharedPreferences sp = PreferenceManager.getDefaultSharedPreferences(this);
        SharedPreferences.Editor editor = sp.edit();
        editor.putBoolean("notifications", getCheckBoxChecked(R.id.notifications));
        editor.putInt("notificationsServiceDelay", Util.strToIntDef((getEditTextText(R.id.notificationsServiceDelay)), 15));

        editor.putBoolean("noLendBookDetails", ((CheckBox)findViewById(R.id.noLendBookDetails)).isChecked());
        editor.putBoolean("showRenewCount", ((CheckBox)findViewById(R.id.showRenewCount)).isChecked());
        editor.putBoolean("alwaysFilterOnHistory", ((CheckBox)findViewById(R.id.alwaysFilterOnHistory)).isChecked());

        final String[] filterKeys = getResources().getStringArray(R.array.filterable_properties);
        int filteringPos = ((Spinner)findViewById(R.id.searchFilter)).getSelectedItemPosition();
        if (filteringPos >= 0 && filteringPos < filterKeys.length) editor.putString("filtering", filterKeys[filteringPos]);

        editor.commit();

        NotificationService.resheduleDailyIfNecessary(this, false);


        VideLibriApp.setACRAlogcat(getCheckBoxChecked(R.id.loggingSend));

    }


    static void showLendingOptionsInView(Activity activity, View v){
        SharedPreferences sp = PreferenceManager.getDefaultSharedPreferences(activity);
        if (!VideLibri.displayHistory) ((RadioButton) v.findViewById(R.id.radioButton1)).setChecked(true);
        else ((RadioButton) v.findViewById(R.id.radioButton2)).setChecked(true);

        LayoutInflater inflater = activity.getLayoutInflater();

        LinearLayout linearLayout = (LinearLayout) v.findViewById(R.id.viewaccounts);
        linearLayout.removeAllViews();
        for (final Bridge.Account acc: VideLibriApp.accounts) if (acc != null) {
            CheckBox viewAcc = (CheckBox) inflater.inflate(R.layout.checkbox, null);
            viewAcc.setText(acc.prettyName);
            viewAcc.setChecked(!VideLibri.hiddenAccounts.contains(acc));
            viewAcc.setOnCheckedChangeListener(new CompoundButton.OnCheckedChangeListener() {
                @Override
                public void onCheckedChanged(CompoundButton compoundButton, boolean b) {
                    if (!b == VideLibri.hiddenAccounts.contains(acc)) return;
                    if (!b) VideLibri.hiddenAccounts.add(acc);
                    else VideLibri.hiddenAccounts.remove(acc);
                }
            });
            linearLayout.addView(viewAcc);
        }


        final String[] sortingKeys = activity.getResources().getStringArray(R.array.sortable_properties);
        String sorting = sp.getString("sorting", "dueDate"),
                grouping = sp.getString("grouping", "_dueWeek");
        setSpinnerSelection ((Spinner)v.findViewById(R.id.sorting), sortingKeys, sorting);
        setSpinnerSelection ((Spinner)v.findViewById(R.id.grouping), sortingKeys, grouping);
    }

    static void putLendingOptionsFromView(Activity activity, View v){
        SharedPreferences sp = PreferenceManager.getDefaultSharedPreferences(activity);
        SharedPreferences.Editor editor = sp.edit();

        VideLibri.displayHistory = ((RadioButton) v.findViewById(R.id.radioButton2)).isChecked();
        editor.putBoolean("displayHistory", VideLibri.displayHistory);

        final String[] sortingKeys = activity.getResources().getStringArray(R.array.sortable_properties);
        int sortingPos = ((Spinner)v.findViewById(R.id.sorting)).getSelectedItemPosition();
        if (sortingPos >= 0 && sortingPos < sortingKeys.length) editor.putString("sorting", sortingKeys[sortingPos]);
        int groupingPos = ((Spinner)v.findViewById(R.id.grouping)).getSelectedItemPosition();
        if (groupingPos >= 0 && groupingPos < sortingKeys.length) editor.putString("grouping", sortingKeys[groupingPos]);

        editor.commit();
    }

}
