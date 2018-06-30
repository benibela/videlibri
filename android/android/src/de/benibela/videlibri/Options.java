package de.benibela.videlibri;

import android.app.Activity;
import android.content.Context;
import android.content.Intent;
import android.content.SharedPreferences;
import android.os.Bundle;
import android.preference.PreferenceManager;
import android.support.v7.preference.Preference;
import android.support.v7.preference.PreferenceCategory;
import android.support.v7.view.ContextThemeWrapper;
import android.util.TypedValue;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.*;
import org.acra.ACRA;

import java.util.ArrayList;


public class Options extends VideLibriBaseActivity{


    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);    //To change body of overridden methods use File | Settings | File Templates.
        syncBridgeToPreferences(this);
        setContentView(R.layout.videlibribaselayout);
    }

    @Override
    protected void onPostCreate(Bundle savedInstanceState) {
        super.onPostCreate(savedInstanceState);
        getSupportFragmentManager().beginTransaction().replace(R.id.content_holder, new SettingsFragment()).commit();
    }

    static public class SettingsFragment extends android.support.v7.preference.PreferenceFragmentCompat  {

        @Override
        public void onCreate(Bundle savedInstanceState) {
            super.onCreate(savedInstanceState);
            addPreferencesFromResource(R.xml.preferences);
            Preference[] prefs = new Preference[]{ findPreference("bridge_logging"), findPreference("bridge_nearTime"), findPreference("bridge_refreshInterval") };
            Preference.OnPreferenceChangeListener listener = new Preference.OnPreferenceChangeListener() {
                @Override
                public boolean onPreferenceChange(Preference preference, Object newValue) {
                    Bridge.globalOptions = Bridge.VLGetOptions();
                    Bridge.Options options = Bridge.globalOptions;
                    switch (preference.getKey()) {
                        case "bridge_logging":
                            options.logging = (Boolean) newValue;
                            break;
                        case "bridge_nearTime":
                            options.nearTime = (Integer) newValue;
                            break;
                        case "bridge_refreshInterval":
                            options.refreshInterval = (Integer) newValue;
                            break;
                    }
                    Bridge.VLSetOptions(options);
                    return true;
                }
            };
            for (Preference p: prefs) p.setOnPreferenceChangeListener(listener);
        }

        @Override
        public void onCreatePreferences(Bundle savedInstanceState, String rootKey) {

        }

        @Override
        public void onResume() {
            super.onResume();

            PreferenceCategory cat = (PreferenceCategory)getPreferenceScreen().findPreference("accounts");
            cat.removeAll();

            TypedValue themeTypedValue = new TypedValue();
            getContext().getTheme().resolveAttribute(R.attr.preferenceTheme, themeTypedValue, true);
            @SuppressWarnings("RestrictedApi")
            ContextThemeWrapper contextThemeWrapper = new ContextThemeWrapper(getContext(), themeTypedValue.resourceId);



            String summary = getString(R.string.lay_options_label_accounts_summary);
            for (final Bridge.Account acc: VideLibriApp.accounts) if (acc != null) {
                Preference pref = new Preference(contextThemeWrapper);
                pref.setTitle(acc.prettyName);
                pref.setSummary(summary);
                pref.setOnPreferenceClickListener(new Preference.OnPreferenceClickListener(){
                    @Override
                    public boolean onPreferenceClick(Preference preference) {
                        Intent intent = new Intent(getActivity(), AccountInfo.class);
                        intent.putExtra("mode", AccountInfo.MODE_ACCOUNT_MODIFY);
                        intent.putExtra("account", acc);
                        startActivity(intent);
                        return true;
                    }
                });
                cat.addPreference(pref);
            }


            Preference pref = new Preference(contextThemeWrapper);
            pref.setTitle(getString(R.string.lay_options_btn_newaccount));
            pref.setOnPreferenceClickListener(new Preference.OnPreferenceClickListener() {
                @Override
                public boolean onPreferenceClick(Preference preference) {
                    Intent intent = new Intent(getActivity(), AccountInfo.class);
                    intent.putExtra("mode", AccountInfo.MODE_ACCOUNT_CREATION) ;
                    startActivityForResult(intent, NEW_ACCOUNT_CREATION_RESULT);
                    return true;
                }
            });
            cat.addPreference(pref);


            cat = (PreferenceCategory)getPreferenceScreen().findPreference("ownlibraries");
            cat.removeAll();

            Bridge.Options options = Bridge.VLGetOptions();

            summary = getString(R.string.lay_options_label_ownlibraries_summary);
            for (final String userLibId: options.roUserLibIds) if (userLibId != null) {
                final Bridge.LibraryDetails details = Bridge.VLGetLibraryDetails(userLibId);
                if (details == null) continue;
                pref = new Preference(contextThemeWrapper);
                pref.setTitle(details.prettyName);
                pref.setSummary(summary);
                cat.addPreference(pref);
                pref.setOnPreferenceClickListener(new Preference.OnPreferenceClickListener() {
                    @Override
                    public boolean onPreferenceClick(Preference preference) {
                        Intent intent = new Intent(getActivity(), NewLibrary.class);
                        intent.putExtra("mode", NewLibrary.MODE_LIBRARY_MODIFY);
                        intent.putExtra("libId", userLibId);
                        startActivity(intent);
                        return true;
                    }
                });
            }

            pref = new Preference(contextThemeWrapper);
            pref.setTitle(getString(R.string.lay_options_btn_newlib));
            pref.setOnPreferenceClickListener(new Preference.OnPreferenceClickListener() {
                @Override
                public boolean onPreferenceClick(Preference preference) {
                    startActivity(new Intent(getActivity(), NewLibrary.class));
                    return true;
                }
            });
            cat.addPreference(pref);
        }

        @Override
        public void onDisplayPreferenceDialog(Preference preference) {
            if (preference instanceof PreferenceInteger) {

            } else super.onDisplayPreferenceDialog(preference);
        }
    }


    static final int NEW_ACCOUNT_CREATION_RESULT = 1235;

    @Override
    protected void onResume() {
        super.onResume();    //To change body of overridden methods use File | Settings | File Templates.




    }

    @Override
    protected void onActivityResult(int requestCode, int resultCode, Intent data) {
        if (requestCode == NEW_ACCOUNT_CREATION_RESULT && resultCode == AccountInfo.RESULT_OK)
            finish();
        else super.onActivityResult(requestCode, resultCode, data);
    }

    @Override
    protected void onPause() {
        super.onPause();

       /* Bridge.Options options = new Bridge.Options();

         Bridge.VLSetOptions(options);
                 Bridge.Options options = Bridge.VLGetOptions();
*/

        NotificationService.resheduleDailyIfNecessary(this, false);


    }


    static void syncBridgeToPreferences(Activity activity){
        Bridge.globalOptions = Bridge.VLGetOptions();
        Bridge.Options options = Bridge.globalOptions;
        SharedPreferences sp = PreferenceManager.getDefaultSharedPreferences(activity);
        SharedPreferences.Editor editor = sp.edit();
        editor.putBoolean("bridge_logging", options.logging);
        editor.putInt("bridge_nearTime", options.nearTime);
        editor.putInt("bridge_refreshInterval", options.refreshInterval);
        editor.apply();
    }
    /*static void syncPreferencesToBridge(Activity activity){
        Bridge.Options options = Bridge.VLGetOptions();
        SharedPreferences sp = PreferenceManager.getDefaultSharedPreferences(activity);
        options.logging = sp.getBoolean("bridge_logging", false);
        options.nearTime = sp.getInt("bridge_nearTime", 3);
        options.refreshInterval = sp.getInt("bridge_refreshInterval", 1);
        Bridge.VLSetOptions(options);
    }*/

    static void showLendingOptionsInView(Activity activity, View v){
        SharedPreferences sp = PreferenceManager.getDefaultSharedPreferences(activity);
        ((Checkable)v.findViewById(R.id.viewHistory)).setChecked(VideLibri.displayHistory);


        final String[] sortingKeys = activity.getResources().getStringArray(R.array.sortable_properties);
        String sorting = sp.getString("sorting", "dueDate"),
                grouping = sp.getString("grouping", "_dueWeek");
        setSpinnerSelection ((Spinner)v.findViewById(R.id.sorting), sortingKeys, sorting);
        setSpinnerSelection ((Spinner)v.findViewById(R.id.grouping), sortingKeys, grouping);
    }

    static void putLendingOptionsFromView(Activity activity, View v){
        SharedPreferences sp = PreferenceManager.getDefaultSharedPreferences(activity);
        SharedPreferences.Editor editor = sp.edit();

        VideLibri.displayHistory = ((CompoundButton) v.findViewById(R.id.viewHistory)).isChecked();
        editor.putBoolean("displayHistory", VideLibri.displayHistory);

        final String[] sortingKeys = activity.getResources().getStringArray(R.array.sortable_properties);
        int sortingPos = ((Spinner)v.findViewById(R.id.sorting)).getSelectedItemPosition();
        if (sortingPos >= 0 && sortingPos < sortingKeys.length) editor.putString("sorting", sortingKeys[sortingPos]);
        int groupingPos = ((Spinner)v.findViewById(R.id.grouping)).getSelectedItemPosition();
        if (groupingPos >= 0 && groupingPos < sortingKeys.length) editor.putString("grouping", sortingKeys[groupingPos]);

        editor.commit();
    }

}
