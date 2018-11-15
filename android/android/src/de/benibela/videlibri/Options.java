package de.benibela.videlibri;

import android.annotation.SuppressLint;
import android.app.Activity;
import android.app.Application;
import android.content.DialogInterface;
import android.content.Intent;
import android.content.SharedPreferences;
import android.os.Build;
import android.os.Bundle;
import android.preference.PreferenceManager;
import android.support.v4.app.Fragment;
import android.support.v7.preference.Preference;
import android.support.v7.preference.PreferenceCategory;
import android.support.v7.view.ContextThemeWrapper;
import android.util.TypedValue;
import android.view.View;
import android.widget.Checkable;
import android.widget.CompoundButton;
import android.widget.Spinner;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import de.benibela.videlibri.internet.DownloadCertificate;
import de.benibela.videlibri.internet.UserKeyStore;
import de.benibela.videlibri.jni.Bridge;


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
                        case "languageOverride":
                            VideLibriApp.setLanguageOverride(getContext(), (String)newValue);
                            //if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.HONEYCOMB) {
                            //    getActivity().recreate();
                            //}
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

        private class CustomPreferenceMaker{
            ContextThemeWrapper contextThemeWrapper;
            PreferenceCategory cat;

            CustomPreferenceMaker(ContextThemeWrapper contextThemeWrapper){
                this.contextThemeWrapper = contextThemeWrapper;
            }

            void beginCat(String key){
                cat = (PreferenceCategory)getPreferenceScreen().findPreference(key);
                cat.removeAll();
            }
            @SuppressWarnings("UnusedReturnValue")
            Preference makePreference(String title, Preference.OnPreferenceClickListener onClick) {
                return makePreference(title, null, onClick);
            }
            Preference makePreference(String title, String summary, Preference.OnPreferenceClickListener onClick) {
                Preference pref = new Preference(contextThemeWrapper);
                pref.setTitle(title);
                if (summary != null) pref.setSummary(summary);
                pref.setOnPreferenceClickListener(onClick);
                cat.addPreference(pref);
                return pref;
            }
        }

        @Override
        public void onResume() {
            super.onResume();
            updatePreferences();
        }

        private void updatePreferences (){
            TypedValue themeTypedValue = new TypedValue();
            getContext().getTheme().resolveAttribute(R.attr.preferenceTheme, themeTypedValue, true);
            @SuppressWarnings("RestrictedApi")
            CustomPreferenceMaker cpm = new CustomPreferenceMaker(new ContextThemeWrapper(getContext(), themeTypedValue.resourceId));

            cpm.beginCat("accounts");

            String summary = getString(R.string.lay_options_label_accounts_summary);
            for (final Bridge.Account acc: VideLibriApp.accounts) if (acc != null) {
                cpm.makePreference(acc.prettyName, summary, new Preference.OnPreferenceClickListener(){
                    @Override
                    public boolean onPreferenceClick(Preference preference) {
                        Intent intent = new Intent(getActivity(), AccountInfo.class);
                        intent.putExtra("mode", AccountInfo.MODE_ACCOUNT_MODIFY);
                        intent.putExtra("account", acc);
                        startActivity(intent);
                        return true;
                    }
                });
            }

            cpm.makePreference(getString(R.string.lay_options_btn_newaccount), new Preference.OnPreferenceClickListener() {
                @Override
                public boolean onPreferenceClick(Preference preference) {
                    Intent intent = new Intent(getActivity(), AccountInfo.class);
                    intent.putExtra("mode", AccountInfo.MODE_ACCOUNT_CREATION) ;
                    startActivityForResult(intent, NEW_ACCOUNT_CREATION_RESULT);
                    return true;
                }
            });


            cpm.beginCat("ownlibraries");

            Bridge.Options options = Bridge.VLGetOptions();

            summary = getString(R.string.lay_options_label_ownlibraries_summary);
            for (final String userLibId: options.roUserLibIds) if (userLibId != null) {
                final Bridge.LibraryDetails details = Bridge.VLGetLibraryDetails(userLibId);
                if (details == null) continue;
                cpm.makePreference(details.prettyName, summary, new Preference.OnPreferenceClickListener() {
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

            cpm.makePreference(getString(R.string.lay_options_btn_newlib), new Preference.OnPreferenceClickListener() {
                @Override
                public boolean onPreferenceClick(Preference preference) {
                    startActivity(new Intent(getActivity(), NewLibrary.class));
                    return true;
                }
            });
            cpm.makePreference(getString(R.string.lay_options_btn_editsource), new Preference.OnPreferenceClickListener() {
                @Override
                public boolean onPreferenceClick(Preference preference) {
                    startActivity(new Intent(getActivity(), SourceEdit.class));
                    return true;
                }
            });

            cpm.beginCat("owncertificates");
            if (UserKeyStore.hasCertificates())
                for (final byte[] cert: UserKeyStore.getCertificates()) {
                    cpm.makePreference(UserKeyStore.getFingerprint(cert), getString(R.string.lay_options_btn_newcertificate_delete), new Preference.OnPreferenceClickListener() {
                    @Override
                    public boolean onPreferenceClick(Preference preference) {
                        Bundle b = new Bundle();
                        b.putByteArray("cert", cert);
                        Util.showMessageYesNo(DialogId.OPTIONS_DELETE_USER_CERTIFICATE, getString(R.string.certificate_delete), b);
                        return true;
                    }});
                }

            cpm.makePreference(getString(R.string.lay_options_btn_newcertificate), new Preference.OnPreferenceClickListener() {
                @Override
                public boolean onPreferenceClick(Preference preference) {
                    String defaultServer = "";
                    if (VideLibriApp.errors != null)
                        for (Bridge.PendingException e: VideLibriApp.errors)
                            if (e.kind == Bridge.PendingException.KIND_INTERNET)
                                if (e.error != null && e.error.contains("https://")) {
                                    Matcher matcher = Pattern.compile("https://([^/]+)").matcher(e.error);
                                    if (matcher.find()) {
                                        defaultServer = matcher.group(1);
                                        break;
                                    }
                                }

                    Util.inputDialog(DialogId.OPTIONS_ADD_USER_CERTIFICATE, getString(R.string.certificate_download), null, defaultServer);
                    return true;
                }
            });
        }

        @Override
        public void onDisplayPreferenceDialog(Preference preference) {
            if (preference instanceof PreferenceInteger) {

            } else super.onDisplayPreferenceDialog(preference);
        }
    }


    static final int NEW_ACCOUNT_CREATION_RESULT = 1235;

    @Override
    protected void onActivityResult(int requestCode, int resultCode, Intent data) {
        if (requestCode == NEW_ACCOUNT_CREATION_RESULT && resultCode == AccountInfo.RESULT_OK)
            finish();
        else super.onActivityResult(requestCode, resultCode, data);
    }



    @Override
    boolean onDialogResult(int dialogId, int buttonId, Bundle more) {
        switch (dialogId) {
            case DialogId.OPTIONS_ADD_USER_CERTIFICATE:
                if (buttonId != Util.MessageHandlerCanceled)
                    (new Thread(new DownloadCertificate(more.getString("text")))).start();
                return true;
            case DialogId.OPTIONS_DELETE_USER_CERTIFICATE:
                if (buttonId == DialogInterface.BUTTON_POSITIVE) {
                    UserKeyStore.removeUserCertificate(more.getByteArray("cert"));
                    UserKeyStore.storeUserCertificates(PreferenceManager.getDefaultSharedPreferences(VideLibriApp.currentContext()));
                    updatePreferences();
                }
                return true;
            default:
                return super.onDialogResult(dialogId, buttonId, more);
        }
    }

    public void updatePreferences() {
        for (Fragment f: getSupportFragmentManager().getFragments())
            if (f instanceof SettingsFragment)
                ((SettingsFragment)f).updatePreferences();
    }

    @Override
    protected void onPause() {
        super.onPause();

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

    @SuppressLint("ApplySharedPref")
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
