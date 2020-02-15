package de.benibela.videlibri

import android.app.Activity
import android.content.Intent
import android.os.Bundle
import android.preference.PreferenceManager
import androidx.preference.Preference
import androidx.preference.PreferenceCategory
import androidx.appcompat.view.ContextThemeWrapper
import android.util.TypedValue
import android.view.View
import android.widget.CompoundButton
import android.widget.Spinner
import de.benibela.videlibri.internet.DownloadCertificate
import de.benibela.videlibri.internet.UserKeyStore
import de.benibela.videlibri.jni.Bridge
import de.benibela.videlibri.notifications.NotificationScheduling
import java.util.regex.Pattern


class Options : VideLibriBaseActivity() {


    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        syncBridgeToPreferences(this)
        setContentView(R.layout.videlibribaselayout)
    }

    override fun onPostCreate(savedInstanceState: Bundle?) {
        super.onPostCreate(savedInstanceState)
        supportFragmentManager.beginTransaction().replace(R.id.content_holder, SettingsFragment()).commit()
    }

    class SettingsFragment : androidx.preference.PreferenceFragmentCompat() {

        override fun onCreate(savedInstanceState: Bundle?) {
            super.onCreate(savedInstanceState)
            addPreferencesFromResource(R.xml.preferences)
            val prefs = arrayOf(findPreference("bridge_logging"), findPreference("bridge_nearTime"), findPreference("bridge_refreshInterval"))
            val listener = Preference.OnPreferenceChangeListener { preference, newValue ->
                Bridge.globalOptions = Bridge.VLGetOptions()
                val options = Bridge.globalOptions
                when (preference.key) {
                    "bridge_logging" -> options.logging = newValue as Boolean
                    "bridge_nearTime" -> options.nearTime = newValue as Int
                    "bridge_refreshInterval" -> options.refreshInterval = newValue as Int
                    "languageOverride" -> context?.let { VideLibriApp.setLanguageOverride(it, newValue as String) }
                }//if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.HONEYCOMB) {
                //    getActivity().recreate();
                //}
                Bridge.VLSetOptions(options)
                true
            }
            for (p in prefs) p.onPreferenceChangeListener = listener
        }

        override fun onCreatePreferences(savedInstanceState: Bundle?, rootKey: String?) {

        }

        private inner class CustomPreferenceMaker internal constructor(
                internal var contextThemeWrapper: ContextThemeWrapper
        ) {
            internal lateinit var cat: PreferenceCategory

            internal fun beginCat(key: String) {
                cat = preferenceScreen.findPreference(key) as PreferenceCategory
                cat.removeAll()
            }

            internal fun makePreference(title: String, onClick: Preference.OnPreferenceClickListener): Preference {
                return makePreference(title, null, onClick)
            }

            internal fun makePreference(title: String, summary: String?, onClick: Preference.OnPreferenceClickListener): Preference {
                return Preference(contextThemeWrapper).also {
                    it.title = title
                    if (summary != null) it.summary = summary
                    it.onPreferenceClickListener = onClick
                    cat.addPreference(it)
                }
            }
        }

        override fun onResume() {
            super.onResume()
            updatePreferences()
        }

        internal fun updatePreferences() {
            val context = context ?: return
            val themeTypedValue = TypedValue()
            val theme = context.theme
            theme?.resolveAttribute(R.attr.preferenceTheme, themeTypedValue, true)
            val cpm = CustomPreferenceMaker(ContextThemeWrapper(context, themeTypedValue.resourceId))

            cpm.beginCat("accounts")

            var summary = getString(R.string.lay_options_label_accounts_summary)
            accounts.forEach {acc ->
                cpm.makePreference(acc.prettyName, summary, Preference.OnPreferenceClickListener {
                    startActivity<AccountInfo>(
                            "mode" to AccountInfo.MODE_ACCOUNT_MODIFY,
                            "account" to acc
                    )
                    true
                })
            }

            cpm.makePreference(getString(R.string.lay_options_btn_newaccount), Preference.OnPreferenceClickListener {
                startActivity<AccountInfo>(
                        "mode" to AccountInfo.MODE_ACCOUNT_CREATION
                )
                true
            })


            cpm.beginCat("ownlibraries")

            val options = Bridge.VLGetOptions()

            summary = getString(R.string.lay_options_label_ownlibraries_summary)
            options.roUserLibIds.filterNotNull().forEach { userLibId ->
                val details = Bridge.VLGetLibraryDetails(userLibId) ?: return@forEach
                cpm.makePreference(details.prettyName, summary, Preference.OnPreferenceClickListener {
                    startActivity<NewLibrary>(
                            "mode" to NewLibrary.MODE_LIBRARY_MODIFY,
                            "libId" to userLibId
                    )
                    true
                })
            }

            cpm.makePreference(getString(R.string.lay_options_btn_newlib), Preference.OnPreferenceClickListener {
                startActivity<NewLibrary>()
                true
            })
            cpm.makePreference(getString(R.string.lay_options_btn_editsource), Preference.OnPreferenceClickListener {
                startActivity<SourceEdit>()
                true
            })

            cpm.beginCat("owncertificates")
            if (UserKeyStore.hasCertificates())
                for (cert in UserKeyStore.getCertificates()) {
                    cpm.makePreference(UserKeyStore.getFingerprint(cert), getString(R.string.lay_options_btn_newcertificate_delete), Preference.OnPreferenceClickListener {
                        showMessageYesNo(R.string.certificate_delete) {
                            UserKeyStore.removeUserCertificate(cert)
                            UserKeyStore.storeUserCertificates(PreferenceManager.getDefaultSharedPreferences(VideLibriApp.currentContext()))
                            currentActivity<Options>()?.updatePreferences()
                        }
                        true
                    })
                }

            cpm.makePreference(getString(R.string.lay_options_btn_newcertificate), Preference.OnPreferenceClickListener {
                val defaultServer: String? = VideLibriApp.errors.asSequence().map { e ->
                    if (e.kind == Bridge.PendingException.KIND_INTERNET && e.error?.contains("https://") == true) {
                        val matcher = Pattern.compile("https://([^/]+)").matcher(e.error)
                        if (matcher.find()) matcher.group(1) else null
                    } else null
                }.firstOrNull()

                showInputDialog(R.string.certificate_download, default = defaultServer) { text ->
                    Thread(DownloadCertificate(text)).start()
                }
                true
            })
        }
    }

    override fun onActivityResult(requestCode: Int, resultCode: Int, data: Intent?) {
        if (requestCode == NEW_ACCOUNT_CREATION_RESULT && resultCode == Activity.RESULT_OK)
            finish()
        else
            super.onActivityResult(requestCode, resultCode, data)
    }

    fun updatePreferences() {
        for (f in supportFragmentManager.fragments)
            (f as? SettingsFragment)?.updatePreferences()
    }

    override fun onPause() {
        super.onPause()

        NotificationScheduling.rescheduleDailyIfNecessary(this, false)
    }

    companion object {


        internal const val NEW_ACCOUNT_CREATION_RESULT = 1235


        internal fun syncBridgeToPreferences(activity: Activity) {
            Bridge.globalOptions = Bridge.VLGetOptions()
            val options = Bridge.globalOptions
            PreferenceManager.getDefaultSharedPreferences(activity).edit().apply {
                putBoolean("bridge_logging", options.logging)
                putInt("bridge_nearTime", options.nearTime)
                putInt("bridge_refreshInterval", options.refreshInterval)
                apply()
            }
        }
        /*static void syncPreferencesToBridge(Activity activity){
        Bridge.Options options = Bridge.VLGetOptions();
        SharedPreferences sp = PreferenceManager.getDefaultSharedPreferences(activity);
        options.logging = sp.getBoolean("bridge_logging", false);
        options.nearTime = sp.getInt("bridge_nearTime", 3);
        options.refreshInterval = sp.getInt("bridge_refreshInterval", 1);
        Bridge.VLSetOptions(options);
    }*/

        internal fun showLendingOptionsInView(activity: Activity, v: View) {
            val sp = PreferenceManager.getDefaultSharedPreferences(activity)
            v.findViewById<CompoundButton>(R.id.viewHistory).isChecked = LendingList.displayHistory


            val sortingKeys = activity.resources.getStringArray(R.array.sortable_properties)
            val sorting = sp.getString("sorting", "dueDate")
            val grouping = sp.getString("grouping", "_dueWeek")
            v.findViewById<Spinner>(R.id.sorting).setSelection(sorting, sortingKeys)
            v.findViewById<Spinner>(R.id.grouping).setSelection(grouping, sortingKeys)
        }

        internal fun putLendingOptionsFromView(activity: Activity, v: View) {
            val sortingKeys = activity.resources.getStringArray(R.array.sortable_properties)
            LendingList.displayHistory = v.findViewById<CompoundButton>(R.id.viewHistory).isChecked
            PreferenceManager.getDefaultSharedPreferences(activity).edit().apply {
                putBoolean("displayHistory", LendingList.displayHistory)

                sortingKeys.getOrNull(v.findViewById<Spinner>(R.id.sorting).selectedItemPosition)?.let { putString("sorting", it) }
                sortingKeys.getOrNull(v.findViewById<Spinner>(R.id.grouping).selectedItemPosition)?.let { putString("grouping", it) }

                apply()
            }
        }
    }

}
