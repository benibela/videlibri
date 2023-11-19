package de.benibela.videlibri.activities

import android.app.Activity
import android.content.Intent
import android.os.Bundle
import android.util.TypedValue
import androidx.appcompat.view.ContextThemeWrapper
import androidx.preference.Preference
import androidx.preference.PreferenceCategory
import de.benibela.videlibri.LibraryUpdateLoader
import de.benibela.videlibri.R
import de.benibela.videlibri.VideLibriApp
import de.benibela.videlibri.accounts
import de.benibela.videlibri.components.CategoryBuilder
import de.benibela.videlibri.components.PreferenceScreenBuilder
import de.benibela.videlibri.components.PreferenceSeekBar
import de.benibela.videlibri.databinding.OptionsLendingsBinding
import de.benibela.videlibri.internet.DownloadCertificate
import de.benibela.videlibri.internet.UserKeyStore
import de.benibela.videlibri.jni.*
import de.benibela.videlibri.notifications.NotificationScheduling
import de.benibela.videlibri.notifications.checkForRequiredNotificationPermission
import de.benibela.videlibri.utils.*
import java.util.regex.Pattern


class Options : VideLibriBaseActivity() {


    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContentView(R.layout.videlibribaselayout)
    }

    override fun onPostCreate(savedInstanceState: Bundle?) {
        super.onPostCreate(savedInstanceState)
        supportFragmentManager.beginTransaction().replace(R.id.content_holder, SettingsFragment()).commit()
    }

    class SettingsFragment : androidx.preference.PreferenceFragmentCompat() {

        private var categoryAccounts: PreferenceCategory? = null
        private var categoryOwnLibraries: PreferenceCategory? = null
        private var categoryOwnCertificates: PreferenceCategory? = null

        override fun onCreatePreferences(savedInstanceState: Bundle?, rootKey: String?) {
            val context = context ?: return
            val themeTypedValue = TypedValue()
            val theme = context.theme
            theme?.resolveAttribute(R.attr.preferenceTheme, themeTypedValue, true)
            val ctw = ContextThemeWrapper(context, themeTypedValue.resourceId)

            preferenceScreen = preferenceManager.createPreferenceScreen(ctw)

            val saveOptionsAndroidOnly: (Preference?,Any) -> Unit = {_, _ -> globalOptionsAndroid.save();  }
            val saveOptionsSHARED: (Preference?, Any) -> Unit = {_, _ -> Bridge.VLSetOptions(globalOptionsShared); }

            PreferenceScreenBuilder(ctw, preferenceScreen) {
                category(R.string.lay_options_caption_displayoptions){
                    list {
                        property(globalOptionsAndroid.bookListDisplayOptions::filterKey)
                        title(R.string.lay_options_label_filtering)
                        dialogTitle(R.string.lay_options_label_filtering)
                        entries(R.array.filterable_properties_tr)
                        entryValues(R.array.filterable_properties)
                        summary(R.string.lay_options_label_filtering_summary)
                        onChanged(saveOptionsAndroidOnly)
                    }
                    switchCompat {
                        property(globalOptionsAndroid.bookListDisplayOptions::alwaysFilterOnHistory)
                        summaryOn(R.string.lay_options_filter_search_in_history_summary_on)
                        summaryOff(R.string.lay_options_filter_search_in_history_summary_off)
                        title(R.string.lay_options_filter_search_in_history)
                        onChanged(saveOptionsAndroidOnly)
                    }
                    switchCompat {
                        property(globalOptionsAndroid.bookListDisplayOptions::noBorrowedBookDetails)
                        summaryOn(R.string.lay_options_option_onlytitles_summary_on)
                        title(R.string.lay_options_option_onlytitles)
                        onChanged(saveOptionsAndroidOnly)
                    }
                    switchCompat {
                        property(globalOptionsAndroid.bookListDisplayOptions::showRenewCount)
                        summaryOn(R.string.lay_show_renewcount_summary_on)
                        title(R.string.lay_show_renewcount)
                        onChanged(saveOptionsAndroidOnly)
                    }
                }



                categoryAccounts = category(R.string.lay_options_caption_accountchange){
                    createPreferencesAccounts(this)
                }


                var seekBarToToggle: PreferenceSeekBar? = null
                category(R.string.lay_options_caption_notifications) {
                    switchCompat {
                        property(globalOptionsAndroid.notifications::enabled)
                        title(R.string.lay_options_option_warn)
                        summaryOn(R.string.lay_options_option_warn_summary_on)
                        onChanged { _, v ->
                            seekBarToToggle?.isEnabled = v
                            globalOptionsAndroid.save()
                            VideLibriApp.currentActivity?.let(::checkForRequiredNotificationPermission)
                        }
                    }
                    seekBarToToggle = seekBar(PreferenceSeekBar(ctw)) {
                        property(globalOptionsAndroid.notifications::serviceDelay)
                        //dependency(notificationSwitcher.key)
                        max(1440)
                        title(R.string.lay_options_label_autocheckdelay)
                        onChanged(saveOptionsAndroidOnly)
                    } as PreferenceSeekBar
                    seekBarToToggle!!.apply {
                        dynamicSummary = getString(R.string.lay_options_label_autocheckdelay_summary)
                        safeMax = 120
                        unsafeWarning = getString(R.string.lay_options_label_autocheckdelay_too_large)
                        isEnabled = globalOptionsAndroid.notifications.enabled
                        showDynamicSummary()
                    }
                    seekBar(PreferenceSeekBar(ctw)) {
                        property(globalOptionsShared::nearTime)
                        max(31)
                        title(R.string.lay_options_warningtimedelay)
                        onChanged(saveOptionsSHARED)
                        (preference as PreferenceSeekBar).apply {
                            dynamicSummary = getString(R.string.lay_options_warningtimedelay_summary)
                            safeMin = 3
                            unsafeWarning = getString(R.string.lay_options_warningtimedelay_too_small)
                            showDynamicSummary()
                        }
                    }
                }


                categoryOwnLibraries = category(R.string.lay_options_caption_ownlibs) {
                    createPreferencesOwnLibraries(this)
                }

                categoryOwnCertificates = category(R.string.lay_options_caption_owncertificates) {
                    createPreferencesOwnCertificates(this)
                }

                category(R.string.lay_options_caption_misc){
                    seekBar(PreferenceSeekBar(ctw)) {
                        property(globalOptionsShared::refreshInterval)
                        max(31)
                        title(R.string.lay_options_refreshInterval)
                        onChanged(saveOptionsSHARED)
                        (preference as PreferenceSeekBar).apply {
                            dynamicSummary = getString(R.string.lay_options_check_summary)
                            safeMax = 5
                            unsafeWarning = getString(R.string.lay_options_check_summary_too_large)
                            showDynamicSummary()
                        }
                    }

                    list {
                        dialogTitle(R.string.lay_options_label_language)
                        entries(R.array.languages_tr)
                        entryValues(R.array.languages)
                        title(R.string.lay_options_label_language)
                        summary(R.string.lay_options_label_language_summary)
                        preference.key = "languageOverride"
                        preference.isPersistent = true
                        onChanged {_, newValue ->
                            VideLibriApp.setLanguageOverride(context, newValue)
                        }
                    }

                    switchCompat {
                        property(globalOptionsAndroid::logging)
                        summaryOn(R.string.lay_options_option_debuglog_summary_on)
                        title(R.string.lay_options_option_debuglog)
                        onChanged(saveOptionsAndroidOnly)
                    }

                    switchCompat {
                        summaryOn(R.string.lay_options_option_includelog_summary_on)
                        title(R.string.lay_options_option_includelog)
                        preference.isPersistent = true
                        preference.key = "acra.syslog.enable"
                    }

                }

            }
        }

        private fun createPreferencesAccounts(categoryBuilder: CategoryBuilder) = categoryBuilder.apply {
            val summary = getString(R.string.lay_options_label_accounts_summary)
            accounts.forEach { acc ->
                preference {
                    title(acc.prettyName)
                    summary(summary)
                    onClick {
                        startActivity<AccountInfo>(
                                "mode" to AccountInfo.MODE_ACCOUNT_MODIFY,
                                "account" to acc
                        )
                    }
                }
            }
            preference {
                title(R.string.lay_options_btn_newaccount)
                onClick { startActivity<AccountInfo>(
                        "mode" to AccountInfo.MODE_ACCOUNT_CREATION
                ) }
            }

        }
        private fun createPreferencesOwnLibraries(categoryBuilder: CategoryBuilder) = categoryBuilder.apply {
            val options = Bridge.VLGetOptions()
            val summary = getString(R.string.lay_options_label_ownlibraries_summary)
            options.userLibIds.forEach { userLibId ->
                val details = Bridge.VLGetLibraryDetails(userLibId) ?: return@forEach
                preference {
                    title(details.prettyName)
                    summary(summary)
                    onClick {
                        startActivity<NewLibrary>(
                                "mode" to NewLibrary.MODE_LIBRARY_MODIFY,
                                "libId" to userLibId
                        )
                    }
                }
            }

            preference {
                title(R.string.lay_options_btn_newlib)
                onClick { startActivity<NewLibrary>() }
            }
            preference {
                title(R.string.lay_options_btn_installupdate)
                onClick { LibraryUpdateLoader.askForUpdate() }
            }
            preference {
                title(R.string.lay_options_btn_editsource)
                onClick { startActivity<SourceEdit>() }
            }
        }
        private fun createPreferencesOwnCertificates(categoryBuilder: CategoryBuilder) = categoryBuilder.apply {
            if (UserKeyStore.hasCertificates())
                for (cert in UserKeyStore) {
                    preference {
                        title(UserKeyStore.getFingerprint(cert))
                        summary(R.string.lay_options_btn_newcertificate_delete)
                        onClick {
                            showMessageYesNo(R.string.certificate_delete) {
                                UserKeyStore.removeUserCertificate(cert)
                                globalOptionsAndroid.additionalCertificatesBase64 = UserKeyStore.toArray()
                                globalOptionsAndroid.save()
                                currentActivity<Options>()?.updatePreferences()
                            }
                        }
                    }
                }

            preference {
                title(R.string.lay_options_btn_newcertificate)
                onClick {
                    val defaultServer: String? = VideLibriApp.errors.asSequence().map { e ->
                        if (e.kind == PendingExceptionKind.Internet && e.error.contains("https://")) {
                            val matcher = Pattern.compile("https://([^/]+)").matcher(e.error)
                            if (matcher.find()) matcher.group(1) else null
                        } else null
                    }.firstOrNull()

                    showInputDialog(R.string.certificate_download, default = defaultServer) { text ->
                        Thread(DownloadCertificate(text)).start()
                    }
                }
            }
        }


        override fun onResume() {
            super.onResume()
            updatePreferences()
        }


        internal fun updatePreferences() {
            val ctx = context ?: return
            categoryAccounts?.let { it.removeAll(); createPreferencesAccounts(CategoryBuilder(ctx, it)) }
            categoryOwnLibraries?.let { it.removeAll(); createPreferencesOwnLibraries(CategoryBuilder(ctx, it)) }
            categoryOwnCertificates?.let { it.removeAll(); createPreferencesOwnCertificates(CategoryBuilder(ctx, it)) }

        }
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

        internal fun showLendingOptionsInView(activity: Activity, binding: OptionsLendingsBinding) {
            binding.viewHistory.isChecked = globalOptionsAndroid.bookListDisplayOptions.showHistory

            val sortingKeys = activity.resources.getStringArray(R.array.sortable_properties)
            val sorting = globalOptionsAndroid.bookListDisplayOptions.sortingKey
            val grouping = globalOptionsAndroid.bookListDisplayOptions.groupingKey
            binding.sorting.setSelection(sorting, sortingKeys)
            binding.grouping.setSelection(grouping, sortingKeys)
        }

        internal fun putLendingOptionsFromView(activity: Activity, binding: OptionsLendingsBinding) {
            globalOptionsAndroid.bookListDisplayOptions.showHistory = binding.viewHistory.isChecked
            val sortingKeys = activity.resources.getStringArray(R.array.sortable_properties)
            sortingKeys.getOrNull(binding.sorting.selectedItemPosition)?.let { globalOptionsAndroid.bookListDisplayOptions.sortingKey = it }
            sortingKeys.getOrNull(binding.grouping.selectedItemPosition)?.let {  globalOptionsAndroid.bookListDisplayOptions.groupingKey = it }
            globalOptionsAndroid.save()
        }
    }

}
