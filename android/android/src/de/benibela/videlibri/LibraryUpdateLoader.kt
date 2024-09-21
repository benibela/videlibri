package de.benibela.videlibri

import android.os.Handler
import android.os.Looper
import android.os.Message
import de.benibela.videlibri.activities.LibraryList
import de.benibela.videlibri.activities.Options
import de.benibela.videlibri.activities.VideLibriBaseActivity
import de.benibela.videlibri.jni.Bridge
import de.benibela.videlibri.utils.*

object LibraryUpdateLoader{
    private var lastUrl: String = "https://"
    var isActive = false
    fun askForUpdate(){
        val context = VideLibriApp.currentContext() ?: return
        showInputDialog(context.getString(R.string.library_update_dialog_url), "Update", lastUrl) {
            url ->
            lastUrl = url
            if (!lastUrl.contains("://")) lastUrl = "https://$lastUrl"
            Bridge.VLInstallLibrary(lastUrl)
            lastUrl = url
            isActive = true
            currentActivity<VideLibriBaseActivity>()?.refreshLoadingIcon()
        }
    }

    fun registerInstallationDoneHandler() {
        Bridge.installationDoneHandler =
        object : Handler(Looper.getMainLooper()) {
            override fun handleMessage(msg: Message) {
                isActive = false
                currentActivity<VideLibriBaseActivity>()?.refreshLoadingIcon()
                val status = msg.what
                if (status == 1) {
                    showDialog {
                        message(R.string.app_libregistered)
                        onDismiss = {
                            currentActivity<Options>()?.finish()
                            currentActivity<LibraryList>()?.refreshLibraryList()
                        }
                    }
                } else {
                    showMessage(R.string.app_libregisterfailed)
                    VideLibriApp.showPendingExceptions()
                }
            }
        }
    }
}