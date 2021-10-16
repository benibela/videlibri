package de.benibela.videlibri

import android.annotation.SuppressLint
import android.os.Handler
import android.os.Message
import de.benibela.videlibri.activities.LibraryList
import de.benibela.videlibri.activities.Options
import de.benibela.videlibri.activities.VideLibriBaseActivity
import de.benibela.videlibri.activities.VideLibriBaseActivityOld
import de.benibela.videlibri.jni.Bridge
import de.benibela.videlibri.utils.*

object LibraryUpdateLoader{
    private var lastUrl: String = "https://"
    fun askForUpdate(){
        val context = VideLibriApp.currentContext() ?: return
        showInputDialog(context.getString(R.string.library_update_dialog_url), "Update", lastUrl) {
            url ->
            lastUrl = url
            if (!lastUrl.contains("://")) lastUrl = "https://$lastUrl"
            Bridge.VLInstallLibrary(lastUrl)
            currentActivity<VideLibriBaseActivity>()?.beginLoading(VideLibriBaseActivityOld.LOADING_INSTALL_LIBRARY)
            lastUrl = url
        }
    }

    fun registerInstallationDoneHandler() {
        Bridge.installationDoneHandler = @SuppressLint("HandlerLeak")
        object : Handler() {
            override fun handleMessage(msg: Message) {
                currentActivity<VideLibriBaseActivity>()?.endLoading(VideLibriBaseActivityOld.LOADING_INSTALL_LIBRARY)
                val status = msg.what
                if (status == 1) {
                    showDialog {
                        message(R.string.app_libregistered)
                        onDismiss = {
                            currentActivity<Options>()?.finish()
                            currentActivity<LibraryList>()?.onActivityResult(VideLibriBaseActivityOld.RETURNED_FROM_NEW_LIBRARY, 0, null)
                        }
                    }
                } else
                    showMessage(R.string.app_libregisterfailed)
            }
        }
    }
}