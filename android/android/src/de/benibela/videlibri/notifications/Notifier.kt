package de.benibela.videlibri.notifications

import android.Manifest
import android.app.Activity
import android.app.NotificationChannel
import android.app.NotificationManager
import android.app.PendingIntent
import android.content.Context
import android.content.Intent
import android.content.pm.PackageManager
import android.os.Build
import androidx.core.app.ActivityCompat
import androidx.core.app.NotificationCompat
import androidx.core.content.ContextCompat
import de.benibela.videlibri.Accounts
import de.benibela.videlibri.R
import de.benibela.videlibri.VideLibriApp
import de.benibela.videlibri.activities.LendingList
import de.benibela.videlibri.isReal
import de.benibela.videlibri.jni.Bridge
import de.benibela.videlibri.jni.globalOptionsAndroid
import de.benibela.videlibri.jni.globalOptionsShared
import de.benibela.videlibri.jni.save
import de.benibela.videlibri.utils.notificationManager

object Notifier {
    private const val DUEDATE_CHANNEL = "duedate"
    private const val NOTIFICATION_ID = 8239238
    private const val OUTDATED_DATA_PERIOD_DAYS: Long = 7

    private fun skipableNotification(title: String, text: String): Boolean {
        val checkSkipPeriodic: Long = kotlin.math.min(1000L * 60 * 60 * 23, NotificationScheduling.DAILY_CHECK_PERIOD)
        val now = System.currentTimeMillis()
        val notifications = globalOptionsAndroid.notifications
        val canSkip = now - notifications.lastTime < checkSkipPeriodic &&
                      now >= notifications.lastTime &&
                      title == notifications.lastTitle &&
                      text == notifications.lastText

        if (!canSkip) {
            notifications.lastTime = now
            notifications.lastTitle = title
            notifications.lastText = text
            globalOptionsAndroid.save()
        }
        return canSkip
    }


    private fun getNotifications(context: Context): Array<String>? =
            if (Accounts.isEmpty())
                with(context) {
                    if (globalOptionsAndroid.hasBeenStartedAtLeastOnce && globalOptionsAndroid.accountCountBackup <= 0) null
                    else arrayOf(getString(R.string.notificationNoAccountsTitle), getString(R.string.notificationNoAccounts))
                }
            else {
                val n = Bridge.VLGetNotifications()
                if (n?.isNotEmpty() == true) n
                else {
                    Accounts.refreshAccounts()
                    if (Accounts.any { it.isReal && it.lastCheckDate > 0 && it.lastCheckDate <= Bridge.currentPascalDate - OUTDATED_DATA_PERIOD_DAYS })
                        with(context) { arrayOf(getString(R.string.notificationOutdatedDataTitle), getString(R.string.notificationOutdatedData)) }
                    else
                        null
                }
            }

    private fun cancelNotification(context: Context) {
        context.notificationManager?.cancel(NOTIFICATION_ID)
        skipableNotification("", "")
    }

    fun initChannels(context: Context) {
        if (Build.VERSION.SDK_INT < Build.VERSION_CODES.O)
            return
        val channel = NotificationChannel(DUEDATE_CHANNEL, context.getString(R.string.notifications_duedate_channel_name), NotificationManager.IMPORTANCE_DEFAULT)
        channel.description = context.getString(R.string.notifications_duedate_channel_description)
        channel.setShowBadge(true)
        context.notificationManager?.createNotificationChannel(channel)
    }


    /**
     * Show a notification while this service is running.
     * (partly from http://stackoverflow.com/questions/13902115/how-to-create-a-notification-with-notificationcompat-builder)
     */
    @JvmStatic fun updateNotification(activityContext: Context?) {
        val context = activityContext ?: VideLibriApp.currentContext() ?: return

        val notification = getNotifications(context) ?: arrayOf()

        if (notification.size < 2) {
            cancelNotification(context)
            return
        }

        if (skipableNotification(notification[0], notification[1]))
            return

        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.N &&
                context.notificationManager?.areNotificationsEnabled() == false)
            return

        initChannels(context)

        val pendingIntentFlags =
            if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.M) PendingIntent.FLAG_IMMUTABLE else 0

        val builder = NotificationCompat.Builder(context, DUEDATE_CHANNEL)
                .setSmallIcon(VideLibriApp.mainIcon)
                .setContentTitle(notification[0])
                .setContentText(notification[1])
                .setContentIntent(PendingIntent.getActivity(context, 0, Intent(context, LendingList::class.java), pendingIntentFlags))
                .setAutoCancel(true)

        // Set the info for the views that show in the notification panel.
        //notification.setLatestEventInfo(this, getText(R.string.local_service_label),text, contentIntent);

        // Send the notification.
        context.notificationManager?.notify(NOTIFICATION_ID, builder.build())
    }

}


const val PERMISSION_REQUEST_CODE_NOTIFICATIONS = 789
fun checkForRequiredNotificationPermission(context: Activity ){
   with (context) {
       if (!globalOptionsAndroid.notifications.enabled) return
       if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.TIRAMISU){
           if (ContextCompat.checkSelfPermission(this, Manifest.permission.POST_NOTIFICATIONS) != PackageManager.PERMISSION_GRANTED) {
               globalOptionsAndroid.notifications.lastAskedForPermission = Bridge.currentPascalDate
               globalOptionsAndroid.save()
               ActivityCompat.requestPermissions(this, arrayOf(Manifest.permission.POST_NOTIFICATIONS), PERMISSION_REQUEST_CODE_NOTIFICATIONS)
           }
       }
       Notifier.initChannels(context)
   }
}