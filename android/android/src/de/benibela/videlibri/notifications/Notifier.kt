package de.benibela.videlibri.notifications

import android.app.NotificationChannel
import android.app.NotificationManager
import android.app.PendingIntent
import android.content.Context
import android.content.Intent
import android.os.Build
import android.preference.PreferenceManager
import android.support.v4.app.NotificationCompat
import de.benibela.videlibri.*
import de.benibela.videlibri.jni.Bridge
import java.lang.Math.min

object Notifier {
    private const val DUEDATE_CHANNEL = "duedate"
    private const val NOTIFICATION_ID = 8239238

    private fun skipableNotification(context: Context, title: String, text: String): Boolean {
        val checkSkipPeriodic: Long = min(1000L * 60 * 60 * 23, NotificationScheduling.DAILY_CHECK_PERIOD)
        val now = System.currentTimeMillis()
        val sp = context.preferences
        val lastTime = sp.getLong("lastNotificationTime", 0)
        val canSkip = now - lastTime < checkSkipPeriodic &&
                      now >= lastTime &&
                      title == sp.getString("lastNotificationTitle", "") &&
                      text == sp.getString("lastNotificationText", "")

        if (!canSkip) {
            sp.edit().let { e ->
                e.putLong("lastNotificationTime", now)
                e.putString("lastNotificationTitle", title)
                e.putString("lastNotificationText", text)
                e.apply()
            }
        }
        return canSkip
    }


    private fun getNotifications(context: Context): Array<String>? =
            if (Accounts.size == 0) with(context) { arrayOf(getString(R.string.notificationNoAccountsTitle), getString(R.string.notificationNoAccounts)) }
            else Bridge.VLGetNotifications()

    private fun cancelNotification(context: Context) {
        context.notificationManager?.cancel(NOTIFICATION_ID)
        skipableNotification(context, "", "")
    }

    private fun initChannels(context: Context) {
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

        if (skipableNotification(context, notification[0], notification[1]))
            return

        initChannels(context)

        val builder = NotificationCompat.Builder(context, DUEDATE_CHANNEL)
                .setSmallIcon(VideLibriApp.mainIcon)
                .setContentTitle(notification[0])
                .setContentText(notification[1])
                .setContentIntent(PendingIntent.getActivity(context, 0, Intent(context, LendingList::class.java), 0))
                .setAutoCancel(true)

        // Set the info for the views that show in the notification panel.
        //notification.setLatestEventInfo(this, getText(R.string.local_service_label),text, contentIntent);

        // Send the notification.
        context.notificationManager?.notify(NOTIFICATION_ID, builder.build())
    }

}
