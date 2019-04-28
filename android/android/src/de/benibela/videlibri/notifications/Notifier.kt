package de.benibela.videlibri.notifications

import android.app.NotificationChannel
import android.app.NotificationManager
import android.app.PendingIntent
import android.content.Context
import android.content.Context.NOTIFICATION_SERVICE
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

    private fun skippableNotification(context: Context, title: String, text: String): Boolean {
        val CHECK_SKIP_PERIOD: Long = min(1000L * 60 * 60 * 23, NotificationScheduling.DAILY_CHECK_PERIOD)
        val now = System.currentTimeMillis()
        val sp = PreferenceManager.getDefaultSharedPreferences(context)
        val lastTime = sp.getLong("lastNotificationTime", 0)
        val canSkip = now - lastTime < CHECK_SKIP_PERIOD &&
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


    private fun getNotifications(context: Context): Array<String>? {
        return if (Accounts.size == 0) arrayOf(Util.tr(context, R.string.notificationNoAccountsTitle), Util.tr(context, R.string.notificationNoAccounts)) else Bridge.VLGetNotifications()
                ?: return null

    }

    private fun cancelNotification(context: Context) {
        val nm = context.getSystemService(NOTIFICATION_SERVICE) as NotificationManager? ?: return
        nm.cancel(NOTIFICATION_ID)
        skippableNotification(context, "", "")
    }

    private fun initChannels(context: Context) {
        if (Build.VERSION.SDK_INT < Build.VERSION_CODES.O)
            return
        val nm = context.getSystemService(Context.NOTIFICATION_SERVICE) as NotificationManager? ?: return
        val channel = NotificationChannel(DUEDATE_CHANNEL, context.getString(R.string.notifications_duedate_channel_name), NotificationManager.IMPORTANCE_DEFAULT)
        channel.description = context.getString(R.string.notifications_duedate_channel_description)
        channel.setShowBadge(true)
        nm.createNotificationChannel(channel)
    }


    /**
     * Show a notification while this service is running.
     * (partly from http://stackoverflow.com/questions/13902115/how-to-create-a-notification-with-notificationcompat-builder)
     */
    @JvmStatic fun updateNotification(context: Context?) {
        var context = context
        if (context == null) context = VideLibriApp.currentContext()
        if (context == null) return

        val notification = getNotifications(context)

        if (notification == null || notification.size < 2) {
            cancelNotification(context)
            return
        }

        if (skippableNotification(context, notification[0], notification[1]))
            return

        initChannels(context)

        val mBuilder = NotificationCompat.Builder(context, DUEDATE_CHANNEL)
                .setSmallIcon(VideLibriApp.mainIcon)
                .setContentTitle(notification[0])
                .setContentText(notification[1])
                .setContentIntent(PendingIntent.getActivity(context, 0, Intent(context, LendingList::class.java), 0))
                .setAutoCancel(true)

        // Set the info for the views that show in the notification panel.
        //notification.setLatestEventInfo(this, getText(R.string.local_service_label),text, contentIntent);

        // Send the notification.
        val nm = context.getSystemService(NOTIFICATION_SERVICE) as NotificationManager? ?: return
        nm.notify(NOTIFICATION_ID, mBuilder.build())
    }

}
