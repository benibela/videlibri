package de.benibela.videlibri.notifications;

import android.app.NotificationChannel;
import android.app.NotificationManager;
import android.app.PendingIntent;
import android.content.Context;
import android.content.Intent;
import android.os.Build;
import android.support.v4.app.NotificationCompat;

import de.benibela.videlibri.Accounts;
import de.benibela.videlibri.LendingList;
import de.benibela.videlibri.R;
import de.benibela.videlibri.Util;
import de.benibela.videlibri.VideLibriApp;
import de.benibela.videlibri.jni.Bridge;

import static android.content.Context.NOTIFICATION_SERVICE;

public class Notifier {
    static private String lastNotificationTitle = "";
    static private String lastNotificationText = "";
    static private long lastNotificationTime = 0;
    static private final String DUEDATE_CHANNEL = "duedate";

    static private String [] getNotifications(Context context){
        if (Accounts.INSTANCE.size() == 0) return new String[] {
                Util.tr(context, R.string.notificationNoAccountsTitle),
                Util.tr(context, R.string.notificationNoAccounts)
        };

        String [] notification = Bridge.VLGetNotifications();

        if (notification == null) return null;

        return notification;
    }

    static private final int NOTIFICATION_ID = 8239238;
    static private void cancelNotification(Context context) {
        NotificationManager nm = ((NotificationManager)context.getSystemService(NOTIFICATION_SERVICE));
        if (nm == null) return;
        nm.cancel(NOTIFICATION_ID);
        lastNotificationTime = 0;
    }
    static private void initChannels(Context context){
        if (Build.VERSION.SDK_INT < Build.VERSION_CODES.O)
            return;
        NotificationManager nm = (NotificationManager) context.getSystemService(Context.NOTIFICATION_SERVICE);
        if (nm == null) return;
        NotificationChannel channel = new NotificationChannel( DUEDATE_CHANNEL, context.getString(R.string.notifications_duedate_channel_name), NotificationManager.IMPORTANCE_DEFAULT);
        channel.setDescription(context.getString(R.string.notifications_duedate_channel_description));
        channel.setShowBadge(true);
        nm.createNotificationChannel(channel);
    }

    /**
     * Show a notification while this service is running.
     * (partly from http://stackoverflow.com/questions/13902115/how-to-create-a-notification-with-notificationcompat-builder)
     */
    static public void updateNotification(Context context) {
        if (context == null) context = VideLibriApp.currentContext();
        if (context == null) return;

        String[] notification = getNotifications(context);

        if (notification == null || notification.length < 2) {
            cancelNotification(context);
            return;
        }

        if (lastNotificationTitle.equals(notification[0]) && lastNotificationText.equals(notification[1])
                && System.currentTimeMillis() - lastNotificationTime < 1000*60*60*23) return;

        initChannels(context);

        lastNotificationTitle = notification[0];
        lastNotificationText = notification[1];
        lastNotificationTime = System.currentTimeMillis();

        NotificationCompat.Builder mBuilder =
                new NotificationCompat.Builder(context, DUEDATE_CHANNEL)
                        .setSmallIcon(VideLibriApp.getMainIcon())
                        .setContentTitle(notification[0])
                        .setContentText(notification[1])
                        .setContentIntent(PendingIntent.getActivity(context, 0, new Intent(context, LendingList.class), 0))
                        .setAutoCancel(true)
                ;

        // Set the info for the views that show in the notification panel.
        //notification.setLatestEventInfo(this, getText(R.string.local_service_label),text, contentIntent);

        // Send the notification.
        NotificationManager nm = ((NotificationManager)context.getSystemService(NOTIFICATION_SERVICE));
        if (nm == null) return;
        nm.notify(NOTIFICATION_ID, mBuilder.build());
    }

}
