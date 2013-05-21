package de.benibela.videlibri;

import android.app.*;
import android.content.Context;
import android.content.Intent;
import android.net.ConnectivityManager;
import android.net.NetworkInfo;
import android.support.v4.app.NotificationCompat;
import android.os.Binder;
import android.os.IBinder;
import android.util.Log;

public class NotificationService extends Service {
    // Unique Identification Number for the Notification.
    // We use it on Notification start, and to cancel it.

    /**
     * Class for clients to access.  Because we know this service always
     * runs in the same process as its clients, we don't need to deal with
     * IPC.
     */
    public class LocalBinder extends Binder {
        NotificationService getService() {
            return NotificationService.this;
        }
    }

    static NotificationService instance;

    @Override
    public void onCreate() {
        super.onCreate();

        Bridge.initialize();
        instance = this;
    }

    @Override
    public int onStartCommand(Intent intent, int flags, int startId) {
        //Log.i("LocalService", "Received start id " + startId + ": " + intent);
        NetworkInfo network = ((ConnectivityManager) getSystemService(Context.CONNECTIVITY_SERVICE)).getActiveNetworkInfo();
        if (network != null && network.isConnected())
            VideLibri.updateAccount(null, true, false);
        else
            showLater(this, 1000*60*60); //wait 1 h

        showNotification(this);

        return START_NOT_STICKY;// START_STICKY;
    }

    @Override
    public void onDestroy() {
        // Cancel the persistent notification.
        //mNM.cancel(NOTIFICATION);
        instance = null;
    }

    @Override
    public IBinder onBind(Intent intent) {
        return mBinder;
    }

    // This is the object that receives interactions from clients.  See
    // RemoteService for a more complete example.
    private final IBinder mBinder = new LocalBinder();

    static private final int NOTIFICATION_ID = 8239238;
    static private String lastNotificationTitle = "";
    static private String lastNotificationText = "";
    /**
     * Show a notification while this service is running.
     * (partly from http://stackoverflow.com/questions/13902115/how-to-create-a-notification-with-notificationcompat-builder)
     */
    static void showNotification(Context context) {
        if (VideLibri.accounts == null) VideLibri.accounts = Bridge.VLGetAccounts();

        if (VideLibri.accounts.length == 0) return;

        String [] notification = Bridge.VLGetNotifications();

        if (notification == null) return;

        if (lastNotificationTitle.equals(notification[0]) && lastNotificationText.equals(notification[1])) return;

        lastNotificationTitle = notification[0];
        lastNotificationText = notification[1];

        NotificationCompat.Builder mBuilder =
                new NotificationCompat.Builder(context)
                        .setSmallIcon(R.drawable.icon)
                        .setContentTitle(notification[0])
                        .setContentText(notification[1])
                        .setContentIntent(PendingIntent.getActivity(context, 0, new Intent(context, VideLibri.class), 0));

        // Set the info for the views that show in the notification panel.
        //notification.setLatestEventInfo(this, getText(R.string.local_service_label),text, contentIntent);

        // Send the notification.
        ((NotificationManager)context.getSystemService(NOTIFICATION_SERVICE)).notify(NOTIFICATION_ID, mBuilder.build());
    }


    static final int SHOWNOW = 14355;
    static void showLater(Context context, int delayMs){
        PendingIntent intent = PendingIntent.getBroadcast(
                context, SHOWNOW,
                new Intent(context, NotificationShowNow.class),
                PendingIntent.FLAG_UPDATE_CURRENT);
        ((AlarmManager) context.getSystemService(Context.ALARM_SERVICE)).set(AlarmManager.RTC, System.currentTimeMillis() + delayMs, intent);
    }
}