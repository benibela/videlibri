package de.benibela.videlibri;

import android.app.*;
import android.content.Context;
import android.content.Intent;
import android.content.SharedPreferences;
import android.net.ConnectivityManager;
import android.net.Network;
import android.net.NetworkInfo;
import android.os.Build;
import android.preference.PreferenceManager;
import android.support.v4.app.NotificationCompat;
import android.os.Binder;
import android.os.IBinder;
import android.util.Log;

public class NotificationService extends Service implements Bridge.VideLibriContext{
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

        Bridge.initialize(this);
        instance = this;
    }

    private boolean isNetworkConnected(){
        try {
        ConnectivityManager cmanager = (ConnectivityManager) getSystemService(Context.CONNECTIVITY_SERVICE);
        if (cmanager == null) return false;
        {
        NetworkInfo networkInfo = cmanager.getActiveNetworkInfo();
        if (networkInfo != null && networkInfo.isAvailable() && networkInfo.isConnected())
            return true;
        }
        if (Build.VERSION.SDK_INT < 23) {
            NetworkInfo networkInfos[] = cmanager.getAllNetworkInfo();
            for (NetworkInfo networkInfo: networkInfos) {
                if (networkInfo != null && networkInfo.isAvailable() && networkInfo.isConnectedOrConnecting())
                    return true;
            }
        }
        if (Build.VERSION.SDK_INT >= 21){
            Network[] networks = cmanager.getAllNetworks();
            for (Network network: networks) {
                NetworkInfo info = cmanager.getNetworkInfo(network);
                if (info != null && info.isAvailable() && info.isConnectedOrConnecting())
                    return true;
            }
        }
        }catch (Exception e) { return false; } //there are a few reports about networkinfo crashing on stackoverflow
        return false;
    }

    @Override
    public int onStartCommand(Intent intent, int flags, int startId) {
        pendingNotificationService = false;
        //Log.i("LocalService", "Received start id " + startId + ": " + intent);
        SharedPreferences sp = PreferenceManager.getDefaultSharedPreferences(this);
        if (!sp.getBoolean("notifications", true)) return START_NOT_STICKY;



        if (isNetworkConnected()){
            VideLibriApp.updateAccount(null, true, false);
            showLater(this, 1000*60*60*24); //check every day
        } else
            showLater(this, 1000*60*60); //wait 1 h

        updateNotification(this);

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
    static private long lastNotificationTime = 0;

    static private String [] getNotifications(Context context){
        if (VideLibriApp.accounts == null) VideLibriApp.accounts = Bridge.VLGetAccounts();

        if (VideLibriApp.accounts.length == 0) return new String[] {
                Util.tr(context, R.string.notificationNoAccountsTitle),
                Util.tr(context, R.string.notificationNoAccounts)
        };

        String [] notification = Bridge.VLGetNotifications();

        if (notification == null) return null;

        return notification;
    }

    static void cancelNotification(Context context) {
        ((NotificationManager)context.getSystemService(NOTIFICATION_SERVICE)).cancel(NOTIFICATION_ID);
        lastNotificationTime = 0;
    }

    /**
     * Show a notification while this service is running.
     * (partly from http://stackoverflow.com/questions/13902115/how-to-create-a-notification-with-notificationcompat-builder)
     */
    static void updateNotification(Context context) {
        if (context == null) context = instance;
        if (context == null) context = VideLibriApp.currentActivity;
        if (context == null) return;

        String[] notification = getNotifications(context);

        if (notification == null || notification.length < 2) {
            cancelNotification(context);
            return;
        }

        if (lastNotificationTitle.equals(notification[0]) && lastNotificationText.equals(notification[1])
                && System.currentTimeMillis() - lastNotificationTime < 1000*60*60*23) return;

        lastNotificationTitle = notification[0];
        lastNotificationText = notification[1];
        lastNotificationTime = System.currentTimeMillis();

        NotificationCompat.Builder mBuilder =
                new NotificationCompat.Builder(context)
                        .setSmallIcon(VideLibriApp.getMainIcon())
                        .setContentTitle(notification[0])
                        .setContentText(notification[1])
                        .setContentIntent(PendingIntent.getActivity(context, 0, new Intent(context, VideLibri.class), 0));
        //we could setAutoCancel to remove it when the user clicks on it

        // Set the info for the views that show in the notification panel.
        //notification.setLatestEventInfo(this, getText(R.string.local_service_label),text, contentIntent);

        // Send the notification.
        ((NotificationManager)context.getSystemService(NOTIFICATION_SERVICE)).notify(NOTIFICATION_ID, mBuilder.build());
    }


    static final int SHOWNOW = 14355;
    static boolean pendingNotificationService = false;
    static void showLater(Context context, int delayMs){
        pendingNotificationService = true;
        PendingIntent intent = PendingIntent.getBroadcast(
                context, SHOWNOW,
                new Intent(context, NotificationShowNow.class),
                PendingIntent.FLAG_UPDATE_CURRENT);
        ((AlarmManager) context.getSystemService(Context.ALARM_SERVICE)).setInexactRepeating(AlarmManager.RTC, System.currentTimeMillis(), delayMs, intent);
    }

    static void startIfNecessary(Context context){
        if (pendingNotificationService) return;
        SharedPreferences sp = PreferenceManager.getDefaultSharedPreferences(context);
        if (!sp.getBoolean("notifications", true)) return;
        showLater(context, 1000 * 60 * sp.getInt("notificationsServiceDelay", 15)); //wait 15 min
    }

    @Override
    public String userPath() {
        return VideLibriApp.userPath(this);
    }
}