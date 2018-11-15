/*
 * Notification Service for Android 2.2 to Android < 4 < 8
 */
package de.benibela.videlibri.notifications.service;

import android.app.*;
import android.content.Context;
import android.content.Intent;
import android.content.SharedPreferences;
import android.preference.PreferenceManager;
import android.os.Binder;
import android.os.IBinder;

import de.benibela.videlibri.VideLibriApp;
import de.benibela.videlibri.internet.VideLibriNetworkInfo;
import de.benibela.videlibri.jni.Bridge;
import de.benibela.videlibri.notifications.Notifier;

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



    @Override
    public int onStartCommand(Intent intent, int flags, int startId) {
        //Log.i("LocalService", "Received start id " + startId + ": " + intent);
        SharedPreferences sp = PreferenceManager.getDefaultSharedPreferences(this);
        if (!sp.getBoolean("notifications", true)) return START_NOT_STICKY;


        if (VideLibriNetworkInfo.isNetworkConnected(this)){
            VideLibriApp.updateAccount(null, true, false);
        } else
            sheduleQuickCheck(this, 1000*60*60); //wait 1 h

        Notifier.updateNotification(this);

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






    static private final int NOTIFICATION_CHECK_DAILY = 14355;
    static private final int NOTIFICATION_CHECK_SOON = 14356;


    static private PendingIntent getBroadcast(Context context, int requestCode) {
        return PendingIntent.getBroadcast(
                context, requestCode,
                new Intent(context, NotificationShowNow.class),
                PendingIntent.FLAG_UPDATE_CURRENT);
    }

    static private void sheduleDailyCheck(Context context){
        //if (getBroadcast(context, NOTIFICATION_CHECK_DAILY, PendingIntent.FLAG_NO_CREATE) != null)
        //    return; could check for existing pending, but said not to work with force stop http://luboganev.github.io/post/alarms-pending-intent/
        AlarmManager am = ((AlarmManager) context.getSystemService(Context.ALARM_SERVICE));
        if (am == null) return;
        am.setInexactRepeating(AlarmManager.RTC, System.currentTimeMillis() + AlarmManager.INTERVAL_DAY, AlarmManager.INTERVAL_DAY, getBroadcast(context, NOTIFICATION_CHECK_DAILY));
    }

    static private void sheduleQuickCheck(Context context, int delay) {
        AlarmManager am = ((AlarmManager) context.getSystemService(Context.ALARM_SERVICE));
        if (am == null) return;
        am.set(AlarmManager.RTC, System.currentTimeMillis() + delay, getBroadcast(context, NOTIFICATION_CHECK_SOON));
    }


    static public void resheduleDailyIfNecessary(Context context, boolean quickCheck){
        SharedPreferences sp = PreferenceManager.getDefaultSharedPreferences(context);
        if (!sp.getBoolean("notifications", true)) return;
        sheduleDailyCheck(context);
        if (quickCheck)
            sheduleQuickCheck(context, 1000 * 60 * sp.getInt("notificationsServiceDelay", 15));
    }

    @Override
    public String userPath() {
        return VideLibriApp.userPath(this);
    }
}