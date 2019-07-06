/*
 * Notification Service for Android 2.2 to Android < 4 < 8
 */
package de.benibela.videlibri.notifications.service;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.os.DeadObjectException;


public class NotificationShowNow extends BroadcastReceiver {
    @Override
    public void onReceive(Context context, Intent intent) {
        try {
            context.startService(new Intent(context, NotificationService.class));
        } catch (RuntimeException e) {
            //retry. sometimes starting fails randomly? or use bindService? https://stackoverflow.com/questions/40994627/
            e.printStackTrace();
            try {
                Thread.sleep(200);
            } catch (InterruptedException ignore) {

            }
            context.startService(new Intent(context, NotificationService.class));
        }

    }
}
