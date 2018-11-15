/*
 * Notification Service for Android 2.2 to Android < 4 < 8
 */
package de.benibela.videlibri.android2_2;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;


public class NotificationShowNow extends BroadcastReceiver {
    @Override
    public void onReceive(Context context, Intent intent) {
        context.startService(new Intent(context, NotificationService.class));
    }
}
