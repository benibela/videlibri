/*
 * Notification Service for Android 2.2 to Android < 4 < 8
 */
package de.benibela.videlibri.notifications;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;

public class NotificationOnBootCompleted extends BroadcastReceiver {
    @Override
    public void onReceive(Context context, Intent intent) {
        NotificationScheduling.rescheduleDailyIfNecessary(context, true);
    }
}
