/*
 * Notification Service for Android 2.2 to Android < 4 < 8
 */
package de.benibela.videlibri.notifications.service;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;

public class NotificationOnBootCompleted extends BroadcastReceiver {

    public static class LegacyPackageReplace extends BroadcastReceiver {
        @Override
        public void onReceive(Context context, Intent intent) {
            if (intent != null && intent.getData() != null && context.getPackageName().equals(intent.getData().getSchemeSpecificPart()))
            {
                NotificationService.resheduleDailyIfNecessary(context, true);
            }
        }
    }

    @Override
    public void onReceive(Context context, Intent intent) {
        NotificationService.resheduleDailyIfNecessary(context, true);
    }
}
