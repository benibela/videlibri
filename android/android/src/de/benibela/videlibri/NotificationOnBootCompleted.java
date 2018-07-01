package de.benibela.videlibri;

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
