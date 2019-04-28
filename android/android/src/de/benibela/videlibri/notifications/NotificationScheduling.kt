package de.benibela.videlibri.notifications

import android.content.BroadcastReceiver
import android.content.Context
import android.content.Intent
import android.os.Build
import android.preference.PreferenceManager
import de.benibela.videlibri.notifications.jobs.NotificationJobService
import de.benibela.videlibri.notifications.jobs.rescheduleDailyIfNecessaryAsJob
import de.benibela.videlibri.notifications.service.NotificationService
import de.benibela.videlibri.preferences

class NotificationOnBootCompleted : BroadcastReceiver() {
    override fun onReceive(context: Context, intent: Intent?) {
        NotificationScheduling.rescheduleDailyIfNecessary(context, true)
    }
}


object NotificationScheduling{
    @JvmStatic fun rescheduleDailyIfNecessary(context: Context, afterDeviceBoot: Boolean){
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O)
            rescheduleDailyIfNecessaryAsJob(context, afterDeviceBoot)
        else
            NotificationService.resheduleDailyIfNecessary(context, afterDeviceBoot)
    }

    @JvmStatic fun preferenceNotificationsEnabled(context: Context): Boolean{
        return context.preferences.getBoolean("notifications", true)
    }
    @JvmStatic fun preferenceNotificationsBootDelayInMilliseconds(context: Context): Long{
        return 1000 * 60 * context.preferences.getInt("notificationsServiceDelay", 15).toLong()
    }
    @JvmStatic fun onUpdateComplete(){
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O)
            NotificationJobService.finishAll();
    }
    //const val DAILY_CHECK_PERIOD: Long = 1000 * 60 * 16;
    const val DAILY_CHECK_PERIOD: Long = 1000 * 60 * 60 * 24;
}
