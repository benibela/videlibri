package de.benibela.videlibri.notifications

import android.content.BroadcastReceiver
import android.content.Context
import android.content.Intent
import android.os.Build
import android.preference.PreferenceManager
import de.benibela.videlibri.notifications.jobs.NotificationJobService
import de.benibela.videlibri.notifications.jobs.rescheduleDailyIfNecessaryAsJob
import de.benibela.videlibri.notifications.service.NotificationService

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

    @JvmStatic fun preferenceNotificationsEnabled(context: Context?): Boolean{
        val sp = PreferenceManager.getDefaultSharedPreferences(context)
        return sp.getBoolean("notifications", true)
    }
    @JvmStatic fun preferenceNotificationsBootDelayInMilliseconds(context: Context?): Long{
        val sp = PreferenceManager.getDefaultSharedPreferences(context)
        return 1000 * 60 * sp.getInt("notificationsServiceDelay", 15).toLong()
    }
    @JvmStatic fun onUpdateComplete(){
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O)
            NotificationJobService.finishAll();
    }
    //const val DAILY_CHECK_PERIOD: Long = 1000 * 60 * 16;
    const val DAILY_CHECK_PERIOD: Long = 1000 * 60 * 60 * 24;
}
