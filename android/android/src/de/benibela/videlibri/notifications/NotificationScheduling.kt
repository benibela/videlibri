package de.benibela.videlibri.notifications

import android.content.BroadcastReceiver
import android.content.Context
import android.content.Intent
import android.os.Build
import de.benibela.videlibri.jni.globalOptionsAndroid
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

    @JvmStatic fun preferenceNotificationsEnabled(): Boolean{
        return globalOptionsAndroid.notifications.enabled
    }
    @JvmStatic fun preferenceNotificationsBootDelayInMilliseconds(): Long{
        return 1000 * 60 * globalOptionsAndroid.notifications.serviceDelay.let { if (it <= 0) 15L else it.toLong() }
    }
    @JvmStatic fun onUpdateComplete(){
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O)
            NotificationJobService.finishAll()
    }
    //const val DAILY_CHECK_PERIOD: Long = 1000 * 60 * 16
    const val DAILY_CHECK_PERIOD: Long = 1000 * 60 * 60 * 24
}
