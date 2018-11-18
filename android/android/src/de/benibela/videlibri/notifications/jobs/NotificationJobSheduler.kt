package de.benibela.videlibri.notifications.jobs

import android.app.job.JobInfo
import android.app.job.JobParameters
import android.app.job.JobScheduler
import android.app.job.JobService
import android.content.ComponentName
import android.content.Context
import android.os.Build
import android.os.Handler
import android.support.annotation.RequiresApi
import android.util.Log
import de.benibela.videlibri.VideLibriApp
import de.benibela.videlibri.notifications.NotificationScheduling
import de.benibela.videlibri.notifications.Notifier

@RequiresApi(Build.VERSION_CODES.LOLLIPOP)
class NotificationJobService: JobService() {
    override fun onStartJob(params: JobParameters?): Boolean {
        Log.d("VIDELIBRI JOB", "onStartJob!!");
        if (!NotificationScheduling.preferenceNotificationsEnabled(this))
            return false

        VideLibriApp.updateAccount(null, true, false)
        Notifier.updateNotification(this)

        if (VideLibriApp.runningUpdates.isEmpty())
            return false;

        Log.d("VIDELIBRI JOB", "onStartJob!! pending");

        instance = this;
        pendingParams.add(params)
        Handler().postDelayed({
            //timeout. not supposed to happen
            finishAll()
        }, 1000*60*15)
        return true
    }

    override fun onStopJob(params: JobParameters?): Boolean {
        Log.d("VIDELIBRI JOB", "onStopJob!!");
        pendingParams.remove(params)
        return false
    }

    override fun onDestroy() {
        super.onDestroy()
        instance = null;
    }

    companion object {
        var instance: NotificationJobService? = null;
        val pendingParams = mutableListOf<JobParameters?>()

        @RequiresApi(Build.VERSION_CODES.LOLLIPOP)
        fun finishAll() {
            Log.d("VIDELIBRI JOB", "finish all");
            pendingParams.forEach { instance?.jobFinished(it, false) }
            pendingParams.clear();
            instance = null;
        }
    }
}

private const val JOB_ID_DAILY = 123
private const val JOB_ID_AFTER_BOOT = 124

@RequiresApi(Build.VERSION_CODES.LOLLIPOP)
fun rescheduleDailyIfNecessaryAsJob(context: Context, afterDeviceBoot: Boolean){
    val scheduler  = context.getSystemService(Context.JOB_SCHEDULER_SERVICE) as JobScheduler;
    if (afterDeviceBoot) {
        val delay = NotificationScheduling.preferenceNotificationsBootDelayInMilliseconds(context)
        val b = JobInfo.Builder(JOB_ID_AFTER_BOOT, ComponentName(context, NotificationJobService::class.java))
        b.setRequiredNetworkType(JobInfo.NETWORK_TYPE_ANY);
        b.setMinimumLatency(delay);
        b.setOverrideDeadline(delay*2);
        scheduler.schedule(b.build());
    }

    for (j in scheduler.allPendingJobs)
        if (j.id == JOB_ID_DAILY)
            return

    val b = JobInfo.Builder(JOB_ID_DAILY, ComponentName(context, NotificationJobService::class.java))
    b.setRequiredNetworkType(JobInfo.NETWORK_TYPE_ANY)
    b.setPeriodic(NotificationScheduling.DAILY_CHECK_PERIOD)
    b.setPersisted(true)
    scheduler.schedule(b.build());
}

