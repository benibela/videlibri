package de.benibela.videlibri.notifications

import android.content.Context
import android.preference.PreferenceManager
import java.lang.Math.min


fun skippableNotification(context: Context, title: String, text: String): Boolean {
    val CHECK_SKIP_PERIOD: Long = min(1000L * 60 * 60 * 23, NotificationScheduling.DAILY_CHECK_PERIOD)
    val now = System.currentTimeMillis()
    val sp = PreferenceManager.getDefaultSharedPreferences(context)
    val lastTime = sp.getLong("lastNotificationTime", 0)
    val canSkip = now - lastTime < CHECK_SKIP_PERIOD &&
                  now >= lastTime &&
                  title == sp.getString("lastNotificationTitle", "") &&
                  text == sp.getString("lastNotificationText", "")

    if (!canSkip) {
        sp.edit().let { e ->
            e.putLong("lastNotificationTime", now)
            e.putString("lastNotificationTitle", title)
            e.putString("lastNotificationText", text)
            e.apply()
        }
    }
    return canSkip
}
