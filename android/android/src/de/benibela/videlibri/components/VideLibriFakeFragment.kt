package de.benibela.videlibri.components

import androidx.annotation.StringRes
import de.benibela.videlibri.activities.VideLibriBaseActivity

open class VideLibriFakeFragment internal constructor(var activity: VideLibriBaseActivity) {
    fun getString(@StringRes id: Int): String = activity.getString(id)
    fun getString(@StringRes id: Int, vararg args: Any?): String = activity.getString(id, *args)
}