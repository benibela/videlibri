package de.benibela.videlibri

import androidx.annotation.StringRes

open class VideLibriFakeFragment internal constructor(var activity: VideLibriBaseActivity) {
    fun getString(@StringRes id: Int): String = activity.getString(id)
    fun getString(@StringRes id: Int, vararg args: Any?): String = activity.getString(id, *args)
}