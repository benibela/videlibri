package de.benibela.videlibri

import android.view.View
import androidx.annotation.StringRes

open class VideLibriFakeFragment internal constructor(var activity: VideLibriBaseActivity) {
    fun findViewById(id: Int): View = activity.findViewById(id)

    fun getString(@StringRes id: Int): String = activity.getString(id)
    fun getString(@StringRes id: Int, vararg args: Any?): String = activity.getString(id, *args)

}