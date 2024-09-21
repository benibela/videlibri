package de.benibela.videlibri.utils

import android.app.Activity
import android.content.Intent
import android.util.Log

const val ALL_ACTIVITY_RESULTS = 6740000
const val ALL_ACTIVITY_RESULTS_MAX = 6740000 + 9999
//const val ACTIVITY_RESULT_ID_KEY = "---internal-lambda-id"

internal typealias ActivityResultEvent = Activity.(Int, Intent?) -> Unit
internal typealias ActivityOKResultEvent = Activity.(Intent?) -> Unit

class ActivityLaunch(
    val id: Int,
    val callingClass: Class<out Any>,
    val handler: ActivityResultEvent,
)
class PendingActivityResult(
    val launch: ActivityLaunch,
    val resultCode: Int,
    val data: Intent?
)

var activityLaunchIds = ALL_ACTIVITY_RESULTS
var activityLaunches = mutableMapOf<Int, ActivityLaunch>()
var pendingActivityResults = mutableListOf<PendingActivityResult>()

fun Activity.pushActivityLaunch(handler: ActivityResultEvent): Int {
    activityLaunchIds++
    if (activityLaunchIds > ALL_ACTIVITY_RESULTS_MAX) activityLaunchIds = ALL_ACTIVITY_RESULTS
    val id = activityLaunchIds
    activityLaunches[id] = ActivityLaunch(id, this::class.java, handler)
    //val tempparams = params.toMutableList()
    //tempparams.add(0, Pair(ACTIVITY_RESULT_ID_KEY, id))
    return id
}

inline fun <reified T: Activity> Activity.startActivityForResult(vararg params: Pair<String, Any?>, noinline handler: ActivityResultEvent){
    val id = pushActivityLaunch(handler)
    internalStartActivityForResult(this, T::class.java, id, params)//tempparams.toTypedArray())
}
fun Activity.startActivityForResult(intent: Intent, handler: ActivityResultEvent){
    val id = pushActivityLaunch(handler)
    startActivityForResult(intent, id)
}

inline fun <reified T: Activity> Activity.startActivityForResultOk(vararg params: Pair<String, Any?>, noinline handler: ActivityOKResultEvent){
    startActivityForResult<T>(*params) { code, data -> if (code == Activity.RESULT_OK) {
        handler(this, data)
    } }
}


fun handleActivityResult ( requestCode: Int, resultCode: Int, data: Intent? ) {
    Log.i("VIDELIBRI", "handleActivityResult: $requestCode")
    if (requestCode < ALL_ACTIVITY_RESULTS || requestCode > ALL_ACTIVITY_RESULTS_MAX) return
    val launch = activityLaunches[requestCode] ?: return
    activityLaunches.remove(requestCode)
    pendingActivityResults.add(PendingActivityResult(launch, resultCode, data))
}
fun Activity.handlePendingActivityResults ( ) {
    Log.i("VIDELIBRI", "handlePendingActivityResults: ${pendingActivityResults.size}")
    val temp = pendingActivityResults.filter { it.launch.callingClass == this::class.java }
    Log.i("VIDELIBRI", "handlePendingActivityResults: temp: ${temp.size}")
    if (temp.isNotEmpty()) {
        pendingActivityResults = pendingActivityResults.filter { it.launch.callingClass != this::class.java }.toMutableList()
        temp.forEach { it.launch.handler(this, it.resultCode, it.data) }
    }
}




