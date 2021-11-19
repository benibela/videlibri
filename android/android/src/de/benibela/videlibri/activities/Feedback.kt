package de.benibela.videlibri.activities


import android.content.ActivityNotFoundException
import android.content.Intent
import android.content.pm.PackageManager
import android.os.Build
import android.os.Bundle
import android.preference.PreferenceManager
import android.view.View
import android.widget.Button
import android.widget.CheckBox
import android.widget.EditText
import android.widget.TextView
import de.benibela.videlibri.R
import de.benibela.videlibri.VideLibriApp
import de.benibela.videlibri.jni.Bridge
import de.benibela.videlibri.utils.*
import org.acra.ACRA
import java.io.File
import java.io.FileInputStream


class Feedback : VideLibriBaseActivity() {

    internal val version: String
        get() {
            return try {
                packageManager.getPackageInfo("de.benibela.videlibri", 0).versionName
            } catch (e: PackageManager.NameNotFoundException) {
                "??"
            }

        }

    private val systemInfo: String
        get() = StringBuilder().run {
            try {
                append("Android: ${Build.VERSION.RELEASE}\n")
                append("Device: ${Build.MODEL} ${Build.FINGERPRINT}\n")
                val cpu = (if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.LOLLIPOP)
                    Build.SUPPORTED_ABIS.joinToString()
                else @Suppress("DEPRECATION")
                    "${Build.CPU_ABI}+${Build.CPU_ABI2}")
                append("CPU: $cpu\n")

                val cpuinfo = File("/proc/cpuinfo")
                if (cpuinfo.exists()) {
                    val map = mutableMapOf<String, MutableSet<String>>()
                    FileInputStream(cpuinfo).useLines { line ->
                        val key = line.substringBefore(":")
                        if (key == line) append("$line\n")
                        else map.getOrPut(key) { mutableSetOf() }.add(line.substringAfter(":"))
                    }
                    append(map.toList().joinToString("\n") { "${it.first}: ${it.second.joinToString(", ")}" })
                }
            } catch (ignored: Exception) {
                append("??")
            }

            return toString()
        }

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)    //To change body of overridden methods use File | Settings | File Templates.
        setVideLibriView(R.layout.feedback)
        title = getString(R.string.feedback_feedbacktitle)
        if (!PreferenceManager.getDefaultSharedPreferences(this).getBoolean(ACRA.PREF_ENABLE_SYSTEM_LOGS, true))
            findViewById<TextView>(R.id.feedbackACRAHeader).text = getString(R.string.feedback_acraheader)

        intent.getStringExtra("message")?.let {
            findViewById<EditText>(R.id.text).setText(it)
        }

        if (VideLibriApp.errors.size > 0) {
            for (a in arrayOf(arrayOf(R.id.feedbackIncludeErrorDetails, R.id.feedbackIncludeErrorAnonymousDetails, R.id.feedbackIncludeErrors),
                                         arrayOf(R.id.feedbackACRAIncludeErrorDetails, R.id.feedbackACRAIncludeErrorAnonymousDetails, R.id.feedbackACRAIncludeErrors))
                    ) {
                val details = findViewById<CheckBox>(a[0]).apply{
                    visibility = View.VISIBLE
                }
                val anonDetails = findViewById<CheckBox>(a[1]).apply {
                    visibility = View.VISIBLE
                    isChecked = true
                }
                findViewById<CheckBox>(a[2]).apply {
                    visibility = View.VISIBLE
                    isChecked = true
                    setOnCheckedChangeListener { _, b ->
                        details.isEnabled = b
                        details.isChecked = details.isChecked && b
                        anonDetails.isEnabled = b
                        anonDetails.isChecked = b
                    }
                }
            }
        }

        findViewById<Button>(R.id.button).setOnClickListener {
            val name = findViewById<TextView>(R.id.name).text
            val mail = findViewById<TextView>(R.id.mail).text
            val feedback = findViewById<TextView>(R.id.text).text
            val commonData = "Name: $name\nMail: $mail\n$feedback"
            val includeErrors = findViewById<CheckBox>(R.id.feedbackIncludeErrors).isChecked
            val details = includeErrors && findViewById<CheckBox>(R.id.feedbackIncludeErrorDetails).isChecked
            val anonymousDetails = includeErrors && findViewById<CheckBox>(R.id.feedbackIncludeErrorAnonymousDetails).isChecked
            val errCache = VideLibriApp.errors

            Thread(Runnable {

                val system = systemInfo
                val rep = if (errCache.size == 0) 1 else errCache.size
                var ok = 0
                var err = ""
                for (i in 0 until rep) { //send each error separately to avoid running out of memory
                    val postData = mutableListOf(commonData)
                    errCache.getOrNull(i)?.let { e ->
                        postData.add("Error $i: ${e.error} bei ${e.library}\n")
                        when {
                            details -> {
                                postData.add("errorDetails$i")
                                postData.add(e.details)
                            }
                            anonymousDetails -> { //including both often causse an out of memory exception
                                postData.add("errorAnonDetails$i")
                                postData.add(e.anonymousDetails)
                            }
                            else -> 0
                        }
                    }
                    postData.add("system: ")
                    postData.add(system)

                    try {
                        if (Bridge.VLSendFeedback(postData.toTypedArray()))
                            ok += 1
                    } catch (e: Exception) {
                        err = e.localizedMessage ?: e.message ?: "error"
                    }

                    System.gc()
                }

                runOnUiThread {
                    if (ok > 0) {
                        showDialog {
                            message(if (ok == rep) R.string.feedback_send_ok else R.string.feedback_send_failed)
                            okButton {
                                currentActivity<Feedback>()?.findViewById<View>(R.id.button)?.postDelayed({ finish() }, 100) //delayed to avoid  "android.view.WindowManager$BadTokenException: Unable to add window -- token android.os.BinderProxy@40b47bd8 is not valid" error.
                            }
                        }
                    } else
                        showMessage("${getString(R.string.feedback_send_failedconnect)}\n$err")
                }
            }).start()
        }

        findViewById<View>(R.id.textViewMail).setOnClickListener {
            val emailIntent = Intent(Intent.ACTION_SEND)
            emailIntent.action = Intent.ACTION_SEND
            emailIntent.type = "message/rfc822"
            emailIntent.putExtra(Intent.EXTRA_EMAIL, "benito@benibela.de")
            emailIntent.putExtra(Intent.EXTRA_SUBJECT, "VideLibri feedback $version")
            try {
                startActivity(emailIntent)
            } catch (e: ActivityNotFoundException) {
                showMessage(getString(R.string.error_nomailapp))
            }
        }

        findViewById<View>(R.id.acra).setOnClickListener {
            ACRA.getErrorReporter().apply {
                if (findViewById<CheckBox>(R.id.feedbackACRAIncludeErrors).isChecked) {
                    val details = findViewById<CheckBox>(R.id.feedbackACRAIncludeErrorDetails).isChecked
                    val anonymousDetails = findViewById<CheckBox>(R.id.feedbackACRAIncludeErrorAnonymousDetails).isChecked
                    VideLibriApp.errors.forEachIndexed { i, e ->
                        putCustomData("error$i", "Error: ${e.error} bei ${e.library}\n")
                        if (details)
                            putCustomData("errorDetails$i", e.details)
                        else if (anonymousDetails)
                            putCustomData("errorAnonDetails$i", e.anonymousDetails)
                    }
                }

                handleException(null)
            }
            finish()
        }
    }
}
