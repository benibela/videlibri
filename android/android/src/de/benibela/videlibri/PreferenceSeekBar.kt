package de.benibela.videlibri


import android.content.Context
import android.graphics.Color
import android.support.v7.preference.PreferenceViewHolder
import android.support.v7.preference.SeekBarPreference
import android.util.AttributeSet
import android.widget.TextView
import android.widget.SeekBar

class PreferenceSeekBar @JvmOverloads constructor(context: Context, attrs: AttributeSet? = null, defStyleAttr: Int = android.support.v7.preference.R.attr.seekBarPreferenceStyle, defStyleRes: Int = 0) : SeekBarPreference(context, attrs, defStyleAttr, defStyleRes) {
    private val dynamicSummary: String?
    private var seekbarView: SeekBar? = null
    private var editable: Boolean = false

    init {

        val a = context.obtainStyledAttributes(attrs, R.styleable.PreferenceSeekBar, defStyleAttr, defStyleRes)

        dynamicSummary = a.getString(R.styleable.PreferenceSeekBar_dynamicSummary)
        a.recycle()
        showDynamicSummary()
    }

    override fun persistInt(value: Int): Boolean {
        val temp = super.persistInt(value)
        showDynamicSummary()
        return temp
    }

    override fun onBindViewHolder(view: PreferenceViewHolder) {

        super.onBindViewHolder(view)
        //workaround  for https://issuetracker.google.com/issues/37130859
        (view.findViewById(android.R.id.title) as TextView?)?.setTextColor(Color.WHITE)
        seekbarView = view.findViewById(android.support.v7.preference.R.id.seekbar) as SeekBar?
        seekbarView?.isEnabled = editable
    }

    override fun onClick() {
        super.onClick()
        editable = true
        seekbarView?.isEnabled = editable
        showDynamicSummary()
    }

    private fun showDynamicSummary() {
        val s = dynamicSummary?.let { String.format(it, value) } ?: ""
        //todo: plural https://stackoverflow.com/a/25648349
        summary = if (editable) s
                  else s + "   " +getString(R.string.lay_options_seekbar_tap_to_change);
    }
}
