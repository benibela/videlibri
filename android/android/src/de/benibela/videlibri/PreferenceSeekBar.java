package de.benibela.videlibri;


import android.content.Context;
import android.content.res.TypedArray;
import android.graphics.Color;
import android.support.v7.preference.PreferenceViewHolder;
import android.support.v7.preference.SeekBarPreference;
import android.util.AttributeSet;
import android.util.StringBuilderPrinter;
import android.widget.TextView;

/**
 * Created by theo on 30.12.17.
 */

public class PreferenceSeekBar extends SeekBarPreference {
    private String dynamicSummary;
    public PreferenceSeekBar(Context context, AttributeSet attrs, int defStyleAttr, int defStyleRes) {
        super(context, attrs, defStyleAttr, defStyleRes);

        TypedArray a = context.obtainStyledAttributes(attrs, R.styleable.PreferenceSeekBar, defStyleAttr, defStyleRes);

        dynamicSummary = a.getString(R.styleable.PreferenceSeekBar_dynamicSummary);
        a.recycle();
        showDynamicSummary();
    }

    public PreferenceSeekBar(Context context, AttributeSet attrs, int defStyleAttr) {
        this(context, attrs, defStyleAttr, 0);
    }

    public PreferenceSeekBar(Context context, AttributeSet attrs) {
        this(context, attrs, android.support.v7.preference.R.attr.seekBarPreferenceStyle);
    }

    public PreferenceSeekBar(Context context) {
        this(context, null);
    }

    @Override
    protected boolean persistInt(int value) {
        boolean temp = super.persistInt(value);
        showDynamicSummary();
        return temp;
    }

    @Override
    public void onBindViewHolder(PreferenceViewHolder view) {

        super.onBindViewHolder(view);
        //workaround  for https://issuetracker.google.com/issues/37130859
        final TextView titleView = (TextView) view.findViewById(android.R.id.title);
        if (titleView != null) {
            titleView.setTextColor(Color.WHITE);
        }
    }

    private void showDynamicSummary() {
        if (dynamicSummary != null) {
            //todo: plural https://stackoverflow.com/a/25648349
            setSummary(String.format(dynamicSummary, getValue()));
        }
    }
}
