package de.benibela.videlibri;

import android.annotation.TargetApi;
import android.content.Context;
import android.support.v7.widget.SwitchCompat;
import android.util.AttributeSet;
import android.widget.Switch;

@TargetApi(14)
public class SwitchBoxSwitch extends SwitchCompat {
    public SwitchBoxSwitch(Context context) {
        super(context);
    }

    public SwitchBoxSwitch(Context context, AttributeSet attrs) {
        super(context, attrs);
    }

    public SwitchBoxSwitch(Context context, AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

 /*   @TargetApi(21)
    public SwitchBoxSwitch(Context context, AttributeSet attrs, int defStyleAttr, int defStyleRes) {
        super(context, attrs, defStyleAttr, defStyleRes);
    }*/

}
