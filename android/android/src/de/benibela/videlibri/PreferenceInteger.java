package de.benibela.videlibri;


import android.content.Context;


import android.content.res.TypedArray;
import android.support.v7.preference.DialogPreference;
import android.util.AttributeSet;

//see https://stackoverflow.com/questions/32621403/how-do-i-create-custom-preferences-using-android-support-v7-preference-library
public class PreferenceInteger extends DialogPreference {

    private int value;

    public PreferenceInteger(Context context, AttributeSet attrs, int defStyle) {
        super(context, attrs, defStyle);
    }

    public PreferenceInteger(Context context, AttributeSet attrs) {
        super(context, attrs);
    }

    public PreferenceInteger(Context context) {
        super(context);
    }

    public void setText(String text) {
        final boolean wasBlocking = shouldDisableDependents();

        value = Util.strToIntDef(text, 0);

        persistInt(value);

        final boolean isBlocking = shouldDisableDependents();
        if (isBlocking != wasBlocking) {
            notifyDependencyChange(isBlocking);
        }
    }

    public String getText() {
        return value + "";
    }

    @Override
    protected Object onGetDefaultValue(TypedArray a, int index) {
        return a.getInt(index, 0);
    }

    @Override
    protected void onSetInitialValue(boolean restoreValue, Object defaultValue) {
        int def = defaultValue instanceof  Integer ? (Integer)defaultValue
                : defaultValue instanceof String ? Util.strToIntDef((String)defaultValue, 0)
                : 0;
        setText((restoreValue ? getPersistedInt( def ) : def ) + "");
    }
}
