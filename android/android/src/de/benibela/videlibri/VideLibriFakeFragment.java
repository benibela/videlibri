package de.benibela.videlibri;

import android.content.res.Resources;
import android.view.View;
import android.widget.Button;

import android.support.v4.app.Fragment;

public class VideLibriFakeFragment {

    boolean loading;
    VideLibriBaseActivity activity;

    VideLibriFakeFragment (VideLibriBaseActivity activity) {
        this.activity = activity;
    }

    void setLoading(boolean loading){
        this.loading = loading;
        if (activity instanceof VideLibriBaseFragmentActivity)
            activity.setLoading(activity.loading);
    }


    View findViewById(int id){
        return activity.findViewById(id);
    }
    Button findButtonById(int id){
        return (Button) (findViewById(id));
    }

    public String tr(int id){ return Util.tr(activity, id); }
    public String tr(int id, Object... args){ return Util.tr(activity, id, args); }

}
