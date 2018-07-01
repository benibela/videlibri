package de.benibela.videlibri;

import android.view.View;
import android.widget.Button;

public class VideLibriFakeFragment {
    VideLibriBaseActivity activity;

    VideLibriFakeFragment (VideLibriBaseActivity activity) {
        this.activity = activity;
    }

    void beginLoading(int loadingId){ activity.beginLoading(loadingId); }
    void endLoading(int loadingId){ activity.endLoading(loadingId); }
    void endLoadingAll(int loadingId){ activity.endLoadingAll(loadingId); }


    View findViewById(int id){
        return activity.findViewById(id);
    }
    Button findButtonById(int id){
        return (Button) (findViewById(id));
    }

    public String tr(int id){ return Util.tr(activity, id); }
    public String tr(int id, Object... args){ return Util.tr(activity, id, args); }

}
