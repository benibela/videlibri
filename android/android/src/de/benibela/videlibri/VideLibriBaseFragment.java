package de.benibela.videlibri;

import android.view.View;
import android.widget.Button;
import com.actionbarsherlock.app.SherlockFragment;

public class VideLibriBaseFragment extends SherlockFragment {
    boolean loading;
    void setLoading(boolean loading){
        this.loading = loading;
        if (getSherlockActivity() instanceof VideLibriBaseFragmentActivity)
            ((VideLibriBaseFragmentActivity) getSherlockActivity()).setLoading(loading);
    }


    View findViewById(int id){
        View v = getView();
        if (v == null) return null;
        return v.findViewById(id);
    }
    Button findButtonById(int id){
        View v = getView();
        if (v == null) return null;
        return (Button) (v.findViewById(id));
    }


    public void showMessage(String message){ Util.showMessage(getSherlockActivity(), message, null); }
    public void showMessage(String message, MessageHandler handler){ Util.showMessage(getSherlockActivity(), message, handler); }
    public void showMessageYesNo(String message, MessageHandler handler){ Util.showMessageYesNo(getSherlockActivity(), message, handler); }

}
