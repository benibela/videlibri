package de.benibela.videlibri;

import android.content.Intent;
import com.actionbarsherlock.app.SherlockFragmentActivity;
import com.actionbarsherlock.view.Menu;
import com.actionbarsherlock.view.MenuItem;

public class VideLibriBaseFragmentActivity extends SherlockFragmentActivity{
    boolean loading;
    MenuItem loadingItem;

    void setLoading(boolean loading){
        this.loading = loading;
        if (loadingItem == null) return;
        loadingItem.setVisible(loading);
    }


    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
        VideLibriSuperBase.onCreateOptionsMenu(getSherlock(), menu);
        loadingItem = menu.findItem(R.id.loading);
        if (loadingItem != null) loadingItem.setVisible(loading);
        return super.onCreateOptionsMenu(menu);
    }

    @Override
    public boolean onPrepareOptionsMenu(Menu menu) {
        boolean x = super.onPrepareOptionsMenu(menu);    //To change body of overridden methods use File | Settings | File Templates.
        VideLibriSuperBase.onPrepareOptionsMenu(menu);
        loadingItem = menu.findItem(R.id.loading);
        if (loadingItem != null) loadingItem.setVisible(loading);
        return x;
    }

    public boolean onOptionsItemSelected(MenuItem item) {
        if (VideLibriSuperBase.onOptionsItemSelected(this, item)) return true;
        return super.onOptionsItemSelected(item);
    }


    @Override
    protected void onActivityResult(int requestCode, int resultCode, Intent data) {
        if (!VideLibriSuperBase.onActivityResult(this, requestCode, resultCode, data))
            super.onActivityResult(requestCode, resultCode, data);

    }

    public void showMessage(String message){ Util.showMessage(this, message, null); }
    public void showMessage(String message, MessageHandler handler){ Util.showMessage(this, message, handler); }
    public void showMessageYesNo(String message, MessageHandler handler){ Util.showMessageYesNo(this, message, handler); }

}
