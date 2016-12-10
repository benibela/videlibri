package de.benibela.videlibri;

import android.content.Intent;
import android.os.Bundle;
import android.support.annotation.LayoutRes;
import android.support.v7.app.AppCompatActivity;
import android.view.Menu;
import android.view.MenuItem;

public class VideLibriBaseFragmentActivity extends AppCompatActivity implements Bridge.VideLibriContext{
    boolean loading;
    MenuItem loadingItem;

    void setLoading(boolean loading){
        this.loading = loading;
        if (loadingItem == null) return;
        loadingItem.setVisible(loading);
    }

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        Bridge.initialize(this);
    }

    @Override
    public void setContentView(@LayoutRes int layoutResID) {
        super.setContentView(layoutResID);
        VideLibriSuperBase.initActionBar(this);
    }

    @Override
    protected void onResume() {
        super.onResume();
        VideLibriApp.currentActivity = this;
    }

    @Override
    protected void onPause() {
        if (VideLibriApp.currentActivity == this) VideLibriApp.currentActivity = null;
        super.onPause();
    }

    @Override
    protected void onDestroy() {
        if (VideLibriApp.currentActivity == this) VideLibriApp.currentActivity = null;
        super.onDestroy();
    }

    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
        VideLibriSuperBase.onCreateOptionsMenu(this, menu);
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

    public String tr(int id){ return Util.tr(this, id); }
    public String tr(int id, Object... args){ return Util.tr(this, id, args); }

    public void showMessage(String message){ Util.showMessage(message, null); }
    public void showMessage(String message, MessageHandler handler){ Util.showMessage(message, handler); }
    public void showMessageYesNo(String message, MessageHandler handler){ Util.showMessageYesNo(message, handler); }


    @Override
    public String userPath() {
        return VideLibriSuperBase.userPath(this);
    }
}
