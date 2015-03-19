package de.benibela.videlibri;
import android.app.AlertDialog;
import android.content.Context;
import android.content.DialogInterface;
import android.content.Intent;
import android.net.Uri;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.app.Activity;
import android.os.Bundle;
import android.widget.*;

import com.actionbarsherlock.app.*;
import com.actionbarsherlock.view.Menu;
import com.actionbarsherlock.view.MenuItem;
import com.actionbarsherlock.view.MenuInflater;

public class VideLibriBaseActivity extends SherlockActivity implements Bridge.VideLibriContext {

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);    //To change body of overridden methods use File | Settings | File Templates.
        getSupportActionBar().setHomeButtonEnabled(true);
        Bridge.initialize(this);
    }

    @Override
    protected void onPostCreate(Bundle savedInstanceState) {
        super.onPostCreate(savedInstanceState);    //To change body of overridden methods use File | Settings | File Templates.
    }

    boolean loading = false;
    private MenuItem loadingItem;

    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
        VideLibriSuperBase.onCreateOptionsMenu(getSherlock(), menu);
        loadingItem = menu.findItem(R.id.loading);
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

    @Override
    protected void onResume() {
        super.onResume();    //To change body of overridden methods use File | Settings | File Templates.

        VideLibriApp.currentActivity = this;

        /*if(refreshing)
            refreshView = inflater.inflate(R.layout.actionbar_refresh_progress, null);
        else
           refreshView = inflater.inflate(R.layout.actionbar_refresh_button, null);*/


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

    void setLoading(boolean loading){
        this.loading = loading;
        if (loadingItem == null) return;
        loadingItem.setVisible(loading);
    }

    /*@Override
    public void setTitle(CharSequence title){
        super.setTitle(title.length() > 0 ? "VideLibri: "+title : "VideLibri");
    } */

    //Util
    String getStringExtraSafe(String id){
        String r = getIntent().getStringExtra(id);
        if (r == null) return "";
        return  r;
    }

    public Button findButtonById(int id){
        return (Button)findViewById(id);
    }



    public void setTextViewText(int id, CharSequence text){
        TextView tv = (TextView) findViewById(id);
        tv.setText(text);
    }

    public String getTextViewText(int id){
        TextView tv = (TextView) findViewById(id);
        return tv.getText().toString();
    }

    public void setEditTextText(int id, CharSequence text){
        EditText tv = (EditText) findViewById(id);
        tv.setText(text);
    }

    public String getEditTextText(int id){
        EditText tv = (EditText) findViewById(id);
        return tv.getText().toString();
    }

    public void setCheckBoxChecked(int id, boolean text){
        CheckBox tv = (CheckBox) findViewById(id);
        tv.setChecked(text);
    }

    public boolean getCheckBoxChecked(int id){
        CheckBox tv = (CheckBox) findViewById(id);
        return tv.isChecked();
    }

    public void setSpinnerSelection(int id, String[] items, String selection){
        for (int i = 0; i < items.length; i++)
            if (selection.equals(items[i])) {
                ((Spinner)findViewById(id)).setSelection(i);
                return;
            }
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
