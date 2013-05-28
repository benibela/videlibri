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
import android.widget.Button;
import android.widget.CheckBox;
import android.widget.EditText;
import android.widget.TextView;

import com.actionbarsherlock.app.*;
import com.actionbarsherlock.view.Menu;
import com.actionbarsherlock.view.MenuItem;
import com.actionbarsherlock.view.MenuInflater;

public class VideLibriBaseActivity extends SherlockActivity {

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);    //To change body of overridden methods use File | Settings | File Templates.
        getSupportActionBar().setHomeButtonEnabled(true);

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


        /*if(refreshing)
            refreshView = inflater.inflate(R.layout.actionbar_refresh_progress, null);
        else
           refreshView = inflater.inflate(R.layout.actionbar_refresh_button, null);*/


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




    public void showMessage(String message){ Util.showMessage(this, message, null); }
    public void showMessage(String message, MessageHandler handler){ Util.showMessage(this, message, handler); }
    public void showMessageYesNo(String message, MessageHandler handler){ Util.showMessageYesNo(this, message, handler); }


}
