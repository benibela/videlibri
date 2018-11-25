package de.benibela.videlibri;
import android.annotation.SuppressLint;
import android.content.Context;
import android.content.DialogInterface;
import android.content.Intent;
import android.content.res.Configuration;
import android.net.Uri;
import android.support.annotation.LayoutRes;
import android.support.annotation.NonNull;
import android.support.v4.view.GravityCompat;
import android.support.v4.view.MenuItemCompat;
import android.support.v4.widget.DrawerLayout;
import android.support.v7.app.ActionBar;
import android.support.v7.app.ActionBarDrawerToggle;
import android.support.v7.widget.Toolbar;
import android.view.*;
import android.app.Activity;
import android.os.Bundle;
import android.widget.*;

import android.support.v7.app.AppCompatActivity;
import android.support.design.widget.NavigationView;

import java.util.ArrayList;

import de.benibela.videlibri.jni.Bridge;


@SuppressLint("Registered")
public class VideLibriBaseActivityOld extends AppCompatActivity implements Bridge.VideLibriContext {
    protected static final int RETURNED_FROM_NEW_LIBRARY = 29326;

    static final int LOADING_ACCOUNT_UPDATE = 1;
    static final int LOADING_COVER_IMAGE = 100;
    static final int LOADING_SEARCH_CONNECTING = 200;
    static final int LOADING_SEARCH_SEARCHING = 201;
    static final int LOADING_SEARCH_DETAILS = 202;
    static final int LOADING_SEARCH_ORDER = 203;
    static final int LOADING_SEARCH_ORDER_HOLDING = 204;
    static final int LOADING_SEARCH_MESSAGE = 205;
    static final int LOADING_INSTALL_LIBRARY = 600;


    private DrawerLayout mDrawerLayout;
    private ActionBarDrawerToggle mDrawerToggle;


    @Override
    public void setContentView(@LayoutRes int layoutResID) {
        LayoutInflater inflater = (LayoutInflater) getSystemService( Context.LAYOUT_INFLATER_SERVICE );
        setContentView(inflater.inflate(layoutResID, null));
    }
    @Override
    public void setContentView(View view) {
        super.setContentView(view);
        // initActionBar
        Toolbar bar = (Toolbar) findViewById(R.id.actionbar);
        if (bar != null) {
            setSupportActionBar(bar);
            ActionBar sbar = getSupportActionBar();
            if (sbar != null) {
                sbar.setDisplayHomeAsUpEnabled(true);
                sbar.setHomeButtonEnabled(true);
            }
        }
        mDrawerLayout = (DrawerLayout) findViewById(R.id.drawer_layout);
        if (mDrawerLayout != null) {
            NavigationView navi = (NavigationView) findViewById(R.id.navigation);
            if (navi != null) {
                navi.inflateHeaderView(R.layout.naviheader);
                navi.setNavigationItemSelectedListener(new NavigationView.OnNavigationItemSelectedListener() {
                    @Override
                    public boolean onNavigationItemSelected(@NonNull MenuItem item) {
                        mDrawerLayout.closeDrawer(GravityCompat.START);
                        return onOptionsItemIdSelected(item.getItemId());
                    }
                });
            }
        }
    }

    public void setVideLibriView(@LayoutRes int layoutResID) {
        LayoutInflater inflater = (LayoutInflater) getSystemService( Context.LAYOUT_INFLATER_SERVICE );
        View layout = inflater.inflate(R.layout.videlibribaselayout, null);
        inflater.inflate(layoutResID, (ViewGroup) layout.findViewById(R.id.content_holder), true);
        setContentView(layout);
    }

    protected void createDrawerToggle(){
        if (mDrawerLayout != null && mDrawerToggle == null) {
            mDrawerToggle = new ActionBarDrawerToggle(
                    this,
                    mDrawerLayout,
                    R.string.drawer_open,
                    R.string.drawer_close
            );
            mDrawerLayout.addDrawerListener(mDrawerToggle);
        }
    }

    @Override
    protected void onPostCreate(Bundle savedInstanceState) {
        super.onPostCreate(savedInstanceState);    //To change body of overridden methods use File | Settings | File Templates.
        if (mDrawerToggle != null) mDrawerToggle.syncState();
    }

    @Override
    public void onConfigurationChanged(Configuration newConfig) {
        super.onConfigurationChanged(newConfig);
        if (mDrawerToggle != null) mDrawerToggle.onConfigurationChanged(newConfig);
    }

    protected void setItemVisible(MenuItem item, boolean visible){
        if (item == null) return;
        item.setVisible(visible);
    }


    public boolean onOptionsItemIdSelected(int id){
        return false;
    }
    public boolean onOptionsItemSelected(MenuItem item) {
        if (mDrawerToggle != null && mDrawerToggle.onOptionsItemSelected(item)) return true;
        if (onOptionsItemIdSelected(item.getItemId())) return true;
        return super.onOptionsItemSelected(item);
    }




    /*@Override
    public void setTitle(CharSequence title){
        super.setTitle(title.length() > 0 ? "VideLibri: "+title : "VideLibri");
    } */

    int currentMainIcon;
    public void checkMainIcon(){
        if (VideLibriApp.getMainIcon() != currentMainIcon) {
            /*ActionBar ab = getSupportActionBar();
            if (ab != null) {
                currentMainIcon = VideLibriApp.getMainIcon();
                ab.setHomeAsUpIndicator(currentMainIcon);
            }*/

            NavigationView navi = (NavigationView) findViewById(R.id.navigation);
            if (navi != null) {
                ImageView iv = ((ImageView) navi.getHeaderView(0).findViewById(R.id.icon));
                if (iv != null) iv.setImageResource(VideLibriApp.getMainIcon());
            }

        }
    }





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

    public void setCheckableChecked(int id, boolean text){
        Checkable tv = (Checkable) findViewById(id);
        tv.setChecked(text);
    }

    public boolean getCheckableChecked(int id){
        Checkable tv = (Checkable) findViewById(id);
        return tv.isChecked();
    }

    public void setSpinnerSelection(int id, String[] items, String selection){
        setSpinnerSelection((Spinner)findViewById(id), items, selection);
    }

    static public void setSpinnerSelection(Spinner v, String[] items, String selection){
        for (int i = 0; i < items.length; i++)
            if (selection.equals(items[i])) {
                v.setSelection(i);
                return;
            }
    }

    public ArrayAdapter<String> makeAdapterStrings(String[] templates) {
        ArrayAdapter<String> adapter = new ArrayAdapter<>(this, android.R.layout.simple_spinner_item, templates);
        adapter.setDropDownViewResource(android.R.layout.simple_spinner_dropdown_item);
        return adapter;
    }

    boolean onDialogResult(int dialogId, int buttonId, Bundle more){
        //callback
        //debug: Util.showMessage(1234, ""+dialogId+"=>"+buttonId,null,null,null);
        switch (dialogId) {
            case DialogId.RENEW_CONFIRM:
                if (buttonId == DialogInterface.BUTTON_POSITIVE) {
                    VideLibriApp.updateAccount(null, false, true);
                    if (this instanceof RenewList)
                        onBackPressed();
                }
                return true;
            case DialogId.ERROR_CONFIRM:
            case DialogId.ERROR_LOGIN:
            case DialogId.ERROR_INTERNET:
                if ( (dialogId == DialogId.ERROR_CONFIRM &&  buttonId == DialogInterface.BUTTON_POSITIVE) ||
                        (dialogId == DialogId.ERROR_LOGIN &&  buttonId == DialogInterface.BUTTON_NEGATIVE) ||
                        (dialogId == DialogId.ERROR_INTERNET &&  buttonId == DialogInterface.BUTTON_NEGATIVE)
                        ) {
                    Intent intent = new Intent(this, Feedback.class);
                    String queries = "";
                    for (Bridge.PendingException ex: VideLibriApp.errors)
                        if (ex.searchQuery != null && !"".equals(ex.searchQuery))
                            queries = queries + Util.tr(R.string.app_error_searchedfor) + ex.searchQuery+"\n";

                    final String message = Util.tr(R.string.app_error_anerror) + "\n"+ queries + Util.tr(R.string.app_error_needcontact);
                    intent.putExtra("message", message);
                    startActivity(intent);
                } else if (dialogId == DialogId.ERROR_LOGIN &&  buttonId == DialogInterface.BUTTON_POSITIVE) {
                    Intent intent = new Intent(this, AccountInfo.class);
                    intent.putExtra("mode", AccountInfo.MODE_ACCOUNT_MODIFY);
                    Bridge.Account acc = VideLibriApp.getAccount(more.getString("lib"), more.getString("user"));
                    if (acc != null) intent.putExtra("account", acc);
                    startActivity(intent);
                } else if (dialogId == DialogId.ERROR_INTERNET &&  buttonId == DialogInterface.BUTTON_POSITIVE) {
                    startActivityForResult(new Intent(android.provider.Settings.ACTION_WIRELESS_SETTINGS), 0);
                }
                return true;
            case DialogId.INSTALLATION_DONE:
                if (more != null && more.getInt("status") == 1 && this instanceof NewLibrary)
                    finish();
                return true;

        }
        return false;
    }


    public String tr(int id){ return Util.tr(this, id); }
    public String tr(int id, Object... args){ return Util.tr(this, id, args); }



    public void showUriInBrowser(String uri){
        try {
            startActivity(new Intent(Intent.ACTION_VIEW, Uri.parse(uri)));
        } catch (android.content.ActivityNotFoundException e) {
            Util.showMessage(tr(R.string.err_uri_open_failed, uri));
        }
    }


    @Override
    public String userPath() {
        return VideLibriApp.userPath(this);
    }
}
