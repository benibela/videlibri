package de.benibela.videlibri;
import android.annotation.SuppressLint;
import android.content.Context;
import android.content.DialogInterface;
import android.content.Intent;
import android.content.res.Configuration;
import android.net.Uri;
import android.os.Bundle;
import android.support.annotation.LayoutRes;
import android.support.annotation.NonNull;
import android.support.annotation.Nullable;
import android.support.design.widget.NavigationView;
import android.support.v4.view.GravityCompat;
import android.support.v4.widget.DrawerLayout;
import android.support.v7.app.ActionBar;
import android.support.v7.app.ActionBarDrawerToggle;
import android.support.v7.app.AppCompatActivity;
import android.support.v7.widget.Toolbar;
import android.view.LayoutInflater;
import android.view.MenuItem;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ArrayAdapter;
import android.widget.Button;
import android.widget.EditText;
import android.widget.ImageView;
import android.widget.Spinner;
import android.widget.TextView;

import de.benibela.videlibri.jni.Bridge;


@SuppressLint("Registered")
public class VideLibriBaseActivityOld extends AppCompatActivity{
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


    public ArrayAdapter<String> makeAdapterStrings(String[] templates) {
        ArrayAdapter<String> adapter = new ArrayAdapter<>(this, android.R.layout.simple_spinner_item, templates);
        adapter.setDropDownViewResource(android.R.layout.simple_spinner_dropdown_item);
        return adapter;
    }

    boolean onDialogResult(int dialogId, int buttonId, @Nullable Bundle more){
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
}
