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










    //deprecated Util
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

    public ArrayAdapter<String> makeAdapterStrings(String[] templates) {
        ArrayAdapter<String> adapter = new ArrayAdapter<>(this, android.R.layout.simple_spinner_item, templates);
        adapter.setDropDownViewResource(android.R.layout.simple_spinner_dropdown_item);
        return adapter;
    }


    public String tr(int id){ return Util.tr(this, id); }
    public String tr(int id, Object... args){ return Util.tr(this, id, args); }




}
