package de.benibela.videlibri.activities;
import android.annotation.SuppressLint;

import androidx.appcompat.app.AppCompatActivity;

import android.widget.ArrayAdapter;
import android.widget.Button;
import android.widget.TextView;

import de.benibela.videlibri.utils.Util;


@SuppressLint("Registered")
public class VideLibriBaseActivityOld extends AppCompatActivity{
    static final public int RETURNED_FROM_NEW_LIBRARY = 29326;

    static final public int LOADING_ACCOUNT_UPDATE = 1;
    static final public int LOADING_SEARCH_CONNECTING = 200;
    static final public int LOADING_SEARCH_SEARCHING = 201;
    static final public int LOADING_SEARCH_DETAILS = 202;
    static final public int LOADING_SEARCH_ORDER = 203;
    static final public int LOADING_SEARCH_ORDER_HOLDING = 204;
    static final public int LOADING_SEARCH_MESSAGE = 205;
    static final public int LOADING_INSTALL_LIBRARY = 600;










    //deprecated Util
    public Button findButtonById(int id){
        return (Button)findViewById(id);
    }



    public void setTextViewText(int id, CharSequence text){
        TextView tv = findViewById(id);
        tv.setText(text);
    }

    public String getTextViewText(int id){
        TextView tv = findViewById(id);
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
