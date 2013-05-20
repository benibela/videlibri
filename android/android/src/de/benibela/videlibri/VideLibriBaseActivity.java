package de.benibela.videlibri;
import android.app.AlertDialog;
import android.content.Context;
import android.content.DialogInterface;
import android.content.Intent;
import android.view.LayoutInflater;
import android.view.View;
import android.app.Activity;
import android.os.Bundle;
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

    MenuItem loadingItem = null;
    boolean loading = false;

    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
        MenuInflater inflater = getSupportMenuInflater();
        inflater.inflate(R.menu.videlibrimenu, menu);
        loadingItem = menu.findItem(R.id.loading);

        View refreshView;
        LayoutInflater linflater = (LayoutInflater)getSupportActionBar().getThemedContext().getSystemService(Context.LAYOUT_INFLATER_SERVICE);
        refreshView = linflater.inflate(R.layout.actionbar_loading, null);;
        loadingItem.setActionView(refreshView);
        loadingItem.setVisible(loading);


        return super.onCreateOptionsMenu(menu);
    }

    @Override
    protected void onResume() {
        super.onResume();    //To change body of overridden methods use File | Settings | File Templates.


        /*if(refreshing)
            refreshView = inflater.inflate(R.layout.actionbar_refresh_progress, null);
        else
           refreshView = inflater.inflate(R.layout.actionbar_refresh_button, null);*/


    }

    public boolean onOptionsItemSelected(MenuItem item) {
        // Handle item selection
        switch (item.getItemId()) {
            case R.id.search:
                VideLibri.newSearchActivity();
                return true;
            case R.id.accounts:
                Intent intent = new Intent(this, VideLibri.class);
                intent.addFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
                startActivity(intent);
                return true;
            case R.id.options:
                //todo
                return true;
            case  android.R.id.home:
                openOptionsMenu();
                return true;
            default:
                return super.onOptionsItemSelected(item);
        }
    }



    void setLoading(boolean loading){
        this.loading = loading;
        if (loadingItem == null) return;
        loadingItem.setVisible(loading);
    }

    @Override
    public void setTitle(CharSequence title){
        super.setTitle(title.length() > 0 ? "VideLibri: "+title : "VideLibri");
    }

    //Util
    String getStringExtraSafe(String id){
        String r = getIntent().getStringExtra(id);
        if (r == null) return "";
        return  r;
    }

    public void setTextViewText(int id, CharSequence text){
        TextView tv = (TextView) findViewById(id);
        tv.setText(text);
    }

    public String getTextViewText(int id){
        TextView tv = (TextView) findViewById(id);
        return tv.getText().toString();
    }

    static interface MessageHandler{
        void onDialogEnd(DialogInterface dialogInterface, int i);
    }
    static int MessageHandlerCanceled = -1;


    public void showMessage(String message){ showMessage(this, message, null); }
    public void showMessage(String message, MessageHandler handler){ showMessage(this, message, handler); }
    static public void showMessage(Context context, String message, final MessageHandler handler){
        AlertDialog.Builder builder = new AlertDialog.Builder(context);
        builder.setMessage(message);
        builder.setTitle("VideLibri");
        builder.setNegativeButton("OK", new DialogInterface.OnClickListener() {
            @Override
            public void onClick(DialogInterface dialogInterface, int i) {
                if (handler != null) handler.onDialogEnd(dialogInterface, i);
            }
        });
        if (handler != null) {
            builder.setOnCancelListener(new DialogInterface.OnCancelListener() {
                @Override
                public void onCancel(DialogInterface dialogInterface) {
                    handler.onDialogEnd(dialogInterface, MessageHandlerCanceled);
                }
            });
        }
        builder.show();
    }
}
