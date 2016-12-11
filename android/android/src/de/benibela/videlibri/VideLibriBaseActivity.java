package de.benibela.videlibri;
import android.app.AlertDialog;
import android.content.Context;
import android.content.DialogInterface;
import android.content.Intent;
import android.net.Uri;
import android.support.annotation.LayoutRes;
import android.support.v4.view.MenuItemCompat;
import android.support.v7.app.ActionBar;
import android.support.v7.widget.Toolbar;
import android.util.Log;
import android.view.*;
import android.app.Activity;
import android.os.Bundle;
import android.widget.*;

import android.support.v7.app.AppCompatActivity;

 

public class VideLibriBaseActivity extends AppCompatActivity implements Bridge.VideLibriContext {
    private static final int REQUESTED_LIBRARY_HOMEPAGE  = 29324;
    private static final int REQUESTED_LIBRARY_CATALOGUE = 29325;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);    //To change body of overridden methods use File | Settings | File Templates.
        Bridge.initialize(this);
    }

    @Override
    public void setContentView(@LayoutRes int layoutResID) {
        super.setContentView(layoutResID);
        // initActionBar
        Toolbar bar = (Toolbar) findViewById(R.id.actionbar);
        if (bar == null) return;
        setSupportActionBar(bar);
        ActionBar sbar = getSupportActionBar();
        sbar.setDisplayHomeAsUpEnabled(true);
        sbar.setHomeAsUpIndicator(R.drawable.icon);
    }

    @Override
    protected void onPostCreate(Bundle savedInstanceState) {
        super.onPostCreate(savedInstanceState);    //To change body of overridden methods use File | Settings | File Templates.
    }

    boolean loading = false;
    private MenuItem loadingItem, renewAllItem, renewListItem, refreshAllItem, exportItem;

    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
        MenuInflater inflater = getMenuInflater();
        inflater.inflate(R.menu.videlibrimenu, menu);
        loadingItem = menu.findItem(R.id.loading);
        if (loadingItem != null)
            MenuItemCompat.setActionView(loadingItem, R.layout.actionbar_loading);
        renewAllItem = menu.findItem(R.id.renew);
        renewListItem = menu.findItem(R.id.renewlist);
        refreshAllItem = menu.findItem(R.id.refresh);
        exportItem = menu.findItem(R.id.export);
        return super.onCreateOptionsMenu(menu);
    }

    @Override
    public boolean onPrepareOptionsMenu(Menu menu) {
        boolean x = super.onPrepareOptionsMenu(menu);
        loadingItem = menu.findItem(R.id.loading);
        if (loadingItem != null) loadingItem.setVisible(loading);
        setSubMenuVisibility();
        return x;
    }

    private void setSubMenuVisibility(){
        boolean hasAccounts = VideLibriApp.accounts.length > 0;
        if (renewAllItem != null) renewAllItem.setVisible(hasAccounts);
        if (renewListItem != null) renewListItem.setVisible(hasAccounts);
        if (exportItem != null) exportItem.setVisible(hasAccounts);
        if (refreshAllItem != null) refreshAllItem.setVisible(VideLibriApp.runningUpdates.isEmpty() && hasAccounts);
    }

    public boolean onOptionsItemIdSelectedOld(final Activity context, int id) {
        // Handle item selection
        Intent intent;
        switch (id) {
            case R.id.search:
                VideLibriApp.newSearchActivity();
                return true;
            case  android.R.id.home:
//                context.openOptionsMenu();
                //              return true;
//            case R.id.accounts:
                intent = new Intent(context, VideLibri.class);
                intent.addFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
                context.startActivity(intent);
                return true;
            case R.id.more:
                setSubMenuVisibility();
                return false; //should open the normal sub menu
            case R.id.options:
                intent = new Intent(context, Options.class);
                context.startActivity(intent);
                return true;
            case R.id.refresh:
                VideLibriApp.updateAccount(null, false, false);
                return true;
            case R.id.renew:
                Util.showMessageYesNo(Util.tr(R.string.base_renewallconfirm), new MessageHandler() {
                    @Override
                    public void onDialogEnd(DialogInterface dialogInterface, int i) {
                        if (i == DialogInterface.BUTTON_POSITIVE) {
                            VideLibriApp.updateAccount(null, false, true);
                            if (VideLibriApp.currentActivity instanceof RenewList)
                                context.onBackPressed();
                        }
                    }
                });
                return true;
            case R.id.renewlist:
                context.startActivity(new Intent(context, RenewList.class));
                return true;
            case R.id.import_:
                intent = new Intent(context, ImportExport.class);
                intent.putExtra("mode", ImportExport.MODE_IMPORT);
                context.startActivity(intent);
                return true;
            case R.id.export:
                intent = new Intent(context, ImportExport.class);
                intent.putExtra("mode", ImportExport.MODE_EXPORT);
                context.startActivity(intent);
                return true;
            case R.id.libinfo:
                intent = new Intent(context, LibraryList.class);
                intent.putExtra("reason", Util.tr(R.string.base_chooselibhomepage));
                intent.putExtra("search", true);
                context.startActivityForResult(intent, REQUESTED_LIBRARY_HOMEPAGE);
                return true;
            case R.id.libcatalogue:
                intent = new Intent(context, LibraryList.class);
                intent.putExtra("reason", Util.tr(R.string.base_chooselibcat));
                intent.putExtra("search", true);
                context.startActivityForResult(intent, REQUESTED_LIBRARY_CATALOGUE);
                return true;
            case R.id.feedback:
                intent = new Intent(context, Feedback.class);
                context.startActivity(intent);
                return true;
            case R.id.debuglog:
                intent = new Intent(context, DebugLogViewer.class);
                context.startActivity(intent);
                return true;
            case R.id.about:
                intent = new Intent(context, About.class);
                context.startActivity(intent);
                return true;
            //case R.id.others:
            //    displayOthersMenu(context);
        }
        return false;
    }
    public boolean onOptionsItemSelected(MenuItem item) {
        if (onOptionsItemIdSelectedOld(this, item.getItemId())) return true;
        return super.onOptionsItemSelected(item);
    }

    @Override
    protected void onActivityResult(int requestCode, int resultCode, Intent data) {
        if ((requestCode == REQUESTED_LIBRARY_CATALOGUE || requestCode == REQUESTED_LIBRARY_HOMEPAGE) && resultCode == LibraryList.RESULT_OK) {
           Bridge.LibraryDetails details = Bridge.VLGetLibraryDetails(LibraryList.lastSelectedLibId);
           startActivity(new Intent(Intent.ACTION_VIEW, Uri.parse(
              requestCode == REQUESTED_LIBRARY_HOMEPAGE ? details.homepageBase : details.homepageCatalogue
           )));
           return;
        }

        super.onActivityResult(requestCode, resultCode, data);
    }

    @Override
    protected void onResume() {
        super.onResume();

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
        return VideLibriApp.userPath(this);
    }
}
