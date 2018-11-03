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
public class VideLibriBaseActivity extends AppCompatActivity implements Bridge.VideLibriContext {
    private static final int REQUESTED_LIBRARY_HOMEPAGE  = 29324;
    private static final int REQUESTED_LIBRARY_CATALOGUE = 29325;
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

    private ArrayList<Integer> loadingTasks;

    private DrawerLayout mDrawerLayout;
    private ActionBarDrawerToggle mDrawerToggle;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);    //To change body of overridden methods use File | Settings | File Templates.
        Bridge.initialize(this);
        if (savedInstanceState != null) loadingTasks = savedInstanceState.getIntegerArrayList("activeLoadingTasks");
        if (loadingTasks == null) loadingTasks = new ArrayList<>();
    }

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

    final static int ACTIONBAR_MENU_REFRESH = 0x1;
    final static int ACTIONBAR_MENU_RENEW_LIST = 0x2;
    final static int ACTIONBAR_MENU_RENEW_ALL = 0x4;
    final static int ACTIONBAR_MENU_SHARE = 0x8;
    final static int ACTIONBAR_MENU_NEWLIB = 0x10;
    final static int ACTIONBAR_MENU_FILTER = 0x20;

    private MenuItem loadingItem;//, moreItem;

    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
        MenuInflater inflater = getMenuInflater();
        inflater.inflate(R.menu.videlibrimenu, menu);
        loadingItem = menu.findItem(R.id.loading);
        if (loadingItem != null) {
            MenuItemCompat.setActionView(loadingItem, R.layout.actionbar_loading);
            View actionView = MenuItemCompat.getActionView(loadingItem);
            if (actionView != null) actionView.setOnClickListener(new View.OnClickListener() {
                @Override
                public void onClick(View v) {
                    showLoadingInfo();
                }
            });
            loadingItem.setVisible(loadingTasks.size() > 0);
        }
        //moreItem = menu.findItem(R.id.more);
        return super.onCreateOptionsMenu(menu);
    }

    protected void setItemVisible(MenuItem item, boolean visible){
        if (item == null) return;
        item.setVisible(visible);
    }
    protected int onPrepareOptionsMenuVisibility(){
        return 0;
    }
    private void setOptionMenuVisibility(Menu menu){
        if (menu == null) return;
        int visibility = onPrepareOptionsMenuVisibility();
        if (visibility != currentVisibility) {
            currentVisibility = visibility;
            //moreItem.setVisible(visibility != 0);
            //Menu menu = moreItem.getSubMenu();
            setItemVisible(menu.findItem(R.id.refresh), (visibility & ACTIONBAR_MENU_REFRESH) != 0);
            setItemVisible(menu.findItem(R.id.renew), (visibility & ACTIONBAR_MENU_RENEW_LIST) != 0);
            setItemVisible(menu.findItem(R.id.renewlist), (visibility & ACTIONBAR_MENU_RENEW_ALL) != 0);
            setItemVisible(menu.findItem(R.id.share), (visibility & ACTIONBAR_MENU_SHARE) != 0);
            setItemVisible(menu.findItem(R.id.newlib), (visibility & ACTIONBAR_MENU_NEWLIB) != 0);
            setItemVisible(menu.findItem(R.id.filter), (visibility & ACTIONBAR_MENU_FILTER) != 0);
        }
    }

    private int currentVisibility;
    @Override
    public boolean onPrepareOptionsMenu(Menu menu) {
        boolean x = super.onPrepareOptionsMenu(menu);
        if (menu == null) return false;
        loadingItem = menu.findItem(R.id.loading);
        if (loadingItem != null) loadingItem.setVisible(loadingTasks.size() > 0);
        setOptionMenuVisibility(menu);

        return x;
    }


    public boolean onOptionsItemIdSelected(int id) {
        Intent intent;
        Activity context = this;
        switch (id) {
            case R.id.search:
                VideLibriApp.newSearchActivity();
                return true;
            case  android.R.id.home:
//                context.openOptionsMenu();
                //              return true;
//            case R.id.accounts:
                /*intent = new Intent(context, VideLibri.class);
                intent.addFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
                context.startActivity(intent);*/
                onBackPressed();
                return true;
            case R.id.accounts:
                intent = new Intent(context, VideLibri.class);
                context.startActivity(intent);
                return true;
            /*case R.id.more:
                setOptionMenuVisibility();
                return false; //should open the normal sub menu*/
            case R.id.options:
                intent = new Intent(context, Options.class);
                context.startActivity(intent);
                return true;
            case R.id.refresh:
                VideLibriApp.updateAccount(null, false, false);
                return true;
            case R.id.renew:
                Util.showMessageYesNo(DialogId.RENEW_CONFIRM, tr(R.string.base_renewallconfirm));
                return true;
            case R.id.renewlist:
                intent = new Intent(context, RenewList.class);
                //if (this instanceof VideLibri) intent.putExtra("accountFilterOverride", ( (VideLibri)this).accountFilterOverride);
                context.startActivity(intent);
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
                intent.putExtra("reason", tr(R.string.base_chooselibhomepage));
                intent.putExtra("search", true);
                context.startActivityForResult(intent, REQUESTED_LIBRARY_HOMEPAGE);
                return true;
            case R.id.libcatalogue:
                intent = new Intent(context, LibraryList.class);
                intent.putExtra("reason", tr(R.string.base_chooselibcat));
                intent.putExtra("search", true);
                context.startActivityForResult(intent, REQUESTED_LIBRARY_CATALOGUE);
                return true;
            case R.id.newlib:
                intent = new Intent(context, NewLibrary.class);
                context.startActivityForResult(intent, RETURNED_FROM_NEW_LIBRARY);
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
        if (mDrawerToggle != null && mDrawerToggle.onOptionsItemSelected(item)) return true;
        if (onOptionsItemIdSelected(item.getItemId())) return true;
        return super.onOptionsItemSelected(item);
    }

    @Override
    protected void onActivityResult(int requestCode, int resultCode, Intent data) {
        if ((requestCode == REQUESTED_LIBRARY_CATALOGUE || requestCode == REQUESTED_LIBRARY_HOMEPAGE) && resultCode == LibraryList.RESULT_OK) {
           Bridge.LibraryDetails details = Bridge.VLGetLibraryDetails(LibraryList.lastSelectedLibId);
           showUriInBrowser(requestCode == REQUESTED_LIBRARY_HOMEPAGE ? details.homepageBase : details.homepageCatalogue);
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

        if (loadingItem != null) loadingItem.setVisible(loadingTasks.size() > 0);
        checkMainIcon();

        for (Bundle b: VideLibriApp.pendingDialogs)
                Util.showDialog(this, b);
        VideLibriApp.pendingDialogs.clear();
    }

    @Override
    protected void onPause() {
        if (VideLibriApp.currentActivity == this) VideLibriApp.currentActivity = null;
        super.onPause();
    }

    @Override
    protected void onSaveInstanceState(Bundle outState) {
        super.onSaveInstanceState(outState);
        outState.putIntegerArrayList("activeLoadingTasks", loadingTasks);
    }

    @Override
    protected void onDestroy() {
        if (VideLibriApp.currentActivity == this) VideLibriApp.currentActivity = null;
        super.onDestroy();
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


    void beginLoading(int loadingId){
        loadingTasks.add(loadingId);
        if (loadingItem != null) loadingItem.setVisible(loadingTasks.size() > 0);
    }
    void endLoading(int loadingId){
        int pos = loadingTasks.indexOf(loadingId);
        if (pos >= 0) loadingTasks.remove(pos);
        if (loadingItem != null) loadingItem.setVisible(loadingTasks.size() > 0);
    }
    void endLoadingAll(int loadingId){
        Integer lid = loadingId;
        while (true) {
            int pos = loadingTasks.indexOf(lid);
            if (pos < 0) break;
            loadingTasks.remove(pos);
        }
        if (loadingItem != null) loadingItem.setVisible(loadingTasks.size() > 0);
    }
    void endLoadingAll(int[] loadingId){
        for (int id: loadingId) endLoadingAll(id);
    }
    boolean isLoading(int loadingId){
        return loadingTasks.indexOf(loadingId) >= 0;
    }
    void showLoadingInfo(){
        StringBuilder sb = new StringBuilder();
        sb.append(tr(R.string.loading_tasks_info));
        for (int id: loadingTasks) {
            int code = 0;
            switch (id) {
                case LOADING_ACCOUNT_UPDATE:code=R.string.loading_account_update; break;
                case LOADING_COVER_IMAGE: code=R.string.loading_cover; break;
                case LOADING_SEARCH_CONNECTING: code=R.string.loading_search_connecting; break;
                case LOADING_SEARCH_SEARCHING: code=R.string.loading_search_searching; break;
                case LOADING_SEARCH_DETAILS: code=R.string.loading_search_details; break;
                case LOADING_SEARCH_ORDER: code=R.string.loading_search_order; break;
                case LOADING_SEARCH_ORDER_HOLDING: code=R.string.loading_search_order_holding; break;
                case LOADING_SEARCH_MESSAGE: code=R.string.loading_search_message; break;
                case LOADING_INSTALL_LIBRARY: code=R.string.loading_search_install_library; break;
            }
            if (code != 0) sb.append(tr(code));
            sb.append("\n");
        }
        Util.showMessage(sb.toString());
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
