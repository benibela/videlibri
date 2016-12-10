package de.benibela.videlibri;

import android.app.Activity;
import android.app.AlertDialog;
import android.app.Dialog;
import android.content.Context;
import android.content.DialogInterface;
import android.content.Intent;
import android.graphics.Color;
import android.graphics.drawable.Drawable;
import android.net.Uri;
import android.support.annotation.LayoutRes;
import android.support.v7.app.ActionBar;
import android.support.v7.widget.Toolbar;
import android.util.Log;
import android.util.Pair;
import android.view.LayoutInflater;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;
import android.view.View;
import android.view.ViewGroup;
import android.widget.*;


import java.util.ArrayList;
import java.util.List;

import android.support.v7.app.AppCompatActivity;
import android.support.v4.view.MenuItemCompat;


/**
 * Created with IntelliJ IDEA.
 * User: benito
 * Date: 5/28/13
 * Time: 2:23 PM
 * To change this template use File | Settings | File Templates.
 */
public class VideLibriSuperBase {
    private static final int REQUESTED_LIBRARY_HOMEPAGE  = 29324;
    private static final int REQUESTED_LIBRARY_CATALOGUE = 29325;

    static public void initActionBar(AppCompatActivity activity) {
        Toolbar bar = (Toolbar) activity.findViewById(R.id.actionbar);
        if (bar == null) return;
        activity.setSupportActionBar(bar);
        ActionBar sbar = activity.getSupportActionBar();
        sbar.setDisplayHomeAsUpEnabled(true);
        sbar.setHomeAsUpIndicator(R.drawable.icon);
    }


    static public boolean onOptionsItemSelected(Activity context, MenuItem item) {
        return onOptionsItemIdSelected(context, item.getItemId());
    }
    static public boolean onOptionsItemIdSelected(final Activity context, int id) {
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


      static public void onCreateOptionsMenu(Activity activity, Menu menu){
        //if (!(activity instanceof AppCompatActivity)) return;
        //MenuInflater inflater = ((AppCompatActivity)activity).getSupportMenuInflater();
        MenuInflater inflater = activity.getMenuInflater();
        inflater.inflate(R.menu.videlibrimenu, menu);
        MenuItem loadingItem = menu.findItem(R.id.loading);

        if (loadingItem == null) {
            Log.w("VideLibri", "failed to find loading item");
            return;
        }
        MenuItemCompat.setActionView(loadingItem, R.layout.actionbar_loading);
    }

    static public void onPrepareOptionsMenu(Menu menu) {
        if (VideLibriApp.instance != null) {
          //  menu.findItem(R.id.accounts).setEnabled(VideLibriApp.accounts.length > 0);
            /*
            menu.findItem(R.id.refresh).setEnabled(!VideLibriApp.runningUpdates empty);

            menu.findItem(R.id.refresh).setEnabled(VideLibriApp.accounts.length > 0);
            menu.findItem(R.id.renew).setEnabled(VideLibriApp.accounts.length > 0);
            menu.findItem(R.id.renewlist).setEnabled(VideLibriApp.accounts.length > 0);   */
            //menu.findItem(R.id.options).setEnabled(VideLibriApp.accounts.length > 0);
        }
    }

    public static boolean onActivityResult(Activity activity, int requestCode, int resultCode, Intent data) {
       if ((requestCode == REQUESTED_LIBRARY_CATALOGUE || requestCode == REQUESTED_LIBRARY_HOMEPAGE) && resultCode == LibraryList.RESULT_OK) {
            Bridge.LibraryDetails details = Bridge.VLGetLibraryDetails(LibraryList.lastSelectedLibId);
            activity.startActivity(new Intent(Intent.ACTION_VIEW, Uri.parse(
                    requestCode == REQUESTED_LIBRARY_HOMEPAGE ? details.homepageBase : details.homepageCatalogue
                    )));
            return  true;
        }
        return false;
    }


    public static String userPath(Context context) {
        return context.getFilesDir().getAbsolutePath();
    }

}
