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
import android.util.Log;
import android.util.Pair;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.*;
import com.actionbarsherlock.ActionBarSherlock;
import com.actionbarsherlock.view.Menu;
import com.actionbarsherlock.view.MenuInflater;
import com.actionbarsherlock.view.MenuItem;

import java.util.ArrayList;
import java.util.List;

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

    static public boolean onOptionsItemSelected(Activity context, MenuItem item) {
        return onOptionsItemIdSelected(context, item.getItemId());
    }
    static public boolean onOptionsItemIdSelected(Activity context, int id) {
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
                VideLibriApp.updateAccount(null, false, true);
                return true;
            case R.id.renewlist:
                context.startActivity(new Intent(context, RenewList.class));
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
            case R.id.about:
                intent = new Intent(context, About.class);
                context.startActivity(intent);
                return true;
            case R.id.others:
                displayOthersMenu(context);
        }
        return false;
    }


    static public void onCreateOptionsMenu(ActionBarSherlock sherlock, Menu menu){
        MenuInflater inflater = sherlock.getMenuInflater();
        inflater.inflate(R.menu.videlibrimenu, menu);
        MenuItem loadingItem = menu.findItem(R.id.loading);

        if (loadingItem == null) {
            Log.w("VideLibri", "failed to find loading item");
            return;
        }

        View refreshView;
        LayoutInflater linflater = (LayoutInflater)sherlock.getActionBar().getThemedContext().getSystemService(Context.LAYOUT_INFLATER_SERVICE);
        refreshView = linflater.inflate(R.layout.actionbar_loading, null);;
        loadingItem.setActionView(refreshView);
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






    //from http://stackoverflow.com/questions/14933724/android-icon-of-sub-menu-items-not-appear
    public final static class SubMenuWithIconsAdapter extends ArrayAdapter<String> {
        private Activity context;
        private ArrayList<String> texts;
        private ArrayList<Drawable> icons;
        private ArrayList<Integer> ids;

        public SubMenuWithIconsAdapter(Activity context, ArrayList<String> texts, ArrayList<Drawable> icons, ArrayList<Integer> ids) {
            super(context, R.layout.submenu_item, texts);

            this.context = context;
            this.texts = texts;
            this.icons = icons;
            this.ids = ids;
        }

        @Override
        public View getView(int position, View convertView, ViewGroup parent) {
            LayoutInflater inflater = context.getLayoutInflater();
            View row = inflater.inflate(R.layout.submenu_item, parent, false);

            TextView label = (TextView) row.findViewById(R.id.text_item);
            label.setText(texts.get(position));

            ImageView icon = (ImageView) row.findViewById(R.id.icon_item);
            icon.setImageDrawable(icons.get(position));

            return row;
        }

        /*   static SubMenuWithIconsAdapter newSubMenuAdapter(Activity context, int menuId){
            context.getResources().getXml()
            android.view.MenuInflater inflater = context.getMenuInflater();
            context.getLayoutInflater().s
            android.view.Menu menu = new Cont
            inflater.inflate(menuId, menu);
            MenuItem loadingItem = menu.findItem(R.id.loading);

        } */
    }


    static private void displayOthersMenu(final Activity context){
        ArrayList<String> texts = new ArrayList<String>();
        ArrayList<Drawable> icons = new ArrayList<Drawable>();
        final ArrayList<Integer> ids = new ArrayList<Integer>();


        if (VideLibriApp.accounts.length > 0) {
            texts.add(Util.tr(R.string.menu_renewlist)); icons.add(context.getResources().getDrawable(android.R.drawable.ic_menu_week)); ids.add(R.id.renewlist);
            texts.add(Util.tr(R.string.menu_renewall)); icons.add(context.getResources().getDrawable(android.R.drawable.ic_menu_month)); ids.add(R.id.renew);
            if (VideLibriApp.runningUpdates.isEmpty()) {
                texts.add(Util.tr(R.string.menu_refreshall)); icons.add(context.getResources().getDrawable(android.R.drawable.ic_menu_rotate)); ids.add(R.id.refresh);
            }
        }
        texts.add(Util.tr(R.string.menu_homepages)); icons.add(context.getResources().getDrawable(android.R.drawable.ic_menu_info_details)); ids.add(R.id.libinfo);
        texts.add(Util.tr(R.string.menu_cats)); icons.add(context.getResources().getDrawable(android.R.drawable.ic_menu_info_details)); ids.add(R.id.libcatalogue);
        texts.add(Util.tr(R.string.menu_feedback)); icons.add(context.getResources().getDrawable(android.R.drawable.ic_menu_send)); ids.add(R.id.feedback);
        texts.add(Util.tr(R.string.menu_about)); icons.add(context.getResources().getDrawable(android.R.drawable.ic_menu_help)); ids.add(R.id.about);

        AlertDialog.Builder builder = new AlertDialog.Builder(context);
        builder.setTitle(Util.tr(R.string.menu_misc));
        ListView view = new ListView(context);
        view.setAdapter(new SubMenuWithIconsAdapter(context, texts, icons, ids));
        view.setCacheColorHint(0);
        view.setBackgroundColor(Color.WHITE);
        builder.setView(view);
        final Dialog dialog = builder.create();
        view.setOnItemClickListener(new AdapterView.OnItemClickListener() {
            @Override
            public void onItemClick(AdapterView<?> adapterView, View view, int i, long l) {
                if (i >= 0 && i < ids.size()) {
                    onOptionsItemIdSelected(context, ids.get(i));
                    dialog.dismiss();
                }
            }
        });
        dialog.setCanceledOnTouchOutside(true);
        dialog.show();
    }
}
