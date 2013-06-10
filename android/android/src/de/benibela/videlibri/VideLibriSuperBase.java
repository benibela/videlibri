package de.benibela.videlibri;

import android.app.Activity;
import android.content.Context;
import android.content.Intent;
import android.net.Uri;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import com.actionbarsherlock.ActionBarSherlock;
import com.actionbarsherlock.view.Menu;
import com.actionbarsherlock.view.MenuInflater;
import com.actionbarsherlock.view.MenuItem;

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
        // Handle item selection
        Intent intent;
        switch (item.getItemId()) {
            case R.id.search:
                VideLibri.newSearchActivity();
                return true;
            case R.id.accounts:
                intent = new Intent(context, VideLibri.class);
                intent.addFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
                context.startActivity(intent);
                return true;
            case R.id.options:
                intent = new Intent(context, Options.class);
                context.startActivity(intent);
                return true;
            case R.id.refresh:
                if (VideLibri.instance != null && !VideLibri.instance.loading)
                    VideLibri.updateAccount(null, false, false);
                return true;
            case R.id.renew:
                if (VideLibri.instance != null && !VideLibri.instance.loading)
                    VideLibri.updateAccount(null, false, true);
                return true;
            case R.id.libinfo:
                intent = new Intent(context, LibraryList.class);
                intent.putExtra("reason", "Wählen Sie eine Bücherei um ihre Homepage zu öffnen:");
                intent.putExtra("search", true);
                context.startActivityForResult(intent, REQUESTED_LIBRARY_HOMEPAGE);
                return true;
            case R.id.libcatalogue:
                intent = new Intent(context, LibraryList.class);
                intent.putExtra("reason", "Wählen Sie eine Bücherei um ihren Webkatalog zu öffnen:");
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
            case  android.R.id.home:
                context.openOptionsMenu();
                return true;
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
        if (VideLibri.instance != null) {
            menu.findItem(R.id.refresh).setEnabled(!VideLibri.instance.loading);

            menu.findItem(R.id.accounts).setEnabled(VideLibri.instance.accounts.length > 0);
            menu.findItem(R.id.refresh).setEnabled(VideLibri.instance.accounts.length > 0);
            menu.findItem(R.id.renew).setEnabled(VideLibri.instance.accounts.length > 0);
            //menu.findItem(R.id.options).setEnabled(VideLibri.instance.accounts.length > 0);
        }
    }

    public static boolean onActivityResult(Activity activity, int requestCode, int resultCode, Intent data) {
       if ((requestCode == REQUESTED_LIBRARY_CATALOGUE || requestCode == REQUESTED_LIBRARY_HOMEPAGE) && resultCode == LibraryList.RESULT_OK) {
            String id = data.getStringExtra("libId");
            Bridge.LibraryDetails details = Bridge.VLGetLibraryDetails(id);
            activity.startActivity(new Intent(Intent.ACTION_VIEW, Uri.parse(
                    requestCode == REQUESTED_LIBRARY_HOMEPAGE ? details.homepageBase : details.homepageCatalogue
                    )));
            return  true;
        }
        return false;
    }
}
