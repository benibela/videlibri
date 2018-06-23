package de.benibela.videlibri;

import android.content.ContentUris;
import android.content.Context;
import android.content.DialogInterface;
import android.content.Intent;
import android.database.Cursor;
import android.net.Uri;
import android.os.Build;
import android.os.Bundle;
import android.os.Environment;
import android.provider.DocumentsContract;
import android.provider.MediaStore;
import android.support.annotation.RequiresApi;
import android.util.AttributeSet;
import android.view.MotionEvent;
import android.view.View;
import android.widget.AbsListView;
import android.widget.ArrayAdapter;
import android.widget.ListView;
import android.widget.ScrollView;

import java.io.File;
import java.util.ArrayList;

public class ImportExport extends VideLibriBaseActivity {

    //https://stackoverflow.com/a/22751867
    public static class MaxHeightListView extends ListView{


        public MaxHeightListView(Context context) {
            super(context);
        }
        public MaxHeightListView(Context context, AttributeSet attrs)
        {
            super(context, attrs);
        }
        public MaxHeightListView(Context context, AttributeSet attrs, int defStyle) {
            super(context, attrs, defStyle);
        }
        @Override
        public void onMeasure(int widthMeasureSpec, int heightMeasureSpec)
        {
            super.onMeasure(widthMeasureSpec, MeasureSpec.makeMeasureSpec(Integer.MAX_VALUE >> 2, MeasureSpec.AT_MOST));
        }
    }
    public static class ScrollViewInterceptor extends ScrollView
    {
        float startY;

        public ScrollViewInterceptor(Context context, AttributeSet attrs)
        {
            super(context, attrs);
        }

        @Override
        public boolean onInterceptTouchEvent(MotionEvent e)
        {
            onTouchEvent(e);
            if (e.getAction() == MotionEvent.ACTION_DOWN) startY = e.getY();
            return (e.getAction() == MotionEvent.ACTION_MOVE) && (Math.abs(startY - e.getY()) > 50);
        }
    }

    public static final int MODE_IMPORT = 0;
    public static final int MODE_EXPORT = 1;

    int mode;
    ArrayAdapter<String> accountAdapter;
    ArrayAdapter<String> flagAdapter;
    Bridge.ImportExportData data;

    private void checkAll(ListView lv) {
        lv.setChoiceMode(AbsListView.CHOICE_MODE_MULTIPLE);
        int count = lv.getCount();
        for (int i=0;i<count;i++)
            lv.setItemChecked(i, true);
    }

    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);    //To change body of overridden methods use File | Settings | File Templates.
        setVideLibriView(R.layout.importexport);
        mode = getIntent().getIntExtra("mode", MODE_IMPORT);

        File dir = Environment.getExternalStorageDirectory();
        File export = new File(dir , "videlibri.xml" );
        setEditTextText(R.id.edit, export.getAbsolutePath());


        if (mode == MODE_IMPORT) {
            findViewById(R.id.textView).setVisibility(View.GONE);
            findViewById(R.id.textView1).setVisibility(View.GONE);
            findViewById(R.id.listView).setVisibility(View.GONE);
            findViewById(R.id.listView1).setVisibility(View.GONE);
            setTitle(tr(R.string.import_));
            findButtonById(R.id.button).setText(tr(R.string.import_));
            findButtonById(R.id.button).setText(tr(R.string.import_load));
            setTextViewText(R.id.textView, tr(R.string.import_accounts));
            setTextViewText(R.id.textView1, tr(R.string.import_properties));
            setTextViewText(R.id.textView2, tr(R.string.import_file));
            if (VideLibriApp.runningUpdates.size() > 0) {
                Util.showMessage(DialogId.IMPORTEXPORT_DONE, tr(R.string.import_not_while_update_runs));
                return;
            }
        } else {
            setTitle(tr(R.string.export));
            findButtonById(R.id.button).setText(tr(R.string.export));
            setTextViewText(R.id.textView, tr(R.string.export_accounts));
            setTextViewText(R.id.textView1, tr(R.string.export_properties));
            setTextViewText(R.id.textView2, tr(R.string.export_file));

            Bridge.Account[] accounts = Bridge.VLGetAccounts();
            String[] accountNames = new String[accounts.length];
            for (int i=0;i<accounts.length;i++) accountNames[i] = accounts[i].prettyName;
            accountAdapter = new ArrayAdapter<String>(this, android.R.layout.simple_list_item_multiple_choice, accountNames);
            ListView lv = ((ListView)findViewById(R.id.listView));
            lv.setAdapter(accountAdapter);
            checkAll(lv);
        }
        final String[] options = new String[]{tr(R.string.lay_options_option_current), tr(R.string.history), tr(R.string.configuration), tr(R.string.passwords)};
        flagAdapter = new ArrayAdapter<String>(this, android.R.layout.simple_list_item_multiple_choice, options);
        ListView lv = ((ListView)findViewById(R.id.listView1));
        lv.setAdapter(flagAdapter);
        checkAll(lv);
        lv.setItemChecked(flagAdapter.getCount()-1, false);


        findButtonById(R.id.button).setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View view) {
                ListView flagListView = (ListView)findViewById((R.id.listView1));
                int flags = 0;
                int optionI = 0;
                for (int i=0;i<flagAdapter.getCount();i++)
                    if (flagListView.isItemChecked(i)) {
                        for (;!options[optionI].equals(flagAdapter.getItem(i));optionI++)
                            ;
                        flags |= 1 << optionI;
                    }
                if ((flags & Bridge.ImportExportData.PASSWORD) != 0)
                    flags |= Bridge.ImportExportData.CONFIG;


                ListView accountListView = (ListView)findViewById((R.id.listView));
                try {
                    if (mode == MODE_IMPORT) {
                        if (data == null) {
                            data = Bridge.VLImportAccountsPrepare(getEditTextText(R.id.edit));

                            accountAdapter = new ArrayAdapter<String>(ImportExport.this, android.R.layout.simple_list_item_multiple_choice, data.accountsToImport);
                            ListView lv = (ListView)findViewById(R.id.listView);
                            lv.setAdapter(accountAdapter);
                            checkAll(lv);

                            ArrayList<String> newOptions = new ArrayList<String>();
                            for (int i=0;i<options.length;i++)
                                if ((data.flags & (1 << i)) != 0)
                                    newOptions.add(options[i]);
                            flagAdapter = new ArrayAdapter<String>(ImportExport.this, android.R.layout.simple_list_item_multiple_choice, newOptions);
                            lv = ((ListView)findViewById(R.id.listView1));
                            lv.setAdapter(flagAdapter);
                            checkAll(lv);

                            findButtonById(R.id.button).setText(tr(R.string.import_));
                            findViewById(R.id.textView).setVisibility(View.VISIBLE);
                            findViewById(R.id.textView1).setVisibility(View.VISIBLE);
                            findViewById(R.id.listView).setVisibility(View.VISIBLE);
                            findViewById(R.id.listView1).setVisibility(View.VISIBLE);
                            findViewById(R.id.textView2).setVisibility(View.GONE);
                            findViewById(R.id.edit).setVisibility(View.GONE);
                        } else {
                            ArrayList<String> choosen = new ArrayList<String>();
                            for (int i=0;i<accountAdapter.getCount();i++)
                                if (accountListView.isItemChecked(i))
                                    choosen.add(data.accountsToImport[i]);
                            data.accountsToImport = new String[choosen.size()];
                            for (int i=0;i<data.accountsToImport.length;i++)
                                data.accountsToImport[i] = choosen.get(i);
                            data.flags = flags;
                            Bridge.VLImportAccounts(data);
                            VideLibriApp.refreshAccountList();
                            VideLibriApp.refreshDisplayedLendBooks();
                            boolean hasEmptyPass = false;
                            for (Bridge.Account acc: VideLibriApp.accounts)
                                if (!Util.isEmptyString(acc.name) && Util.isEmptyString(acc.pass)) hasEmptyPass = true;
                            Util.showMessage(DialogId.IMPORTEXPORT_DONE, tr( hasEmptyPass ? R.string.import_done_has_empty_pass : R.string.import_done));
                            data = null;
                        }
                    }   else {
                        Bridge.Account[] accounts = Bridge.VLGetAccounts();
                        ArrayList<Bridge.Account> choosen = new ArrayList<Bridge.Account>();
                        for (int i=0;i<accounts.length;i++)
                            if (accountListView.isItemChecked(i))
                                choosen.add(accounts[i]);
                        accounts = new Bridge.Account[choosen.size()];
                        for (int i=0;i<accounts.length;i++)
                            accounts[i] = choosen.get(i);
                        Bridge.VLExportAccounts(getEditTextText(R.id.edit), accounts, flags);
                        Util.showMessage(DialogId.IMPORTEXPORT_DONE, tr(R.string.export_done));
                    }
                } catch (Bridge.InternalError e) {
                    Util.showMessage(e.getMessage());
                }
            }
        });

        findButtonById(R.id.buttonChoose).setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                Intent intent = new Intent();
                intent.setAction(Intent.ACTION_GET_CONTENT);
                intent.setType("*/*");
                //intent.putExtra(Intent.CAT, true);
                intent.putExtra("android.intent.extra.LOCAL_ONLY", true);
                startActivityForResult(intent, 30017);
            }
        });
    }

    @Override
    protected void onDestroy() {
        if (data != null) {
            data.flags = 0;
            Bridge.VLImportAccounts(data);
        }
        super.onDestroy();
    }

    @Override
    boolean onDialogResult(int dialogId, int buttonId, Bundle more) {
        switch (dialogId) {
            case DialogId.IMPORTEXPORT_DONE:
                finish();
                return true;
        }
        return super.onDialogResult(dialogId, buttonId, more);
    }

    @Override
    protected void onActivityResult(int requestCode, int resultCode, Intent data) {
        if (requestCode == 30017 && resultCode != RESULT_CANCELED && data != null) {
            String path = UriToPath.getPath(this, data.getData());
            if (path != null) setEditTextText(R.id.edit, path);
            return;
        }
        super.onActivityResult(requestCode, resultCode, data);
    }


    static private class UriToPath {
        //https://stackoverflow.com/a/36129285
        public static String getPath(Context context, Uri uri) {
            if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.KITKAT) {
                if (DocumentsContract.isDocumentUri(context, uri)) {
                    // ExternalStorageProvider
                    if (isExternalStorageDocument(uri)) {
                        final String docId = DocumentsContract.getDocumentId(uri);
                        final String[] split = docId.split(":");
                        final String type = split[0];

                        if ("primary".equalsIgnoreCase(type)) {
                            return Environment.getExternalStorageDirectory() + "/" + split[1];
                        }
                        return Environment.getExternalStorageDirectory() + "/" + split[1]; //??
                    }
                    // DownloadsProvider
                    else if (isDownloadsDocument(uri)) {
                        final String id = DocumentsContract.getDocumentId(uri);
                        final Uri contentUri = ContentUris.withAppendedId(Uri.parse("content://downloads/public_downloads"), Long.valueOf(id));
                        return getDataColumn(context, contentUri, null, null);
                    }
                    // MediaProvider
                    else if (isMediaDocument(uri)) {
                        final String docId = DocumentsContract.getDocumentId(uri);
                        final String[] split = docId.split(":");
                        final String type = split[0];
                        Uri contentUri = null;
                        if ("image".equals(type)) {
                            contentUri = MediaStore.Images.Media.EXTERNAL_CONTENT_URI;
                        } else if ("video".equals(type)) {
                            contentUri = MediaStore.Video.Media.EXTERNAL_CONTENT_URI;
                        } else if ("audio".equals(type)) {
                            contentUri = MediaStore.Audio.Media.EXTERNAL_CONTENT_URI;
                        }
                        final String selection = "_id=?";
                        final String[] selectionArgs = new String[]{split[1]};
                        return getDataColumn(context, contentUri, selection, selectionArgs);
                    }
                }
            }

            // MediaStore (and general)
            if ("content".equalsIgnoreCase(uri.getScheme())) {
                // Return the remote address
                if (isGooglePhotosUri(uri))
                    return uri.getLastPathSegment();
                return getDataColumn(context, uri, null, null);
            }
            // File
            if ("file".equalsIgnoreCase(uri.getScheme())) {
                return uri.getPath();
            }
            return null;
        }

        public static String getDataColumn(Context context, Uri uri, String selection, String[] selectionArgs) {
            Cursor cursor = null;
            final String column = "_data";
            final String[] projection = {column};
            try {
                cursor = context.getContentResolver().query(uri, projection, selection, selectionArgs, null);
                if (cursor != null && cursor.moveToFirst()) {
                    final int index = cursor.getColumnIndexOrThrow(column);
                    return cursor.getString(index);
                }
            } finally {
                if (cursor != null)
                    cursor.close();
            }
            return null;
        }

        public static boolean isExternalStorageDocument(Uri uri) {
            return "com.android.externalstorage.documents".equals(uri.getAuthority());
        }


        /**
         * @param uri The Uri to check.
         * @return Whether the Uri authority is DownloadsProvider.
         */
        public static boolean isDownloadsDocument(Uri uri) {
            return "com.android.providers.downloads.documents".equals(uri.getAuthority());
        }

        /**
         * @param uri The Uri to check.
         * @return Whether the Uri authority is MediaProvider.
         */
        public static boolean isMediaDocument(Uri uri) {
            return "com.android.providers.media.documents".equals(uri.getAuthority());
        }

        /**
         * @param uri The Uri to check.
         * @return Whether the Uri authority is Google Photos.
         */
        public static boolean isGooglePhotosUri(Uri uri) {
            return "com.google.android.apps.photos.content".equals(uri.getAuthority());
        }
    }
}
