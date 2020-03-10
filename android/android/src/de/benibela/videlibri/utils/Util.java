package de.benibela.videlibri.utils;

import android.content.ClipData;
import android.content.ContentUris;
import android.content.Context;
import android.content.res.Resources;
import android.database.Cursor;
import android.net.Uri;
import android.os.Build;
import android.os.Environment;
import android.provider.DocumentsContract;
import android.provider.MediaStore;
import androidx.annotation.NonNull;
import androidx.appcompat.app.AppCompatActivity;

import android.view.View;
import android.view.ViewGroup;
import android.widget.Toast;

import java.text.DateFormat;
import java.util.Date;
import java.util.IllegalFormatException;

import de.benibela.videlibri.VideLibriApp;

import static android.content.Context.CLIPBOARD_SERVICE;

@SuppressWarnings({"WeakerAccess"})
public class Util {
    static int MessageHandlerCanceled = -123;
    static public String tr(int id){
        if (VideLibriApp.currentContext() == null) return "?tr?"+id;
        else return tr(VideLibriApp.currentContext(), id);
    }
    static public String tr(int id, Object... args){
        Context context = VideLibriApp.currentContext();
        if (context == null) return "?tr?" + id;
        try {
            return context.getString(id, args);
        } catch (Resources.NotFoundException e) {
            return "missing translation: "+id;
        } catch (IllegalFormatException e) {
            return context.getString(id);
        }
    }
    static public String tr(Context context, int id){
        try {
            return context.getString(id);
        } catch (Resources.NotFoundException e) {
            return "missing translation: "+id;
        }
    }
    static public String tr(Context context, int id, Object... args){
        try {
            return context.getString(id, args);
        } catch (Resources.NotFoundException e) {
            return "missing translation: "+id;
        } catch (IllegalFormatException e) {
            return context.getString(id);
        }
    }
    static public String[] tr(Context context, int[] ids){
        String[] res = new String[ids.length];
        for (int i=0;i<ids.length;i++)
            res[i] = tr(context, ids[i]);
        return res;
    }


    static public boolean equalStrings(String s, String t) {
        return s == null ? t == null : s.equals(t);
    }

    public static boolean isEmptyString(String s) {
        return s == null || "".equals(s);
    }












    static public class UriToPath {
        //https://stackoverflow.com/a/36129285
        static public String getPath(Context context, Uri uri) {
            if (uri == null) return null;
            try {
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
                            if (id != null && id.startsWith("raw:/"))
                                return id.substring(4);
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
            } catch (Exception ignored) {

            }
            // File
            if ("file".equalsIgnoreCase(uri.getScheme()) || "raw".equalsIgnoreCase(uri.getScheme())) {
                return uri.getPath();
            }
            return null;
        }

        static String getDataColumn(Context context, Uri uri, String selection, String[] selectionArgs) {
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

        static boolean isExternalStorageDocument(Uri uri) {
            return "com.android.externalstorage.documents".equals(uri.getAuthority());
        }


        /**
         * @param uri The Uri to check.
         * @return Whether the Uri authority is DownloadsProvider.
         */

        static boolean isDownloadsDocument(Uri uri) {
            return "com.android.providers.downloads.documents".equals(uri.getAuthority());
        }

        /**
         * @param uri The Uri to check.
         * @return Whether the Uri authority is MediaProvider.
         */
        static boolean isMediaDocument(Uri uri) {
            return "com.android.providers.media.documents".equals(uri.getAuthority());
        }

        /**
         * @param uri The Uri to check.
         * @return Whether the Uri authority is Google Photos.
         */
        static boolean isGooglePhotosUri(Uri uri) {
            return "com.google.android.apps.photos.content".equals(uri.getAuthority());
        }
    }
}
