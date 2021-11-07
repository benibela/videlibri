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

    public static boolean isEmptyString(String s) {
        return s == null || "".equals(s);
    }



}
