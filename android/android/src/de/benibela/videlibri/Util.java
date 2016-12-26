package de.benibela.videlibri;

import android.app.AlertDialog;
import android.app.Dialog;
import android.content.Context;
import android.content.DialogInterface;
import android.content.res.Resources;
import android.view.View;
import android.widget.AdapterView;
import android.widget.ListView;

import java.text.DateFormat;
import java.util.*;

/**
 * Created with IntelliJ IDEA.
 * User: benito
 * Date: 5/28/13
 * Time: 2:07 PM
 * To change this template use File | Settings | File Templates.
 */
interface MessageHandler{
    void onDialogEnd(DialogInterface dialogInterface, int i);
}
public class Util {
    static int MessageHandlerCanceled = -123;
    static public String tr(int id){
        if (VideLibriApp.currentActivity == null) return "?tr?";
        else return tr(VideLibriApp.currentActivity, id);
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
        /*} catch (MissingFormatArgumentException e) { child of    IllegalFormatException
            return context.getString(id);
        } */
        }
    }

    static public boolean equalStrings(String s, String t) {
        return s == null ? t == null : s.equals(t);
    }

    static public void showMessage(String message){showMessage(message, null, tr(R.string.ok), null, null);}
    static public void showMessage(String message, final MessageHandler handler){showMessage(message, null, tr(R.string.ok), null, handler);}
    static public void showMessageYesNo(String message, MessageHandler handler){ Util.showMessage(message, tr(R.string.no), null, tr(R.string.yes), handler); }
    static public void showMessage(String message, String negative, String neutral, String positive, final MessageHandler handler){
        Context context = VideLibriApp.currentActivity;
        if (context == null) return;
        AlertDialog.Builder builder = new AlertDialog.Builder(context);
        builder.setMessage(message);
        builder.setTitle("VideLibri");
        if (negative != null)
            builder.setNegativeButton(negative, new DialogInterface.OnClickListener() {
                @Override
                public void onClick(DialogInterface dialogInterface, int i) {
                    if (handler != null) handler.onDialogEnd(dialogInterface, i);
                }
            });
        if (neutral != null)
            builder.setNeutralButton(neutral, new DialogInterface.OnClickListener() {
                @Override
                public void onClick(DialogInterface dialogInterface, int i) {
                    if (handler != null) handler.onDialogEnd(dialogInterface, i);
                }
            });
        if (positive != null)
            builder.setPositiveButton(positive, new DialogInterface.OnClickListener() {
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

    static public void chooseDialog(Context context, String message, String[] options, final MessageHandler handler) {
        AlertDialog.Builder builder = new AlertDialog.Builder(context);
        builder.setTitle(message);
        builder.setItems(options, new DialogInterface.OnClickListener() {
            public void onClick(DialogInterface dialog, int item) {
                if (handler != null) handler.onDialogEnd(dialog, item);
            }
        });
        if (handler != null)
            builder.setOnCancelListener(new DialogInterface.OnCancelListener() {
                @Override
                public void onCancel(DialogInterface dialogInterface) {
                    handler.onDialogEnd(dialogInterface, MessageHandlerCanceled);
                }
            });
        AlertDialog alert = builder.create();
        alert.show();
    }

    public static int strToIntDef(String firstVersion, int i) {
        try {
            return Integer.parseInt(firstVersion);
        } catch (NumberFormatException e) {
            return i;
        }
    }


    private static DateFormat dateFormat;
    public static String formatDate(Date date){
        if (date == null) return "";
        if (dateFormat != null) return dateFormat.format(date);
        if (dateFormat == null && VideLibriApp.currentActivity != null)
            dateFormat = android.text.format.DateFormat.getDateFormat(VideLibriApp.currentActivity);
        if (dateFormat != null) return dateFormat.format(date);
        return date.getYear()+"-"+date.getMonth()+"-"+date.getDay();
    }

    public static String formatDate(Bridge.SimpleDate date){
        if (date == null) return tr(R.string.unknown_date);
        if (Bridge.currentPascalDate > 0 && VideLibriApp.currentActivity != null) {
            switch (date.pascalDate - Bridge.currentPascalDate) {
                case -2: return tr(R.string.daybeforeyesterday);
                case -1: return tr(R.string.yesterday);
                case 0: return tr(R.string.today);
                case 1: return tr(R.string.tomorrow);
                case 2: return tr(R.string.dayaftertomorrow);
            }
        }
        return formatDate(date.getTime());
    }

    public static int compare(int a, int b) {
        if (a < b) return -1;
        else if (b < a) return 1;
        else return 0;
    }
    public static int compare(boolean a, boolean b) {
        if (a == b) return 0;
        if (a) return 1;
        return -1;
    }
    public static int   compareNullFirst(Object a, Object b) {
        return compare(a != null, b != null);
    }
}
