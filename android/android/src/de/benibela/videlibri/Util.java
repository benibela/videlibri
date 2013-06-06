package de.benibela.videlibri;

import android.app.AlertDialog;
import android.app.Dialog;
import android.content.Context;
import android.content.DialogInterface;
import android.view.View;
import android.widget.AdapterView;
import android.widget.ListView;

import java.util.ArrayList;
import java.util.List;

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
    static public void showMessage(Context context, String message, final MessageHandler handler){showMessage(context, message, null, "OK", null, handler);}
    static public void showMessageYesNo(Context context, String message, MessageHandler handler){ Util.showMessage(context, message, "Nein", null, "Ja", handler); }
    static public void showMessage(Context context, String message, String negative, String neutral, String positive, final MessageHandler handler){
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
}
