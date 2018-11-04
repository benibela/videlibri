package de.benibela.videlibri;

import android.os.Bundle;
import android.view.View;
import android.widget.AdapterView;
import android.widget.EditText;
import android.widget.Spinner;
import android.widget.Toast;

import org.acra.util.IOUtils;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;

import de.benibela.videlibri.jni.Bridge;

public class SourceEdit extends VideLibriBaseActivity{
    final String BASEDIR_TEMPLATES = "libraries/templates/";
    final String BASEDIR_LIBRARIES = "libraries/";

    private String[] libraryIds, templateIds;

    private String baseDir = "";
    private String fileName = "";
    private String[] selection1, selection2;

    void makeLibraryIds(){
        ArrayList<String> temp =  new ArrayList<>(Arrays.asList(Bridge.VLGetLibraryIds()));
        temp.add(tr(R.string.source_edit_new_library));
        libraryIds = temp.toArray(new String[]{});
        for (int i=0;i<libraryIds.length - 1;i++)
            libraryIds[i] = libraryIds[i] + ".xml";
    }

    void makeTemplateIds(){
        templateIds = Bridge.VLGetTemplates();
        selection1 = new String[templateIds.length+2];
        selection1[0] = tr(R.string.lay_source_edit_library_list);
        for (int i=0;i<templateIds.length;i++)
            selection1[i+1] = tr(R.string.lay_source_edit_template, templateIds[i]);
        selection1[selection1.length - 1] = tr(R.string.source_edit_new_directory);
    }

    void showSelection1(){
        final Spinner templatesSpinner = (Spinner) findViewById(R.id.spinner);
        templatesSpinner.setAdapter(makeAdapterStrings(selection1));
    }
    void showSelection2(int positionOfSelection1, String defaultSelectionText){
        int position = positionOfSelection1;
        if (position == 0) {
            baseDir = BASEDIR_LIBRARIES;
            selection2 = libraryIds;
        } else if (position == selection1.length - 1) {
            Util.inputDialog(DialogId.SOURCE_EDIT_NEW_SYSTEM, R.string.source_edit_new_dialog_filename);
            return;
        } else {
            selection2 = new String[0];
            ArrayList<String> files = new ArrayList<>();
            baseDir = BASEDIR_TEMPLATES + templateIds[position - 1];
            try {
                Collections.addAll(files, getAssets().list(baseDir));
                File f = userFile(baseDir);
                if (f.exists()) {
                    for (String p : f.list())
                        if (!files.contains(p))
                            files.add(p);
                }
            } catch (IOException ignored) {
            }
            files.add(tr(R.string.source_edit_new_file));
            selection2 = files.toArray(selection2);
            baseDir += "/";
        }

        final Spinner fileSpinner = (Spinner) findViewById(R.id.spinnerfile);
        fileSpinner.setAdapter(makeAdapterStrings(selection2));
        int defaultSelection = 0;
        if (restoredStateBundle != null && position == restoredStateBundle.getInt("base"))
            defaultSelection = restoredStateBundle.getInt("file");
        else if (position > 0 || defaultSelectionText != null) {
            if (defaultSelectionText == null) defaultSelectionText = "template";
            for (int i = 0; i < selection2.length; i++)
                if (defaultSelectionText.equals(selection2[i])) {
                    defaultSelection = i;
                    break;
                }
        }
        if (defaultSelection != 0)
            fileSpinner.setSelection(defaultSelection);
    }
    void showSelection2(String defaultSelectionText){
        showSelection2(((Spinner)findViewById(R.id.spinner)).getSelectedItemPosition(), defaultSelectionText);
    }
    void showSelection2(int positionOfSelection1){
        showSelection2(positionOfSelection1, null);
    }

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setVideLibriView(R.layout.sourceedit);
        setTitle(R.string.lay_source_edit);
        restoredStateBundle = savedInstanceState;


        makeLibraryIds();
        makeTemplateIds();
        showSelection1();

        final Spinner templatesSpinner = (Spinner) findViewById(R.id.spinner);
        templatesSpinner.setOnItemSelectedListener(new AdapterView.OnItemSelectedListener() {
            @Override
            public void onItemSelected(AdapterView<?> parent, View view, int position, long id) {
                showSelection2(position);
            }

            @Override
            public void onNothingSelected(AdapterView<?> parent) {

            }
        });


        ((Spinner) findViewById(R.id.spinnerfile)).setOnItemSelectedListener(new AdapterView.OnItemSelectedListener() {
            @Override
            public void onItemSelected(AdapterView<?> parent, View view, int position, long id) {
                if (restoredStateBundle != null) {
                    if (position == restoredStateBundle.getInt("file")
                            && ((Spinner)findViewById(R.id.spinner)).getSelectedItemPosition() == restoredStateBundle.getInt("base") ) {
                        restoreEditText(restoredStateBundle);
                        restoredStateBundle = null;
                        return;
                    }
                }
                if (position == selection2.length - 1) {
                    int dialogid = ((Spinner)findViewById(R.id.spinner)).getSelectedItemPosition() == 0 ? DialogId.SOURCE_EDIT_NEW_LIB : DialogId.SOURCE_EDIT_NEW_FILE;
                    Util.inputDialog(dialogid, R.string.source_edit_new_dialog_filename);
                    return;
                }
                loadFile(baseDir + selection2[position]);
            }

            @Override
            public void onNothingSelected(AdapterView<?> parent) {

            }
        });

        findButtonById(R.id.save).setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                try {
                    writeToFile(fileName, getEditTextText(R.id.edit));
                    Spinner spinner = (Spinner) findViewById(R.id.spinner);
                    try {
                        if (spinner.getSelectedItemPosition() == 0)
                            Bridge.VLReloadLibrary(selection2[((Spinner) findViewById(R.id.spinnerfile)).getSelectedItemPosition()].replace(".xml", ""));
                        else
                            Bridge.VLReloadTemplate(templateIds[spinner.getSelectedItemPosition() - 1]);
                    } catch (Bridge.InternalError e) {
                        Util.showMessage(e.getLocalizedMessage());
                    }
                } catch (IOException e) {
                    Util.showMessage(tr(R.string.source_edit_filewritefailed, e.getLocalizedMessage()));
                }
                showFileName(true);
            }
        });

        findButtonById(R.id.reset).setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                if (hasAsset(fileName)) {
                    File f = userFile(fileName);
                    if (f.exists())
                        if (f.delete())
                            Toast.makeText(SourceEdit.this, tr(R.string.source_edit_deleted), Toast.LENGTH_SHORT).show();
                } else Toast.makeText(SourceEdit.this, tr(R.string.source_edit_nodelete), Toast.LENGTH_SHORT).show();
                loadFile();
            }
        });
    }

    private void restoreEditText(Bundle bundle) {
        EditText edit = (EditText) findViewById(R.id.edit);
        edit.setText(bundle.getString("content"));
        edit.setSelection(bundle.getInt("contentSelectionStart"), bundle.getInt("contentSelectionEnd"));
    }

    Bundle restoredStateBundle;

    @Override
    protected void onSaveInstanceState(Bundle outState) {
        super.onSaveInstanceState(outState);
        outState.putInt("base", ((Spinner)findViewById(R.id.spinner)).getSelectedItemPosition());
        outState.putInt("file", ((Spinner)findViewById(R.id.spinnerfile)).getSelectedItemPosition());
        EditText edit = (EditText) findViewById(R.id.edit);
        outState.putString("content", edit.getText().toString());
        outState.putInt("contentSelectionStart", edit.getSelectionStart());
        outState.putInt("contentSelectionEnd", edit.getSelectionEnd());
    }

    @Override
    protected void onRestoreInstanceState(Bundle savedInstanceState) {
        super.onRestoreInstanceState(savedInstanceState);
        ((Spinner)findViewById(R.id.spinner)).setSelection(savedInstanceState.getInt("base", 0));
        ((Spinner)findViewById(R.id.spinnerfile)).setSelection(savedInstanceState.getInt("file", 0));
        restoreEditText(savedInstanceState);
    }

    File userFile(String fn){
        return new File(userPath() + "/" + fn);
    }

    void loadFile(String fileName){
        this.fileName = fileName;
        loadFile();
    }

    void loadFile(){
        File f = userFile(fileName);
        try {
            if (f.exists()) {
                loadFile(new FileInputStream(f), true);
            } else {
                loadFile(getAssets().open(fileName), false);
            }
        } catch (IOException e) {
            setEditTextText(R.id.edit, "Failed to load source code");
        }
    }

    void loadFile(InputStream stream, boolean userDefined) throws IOException {
        setEditTextText(R.id.edit, IOUtils.streamToString(stream));
        showFileName(userDefined);
    }

    void showFileName(boolean userDefined) {
        setTextViewText(R.id.filename, tr(R.string.source_edit_filename, fileName) + (userDefined ? "\n" + tr(R.string.source_edit_userdefined) : ""));
    }

    boolean hasAsset(String fn)  {
        try {
            InputStream stream = getAssets().open(fn);
            if (stream != null) {
                stream.close();
                return true;
            }
        } catch (IOException ignored) {

        }
        return false;
    }

    void writeToFile(String fileName, String text) throws IOException {
        File f = userFile(fileName);
        f.getParentFile().mkdirs();
        FileWriter fw = new FileWriter(f);
        fw.write(text);
        fw.close();
        Toast.makeText(SourceEdit.this, tr(R.string.source_edit_saved), Toast.LENGTH_SHORT).show();
    }

    void writeToNewFile(String fileName, String text) throws IOException {
        if (userFile(fileName).exists())
            Util.showMessage(tr(R.string.source_edit_new_file_exists));
        else
            writeToFile(fileName, text);
    }

    @Override
    boolean onDialogResult(int dialogId, int buttonId, Bundle more) {
        String text = null;
        if (buttonId != Util.MessageHandlerCanceled && more != null) {
            text = more.getString("text");
            if (Util.isEmptyString(text)) text = null;
        }
        try {
            switch (dialogId) {
                case DialogId.SOURCE_EDIT_NEW_SYSTEM:
                    final String emptyDefaultSystem = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" +
                            "<actions>\n" +
                            "  <action id=\"update-all\">\n  </action>\n\n" +
                            "  <action id=\"renew-list\">\n  </action>\n\n\n\n" +
                            "  <action id=\"search\">\n  </action>\n\n" +
                            "  <action id=\"search-next-page\">\n  </action>\n\n" +
                            "</actions>\n";
                    if (text == null) ((Spinner)(findViewById(R.id.spinner))).setSelection(0);
                    else {
                        writeToNewFile(BASEDIR_TEMPLATES + text + "/template", emptyDefaultSystem);
                        makeTemplateIds();
                        showSelection1();
                    }
                    return true;
                case DialogId.SOURCE_EDIT_NEW_FILE:
                    if (text == null) ((Spinner)(findViewById(R.id.spinnerfile))).setSelection(0);
                    else {
                        fileName = baseDir + "/" + text;
                        writeToNewFile(fileName, "");
                        //loadFile(fileName);
                        showSelection2(text);
                    }
                    return true;
                case DialogId.SOURCE_EDIT_NEW_LIB:
                    if (text != null && text.split("_").length != 4) {
                        Util.showMessage(tr(R.string.source_edit_invalid_library_id));
                        text = null;
                    }
                    if (text == null) ((Spinner)(findViewById(R.id.spinnerfile))).setSelection(0);
                    else {
                        if (!text.endsWith(".xml")) text += ".xml";
                        Bridge.LibraryDetails ld = new Bridge.LibraryDetails();
                        ld.prettyName = tr(R.string.source_edit_new_library_default_name);
                        ld.templateId = "sru";
                        Bridge.VLSetLibraryDetails(text.substring(0, text.length() - 4), ld);
                        //loadFile(BASEDIR_LIBRARIES + text);
                        showSelection2(text);

                    }
                    return true;
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
        return super.onDialogResult(dialogId, buttonId, more);
    }
}
