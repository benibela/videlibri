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
import java.util.Collections;

import de.benibela.videlibri.jni.Bridge;
import okhttp3.internal.http.BridgeInterceptor;

public class SourceEdit extends VideLibriBaseActivity{
    String[] libraryIds, templateIds;

    String baseDir = "";
    String fileName = "";
    String[] selection2;


    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setVideLibriView(R.layout.sourceedit);
        setTitle(R.string.lay_source_edit);
        restoredStateBundle = savedInstanceState;

        libraryIds = Bridge.VLGetLibraryIds();
        for (int i=0;i<libraryIds.length;i++)
            libraryIds[i] = libraryIds[i] + ".xml";
        templateIds = Bridge.VLGetTemplates();

        final String [] selection1 = new String[templateIds.length+1];
        selection1[0] = tr(R.string.lay_source_edit_library_list);
        for (int i=1;i<selection1.length;i++)
            selection1[i] = tr(R.string.lay_source_edit_template, templateIds[i-1]);

        final Spinner templatesSpinner = (Spinner) findViewById(R.id.spinner);
        templatesSpinner.setAdapter(makeAdapterStrings(selection1));
        templatesSpinner.setOnItemSelectedListener(new AdapterView.OnItemSelectedListener() {
            @Override
            public void onItemSelected(AdapterView<?> parent, View view, int position, long id) {
                if (position == 0) {
                    baseDir = "libraries/";
                    selection2 = libraryIds;
                } else {
                    baseDir = "libraries/templates/" + templateIds[position - 1];
                    selection2 = new String[0];
                    ArrayList<String> files = new ArrayList<>();
                    try {
                        selection2 = getAssets().list(baseDir);
                        File f = userFile(baseDir);
                        if (f.exists()) {
                            ArrayList<String> a = new ArrayList<>();
                            Collections.addAll(a, selection2);
                            for (String p : f.list())
                                if (!a.contains(p)) a.add(p);
                            selection2 = a.toArray(selection2);
                        }
                    } catch (IOException ignored) {
                    }
                    baseDir += "/";
                }

                final Spinner fileSpinner = (Spinner) findViewById(R.id.spinnerfile);
                fileSpinner.setAdapter(makeAdapterStrings(selection2));
                int defaultSelection = 0;
                if (restoredStateBundle != null && position == restoredStateBundle.getInt("base"))
                    defaultSelection = restoredStateBundle.getInt("file");
                else if (position > 0)
                    for (int i=0;i<selection2.length;i++) if ("template".equals(selection2[i])) { defaultSelection = i; break; }
                if (defaultSelection != 0)
                    fileSpinner.setSelection(defaultSelection);
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
                fileName = baseDir + selection2[position];
                loadFile();
            }

            @Override
            public void onNothingSelected(AdapterView<?> parent) {

            }
        });

        findButtonById(R.id.save).setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                try {
                    File f = userFile(fileName);
                    f.getParentFile().mkdirs();
                    FileWriter fw = new FileWriter(f);
                    fw.write(getEditTextText(R.id.edit));
                    fw.close();
                    Toast.makeText(SourceEdit.this, tr(R.string.source_edit_saved), Toast.LENGTH_SHORT).show();
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
}
