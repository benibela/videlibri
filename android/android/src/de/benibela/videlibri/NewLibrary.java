package de.benibela.videlibri;


import android.os.Bundle;
import android.sax.TextElementListener;
import android.util.Pair;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.*;

import java.lang.reflect.Array;
import java.util.HashMap;
import java.util.Map;

public class NewLibrary extends VideLibriBaseActivity{


    static final int MODE_LIBRARY_MODIFY = 1237;

    Bridge.LibraryDetails details;


    HashMap<String, Pair<String, EditText>> variables = new HashMap<String, Pair<String, EditText>>();

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);    //To change body of overridden methods use File | Settings | File Templates

        setContentView(R.layout.newlib);

        findButtonById(R.id.install).setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View view) {
                Bridge.VLInstallLibrary(getTextViewText(R.id.url));
                setLoading(true);
            }
        });

        final String [] templates = Bridge.VLGetTemplates();
        final Spinner templatesSpinner = (Spinner) findViewById(R.id.template);
        ArrayAdapter<String> adapter = new ArrayAdapter(this, android.R.layout.simple_spinner_item, templates);
        adapter.setDropDownViewResource(android.R.layout.simple_spinner_dropdown_item);
        templatesSpinner.setAdapter(adapter);

        templatesSpinner.setOnItemSelectedListener(new AdapterView.OnItemSelectedListener() {
            @Override
            public void onItemSelected(AdapterView<?> adapterView, View view, int i, long l) {
                if (i < 0 || i >= templates.length) return;
                selectTemplate(templates[i]);
            }

            @Override
            public void onNothingSelected(AdapterView<?> adapterView) {

            }
        });


        final int mode = getIntent().getIntExtra("mode", 0);
        if (mode == MODE_LIBRARY_MODIFY) {
            final String id = getIntent().getStringExtra("libId");
            if (id == null) return;
            details = Bridge.VLGetLibraryDetails(id);
            if (details == null) return;

            for (int i=0;i<details.variableNames.length;i++)
                variables.put(details.variableNames[i], new Pair<String, EditText>(details.variableValues[i], null));
            for (int i=0;i<templates.length;i++)
                if (details.templateId.equals(templates[i])) {
                    templatesSpinner.setSelection(i);
                    break;
                }
            //selectTemplate(details.templateId);

            findViewById(R.id.createGroup).setVisibility(View.GONE);
            findButtonById(R.id.create).setText("ändern");

            setTextViewText(R.id.id, id);
            setTextViewText(R.id.name, details.prettyName);
            //(
            /*ArrayAdapter<CharSequence> adapter = ArrayAdapter.createFromResource(this,
        R.array.planets_array, android.R.layout.simple_spinner_item);
// Specify the layout to use when the list of choices appears
adapter.
// Apply the adapter to the spinner
spinner.setAdapter(adapter);*/

            findViewById(R.id.create).setOnClickListener(new View.OnClickListener() {
                @Override
                public void onClick(View view) {
                    String newId = getTextViewText(R.id.id);
                    int sublocs = newId.split("_").length;
                    for (int i=sublocs; i < 4;i++) newId = "-_" + newId;

                    details.prettyName = getTextViewText(R.id.name);
                    if (newId != id) Bridge.VLSetLibraryDetails(id, null);

                    Bridge.VLSetLibraryDetails(newId, details);
                    finish();
                }
            });
            findViewById(R.id.deleteButton).setOnClickListener(new View.OnClickListener() {
                @Override
                public void onClick(View view) {
                    Bridge.VLSetLibraryDetails(id, null);
                    finish();
                }
            });
        } else setTextViewText(R.id.id, "user"+(int)(Math.random()*1000));

        findViewById(R.id.create).setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View view) {
                String newId = getTextViewText(R.id.id);
                int sublocs = newId.split("_").length;
                for (int i=sublocs; i < 4;i++) newId = "-_" + newId;

                if (details == null) details = new Bridge.LibraryDetails();

                details.prettyName = getTextViewText(R.id.name);
                if (mode == MODE_LIBRARY_MODIFY && getIntent().getStringExtra("libId") != null) {
                    String id = getIntent().getStringExtra("libId");
                    if (!newId.equals(id)) Bridge.VLSetLibraryDetails(id, null);
                }
                details.id = newId;
                details.templateId = templatesSpinner.getSelectedItem().toString();

                details.variableNames = new String[variables.size()];
                details.variableValues = new String[variables.size()];
                int i = 0;
                for (Map.Entry<String, Pair<String, EditText>> e: variables.entrySet()){
                    details.variableNames[i] = e.getKey();
                    if (e.getValue().second == null) details.variableValues[i] = e.getValue().first;
                    else details.variableValues[i] = e.getValue().second.getText().toString();
                    i+=1;
                }


                Bridge.VLSetLibraryDetails(newId, details);
                finish();
            }
        });

    }

    EditText addTemplateVariable(LinearLayout linearLayout, LayoutInflater inflater, String name, String desc, String value){
        View option = inflater.inflate(R.layout.newliboption, null);
        TextView text = (TextView) option.findViewById(R.id.text);
        text.setText(desc == null ? name : (name +  " ( "+desc+")"));
        EditText edit = (EditText) option.findViewById(R.id.edit);
        if (value == null) value = "";
        edit.setText(value);
        linearLayout.addView(option);
        variables.put(name, new Pair<String, EditText>(value, edit));
        return edit;
    }

    void selectTemplate(String template){
        Bridge.TemplateDetails details = Bridge.VLGetTemplateDetails(template);
        if (details == null) return;

        HashMap<String, Pair<String, EditText>> oldVariables = variables;
        variables = new HashMap<String, Pair<String, EditText>>();

        HashMap<String, String> oldValues = new HashMap<String, String>();

        for (Map.Entry<String, Pair<String, EditText>> e: oldVariables.entrySet() )
            if (e.getValue().second == null) oldValues.put(e.getKey(), e.getValue().first);
            else if (!e.getValue().second.getText().toString().equals(e.getValue().first))
                oldValues.put(e.getKey(), e.getValue().second.getText().toString());

        LinearLayout linearLayout = (LinearLayout) findViewById(R.id.options);
        linearLayout.removeAllViews();

        LayoutInflater inflater = getLayoutInflater();
        for (int i=0;i<details.variablesNames.length;i++) {
            EditText et = addTemplateVariable(    linearLayout, inflater, details.variablesNames[i], details.variablesDescription[i], details.variablesDefault[i]);
            if (oldValues.containsKey(details.variablesNames[i])) et.setText(oldValues.get(details.variablesNames[i]));
        }

        for (Map.Entry<String, String> e: oldValues.entrySet() )
            if (!variables.containsKey(e.getKey()))
                addTemplateVariable(linearLayout, inflater, e.getKey(), null, "").setText(e.getValue());
    }
}
