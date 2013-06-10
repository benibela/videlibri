package de.benibela.videlibri;


import android.os.Bundle;
import android.view.View;
import android.widget.Spinner;

public class NewLibrary extends VideLibriBaseActivity{


    static final int MODE_LIBRARY_MODIFY = 1237;

    static NewLibrary currentNewLibrary = null;
    Bridge.LibraryDetails details;

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

        int mode = getIntent().getIntExtra("mode", 0);
        if (mode == MODE_LIBRARY_MODIFY) {
            final String id = getIntent().getStringExtra("libId");
            if (id == null) return;
            details = Bridge.VLGetLibraryDetails(id);
            if (details == null) return;

            findViewById(R.id.createGroup).setVisibility(View.GONE);
            findButtonById(R.id.create).setText("ändern");

            setTextViewText(R.id.id, id);
            setTextViewText(R.id.name, details.prettyName);
            //((Spinner) findViewById(R.id.template)).setitem
            /*ArrayAdapter<CharSequence> adapter = ArrayAdapter.createFromResource(this,
        R.array.planets_array, android.R.layout.simple_spinner_item);
// Specify the layout to use when the list of choices appears
adapter.setDropDownViewResource(android.R.layout.simple_spinner_dropdown_item);
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
        }
    }

    @Override
    protected void onResume() {
        super.onResume();
        currentNewLibrary = this;
    }

    @Override
    protected void onPause() {
        currentNewLibrary = null;
        super.onPause();
    }
}
