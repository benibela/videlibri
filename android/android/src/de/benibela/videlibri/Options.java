package de.benibela.videlibri;

import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.*;

import java.util.ArrayList;

/**
 * Created with IntelliJ IDEA.
 * User: benito
 * Date: 5/20/13
 * Time: 5:33 PM
 * To change this template use File | Settings | File Templates.
 */
public class Options extends VideLibriBaseActivity{
    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);    //To change body of overridden methods use File | Settings | File Templates.
        setContentView(R.layout.options);




    }

    @Override
    protected void onResume() {
        super.onResume();    //To change body of overridden methods use File | Settings | File Templates.

        if (VideLibri.instance != null) {
            ArrayList<String> accounts = new ArrayList<String>();

            LinearLayout linearLayout = (LinearLayout) findViewById(R.id.viewaccounts);
            linearLayout.removeAllViews();
            for (final Bridge.Account acc: VideLibri.instance.accounts) if (acc != null) {
                CheckBox viewAcc = new CheckBox(this);
                viewAcc.setText(acc.prettyName);
                viewAcc.setChecked(!VideLibri.instance.hiddenAccounts.contains(acc));
                viewAcc.setOnCheckedChangeListener(new CompoundButton.OnCheckedChangeListener() {
                    @Override
                    public void onCheckedChanged(CompoundButton compoundButton, boolean b) {
                        if (!b == VideLibri.instance.hiddenAccounts.contains(acc)) return;
                        if (!b) VideLibri.instance.hiddenAccounts.add(acc);
                        else VideLibri.instance.hiddenAccounts.remove(acc);
                    }
                });
                linearLayout.addView(viewAcc);
            }

            linearLayout = (LinearLayout) findViewById(R.id.accounts);
            linearLayout.removeAllViews();
            LayoutInflater inflater = getLayoutInflater();

            for (Bridge.Account acc: VideLibri.instance.accounts) if (acc != null) {
                Button btn = (Button) inflater.inflate(R.layout.insetbutton, null);
                btn.setText(acc.prettyName);
                linearLayout.addView(btn);
            }

            if (!VideLibri.instance.displayHistory) ((RadioButton) findViewById(R.id.radioButton1)).setChecked(true);
            else ((RadioButton) findViewById(R.id.radioButton2)).setChecked(true);

            findButtonById(R.id.newaccount).setOnClickListener(new View.OnClickListener() {
                @Override
                public void onClick(View view) {
                    VideLibri.instance.newAccountDialog(false);
                }
            });
        }
    }

    @Override
    protected void onPause() {
        super.onPause();    //To change body of overridden methods use File | Settings | File Templates.

        if (VideLibri.instance == null) return;

        VideLibri.instance.displayHistory = ((RadioButton) findViewById(R.id.radioButton2)).isChecked();


    }
}
