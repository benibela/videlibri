package de.benibela.videlibri;

import android.app.Activity;
import android.app.AlertDialog;
import android.content.Context;
import android.content.Intent;
import android.content.res.TypedArray;
import android.graphics.Color;
import android.graphics.Rect;
import android.graphics.drawable.ColorDrawable;
import android.graphics.drawable.Drawable;
import android.graphics.drawable.StateListDrawable;
import android.graphics.drawable.TransitionDrawable;
import android.net.Uri;
import android.os.Bundle;
import android.os.Debug;
import android.util.Log;
import android.util.TypedValue;
import android.view.LayoutInflater;
import android.view.SoundEffectConstants;
import android.view.View;
import android.view.ViewGroup;
import android.widget.*;
import android.widget.BaseExpandableListAdapter;

import java.util.*;
import java.util.concurrent.RunnableFuture;


public class LibraryList extends VideLibriBaseActivity {

    static String lastSelectedLibId, lastSelectedLibShortName, lastSelectedLibName; //result of the activity
    static long lastSelectedTime = 0;                                               //(passing as intent did not work on every device (perhaps the caller is killed?)
    static final long SELECTION_REUSE_TIME = 10*1000;

    final List<String> states = new ArrayList<String>();
    final List<List<String>> cities = new ArrayList<List<String>>();
    final List<List<List<Map<String, String>>>> localLibs = new ArrayList<List<List<Map<String, String>>>>();


    ScrollView scrollView;

    LibraryListView makeLibView(){
        Bridge.Library[] libs = Bridge.getLibraries();

        states.clear(); cities.clear(); localLibs.clear();

        int autoExpand = 0;
        if (VideLibriApp.accounts != null && VideLibriApp.accounts.length > 0) {
            autoExpand = 1;
            ArrayList<String> used = new ArrayList<String>();
            states.add(tr(R.string.liblist_withaccounts));
            cities.add(new ArrayList<String>());
            cities.get(0).add(tr(R.string.liblist_withaccounts));
            localLibs.add(new ArrayList<List<Map<String,String>>>());
            localLibs.get(0).add(new ArrayList<Map<String, String>>());
            for (Bridge.Account account: VideLibriApp.accounts) {
                if (used.contains(account.libId)) continue;
                used.add(account.libId);

                TreeMap map = new TreeMap<String, String>();
                localLibs.get(0).get(localLibs.size()-1).add(map);
                for (Bridge.Library lib: libs)
                    if (lib.id.equals(account.libId)) {
                        map.put("NAME", lib.namePretty);
                        map.put("ID", lib.id);
                        map.put("SHORT", lib.nameShort);
                        break;
                    }
            }
        }

        for (Bridge.Library lib : libs) {
            if (states.isEmpty() || !states.get(states.size()-1).equals(lib.fullStatePretty)) {
                states.add(lib.fullStatePretty);
                cities.add(new ArrayList<String>());
                localLibs.add(new ArrayList<List<Map<String, String>>>());
            }
            List<String> curCities = cities.get(cities.size()-1);
            if (curCities.isEmpty() || !curCities.get(curCities.size()-1).equals(lib.locationPretty)) {
                curCities.add(lib.locationPretty);
                if ("-".equals(lib.locationPretty) && autoExpand < 2) autoExpand+=1;
                localLibs.get(localLibs.size() - 1).add(new ArrayList<Map<String, String>>());
            }
            TreeMap<String,String> map = new TreeMap<String, String>();
            localLibs.get(localLibs.size()-1).get(localLibs.get(localLibs.size()-1).size()-1).add(map);;
            map.put("NAME", lib.namePretty);
            map.put("SHORT", lib.nameShort);
            map.put("ID", lib.id);
        }

        LibraryListView lv = new LibraryListView(this);

        for (int i=0;i<autoExpand;i++) {
            lv.expand(i,false);
            if (cities.get(i).size() == 1) lv.expand(i, 0,false);
        }

        return lv;
    }

    private void onLeafClick(int state, int city, int lib){
        Map<String, String> leaf = localLibs.get(state).get(city).get(lib);
        Intent result = new Intent();
        lastSelectedLibId = leaf.get("ID");
        lastSelectedLibShortName = leaf.get("SHORT");
        lastSelectedLibName = leaf.get("NAME");
        lastSelectedTime = System.currentTimeMillis();

        setResult(RESULT_OK, result);
        LibraryList.this.finish();
    }

    boolean searchMode;

    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.chooselib);

        String reason = getIntent().getStringExtra("reason");
        if (reason != null && !"".equals(reason))
            setTextViewText(R.id.textView, reason);

        searchMode = getIntent().getBooleanExtra("search", false);

        scrollView = ((ScrollView) findViewById(R.id.libListView));
        scrollView.addView(makeLibView());

        View whynot = findViewById(R.id.textViewLibWhyNot);
        if (whynot == null) return;
        if (VideLibriApp.accounts != null && VideLibriApp.accounts.length > 0) whynot.setVisibility(View.GONE);
        else whynot.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View view) {
                Bundle args = new Bundle();
                args.putInt("id", DialogId.SPECIAL_LIBRARY_NOT_IN_LIST);
                args.putInt("special", DialogId.SPECIAL_LIBRARY_NOT_IN_LIST);
                args.putString("message", tr(R.string.foreignlibrariesnotinthelist));
                args.putIntArray("items", new int[]{R.string.foreignlibrariesnotinthelist_easy, R.string.foreignlibrariesnotinthelist_install, R.string.foreignlibrariesnotinthelist_diy, R.string.foreignlibrariesnotinthelist_mail});
                args.putIntArray("itemsSubCaption", new int[]{R.string.foreignlibrariesnotinthelist_easy_req, R.string.foreignlibrariesnotinthelist_install_req, R.string.foreignlibrariesnotinthelist_diy_req, R.string.foreignlibrariesnotinthelist_mail_req});
                Util.showDialog(LibraryList.this, args);
            }
        });

    }


    static class ViewId{
        enum Level { State, City, Lib };
        Level level;
        int a, b, c;
        ViewId(int a){ level = Level.State; this.a = a; }
        ViewId(int a, int b){ level = Level.City; this.a = a; this.b = b;}
        ViewId(int a, int b, int c){ level = Level.Lib; this.a = a; this.b = b; this.c = c; }
    };

    public class LibraryListView extends LinearLayout
    {
        TextView stateViews[];
        TextView cityViews[][];
        LinearLayout stateChildViews[];
        LinearLayout cityChildViews[][];
        //View libViews[][][];

        Drawable groupIndicator, groupIndicatorExpanded;

        LibraryListView (Context context){
            super(context);

            setOrientation(VERTICAL);

            stateViews = new TextView[states.size()];
            cityViews = new TextView[states.size()][];
            for (int i=0;i<cityViews.length;i++) cityViews[i] = new TextView[cities.get(i).size()];
            stateChildViews = new LinearLayout[states.size()];
            cityChildViews = new LinearLayout[states.size()][];
            for (int i=0;i<cityChildViews.length;i++) cityChildViews[i] = new LinearLayout[cities.get(i).size()];

            TypedValue typedValue = new TypedValue();
            if (getTheme().resolveAttribute(android.R.attr.expandableListViewStyle, typedValue , true)){
                TypedArray typedArray = getTheme().obtainStyledAttributes(typedValue.resourceId, new int[] { android.R.attr.groupIndicator });
                if (typedArray.getDrawable(0) instanceof StateListDrawable) {
                    groupIndicator = (StateListDrawable)typedArray.getDrawable(0);
                    StateListDrawable temp = (StateListDrawable)typedArray.getDrawable(0);
                    if (temp != null) {
                        temp.setState(new int[] { android.R.attr.state_expanded });
                        groupIndicatorExpanded = temp.getCurrent()  ;//.getConstantState().newDrawable();
                    }
                }
            }

            for (int i=0;i<states.size();i++) {
                TextView row = (TextView)getLayoutInflater().inflate(android.R.layout.simple_expandable_list_item_1, this, false);
                row.setBackgroundResource(android.R.drawable.list_selector_background);
                row.setText(states.get(i));
                row.setCompoundDrawablesWithIntrinsicBounds(groupIndicator,null,null,null);
                row.setTag(new ViewId(i));
                row.setOnClickListener(combinedListener);
                stateViews[i] = (TextView) row;
                addView(row);
                stateChildViews[i] = new LinearLayout(context);
                stateChildViews[i].setLayoutParams(new LayoutParams(LayoutParams.FILL_PARENT, LayoutParams.WRAP_CONTENT));
                stateChildViews[i].setOrientation(VERTICAL);
                addView(stateChildViews[i]);
            }

        }

        boolean isExpanded(int state){
            return stateChildViews[state] != null && stateChildViews[state].getChildCount() > 0 && stateChildViews[state].isShown();
        }
        boolean isExpanded(int state, int city){
            return cityChildViews[state][city] != null && cityChildViews[state][city].getChildCount() > 0 && cityChildViews[state][city].isShown();
        }


        void expand(final int state, boolean scroll){
            if (stateChildViews[state].getChildCount() == 0) {
                for (int b = 0; b < cities.get(state).size(); b ++) {
                    View row = getLayoutInflater().inflate(R.layout.librarycityinlistview, this, false);
                    ((TextView) row).setText(cities.get(state).get(b));
                    ((TextView) row).setCompoundDrawablesWithIntrinsicBounds(groupIndicator,null,null,null);
                    //l.setPadding((int)(60 * getResources().getDisplayMetrics().density), l.getPaddingTop(), l.getPaddingRight(), l.getPaddingBottom());
                    row.setTag(new ViewId(state, b));
                    row.setOnClickListener(combinedListener);
                    stateChildViews[state].addView(row);
                    cityViews[state][b] = (TextView) row;

                    cityChildViews[state][b] = new LinearLayout(getContext());
                    cityChildViews[state][b].setLayoutParams(new LayoutParams(LayoutParams.FILL_PARENT,LayoutParams.WRAP_CONTENT));
                    cityChildViews[state][b].setOrientation(VERTICAL);
                    stateChildViews[state].addView(cityChildViews[state][b]);
                }
            }
            stateChildViews[state].setVisibility(VISIBLE);

            setIndicator(stateViews[state], true);

            if (scroll)
                smoothScrollTo(cityChildViews[state].length > 0 && isExpanded(state, cityChildViews[state].length-1)
                           ? cityChildViews[state][cityChildViews[state].length-1]
                           : stateChildViews[state], stateViews[state]);
        }
        void expand(final int state, final int city, boolean scroll){
            if (cityChildViews[state][city] == null) expand(state, scroll);
            if (cityChildViews[state][city] == null) return;
            if (cityChildViews[state][city].getChildCount() == 0) {
                for (int libId = 0; libId < localLibs.get(state).get(city).size(); libId ++) {
                    View row = getLayoutInflater().inflate(R.layout.libraryinlistview, this, false);
                    ((TextView) row).setText(localLibs.get(state).get(city).get(libId).get("NAME"));
                    row.setTag(new ViewId(state, city, libId));
                    row.setOnClickListener(combinedListener);
                    cityChildViews[state][city].addView(row);
                }
            }
            cityChildViews[state][city].setVisibility(VISIBLE);
            setIndicator(cityViews[state][city], true);
            if (scroll)
                smoothScrollTo(cityChildViews[state][city], cityViews[state][city]);
        }

        void collapse(int state){
            stateChildViews[state].setVisibility(GONE);
            setIndicator(stateViews[state], false);
        }
        void collapse(int state, int city){
            cityChildViews[state][city].setVisibility(GONE);
            setIndicator(cityViews[state][city], false);
        }

        private void setIndicator(final TextView view, final boolean expanded){
            playSoundEffect(SoundEffectConstants.CLICK);
            view.postDelayed(new Runnable() {
                @Override
                public void run() {
                    view.setCompoundDrawablesWithIntrinsicBounds(expanded ? groupIndicatorExpanded : groupIndicator, null, null, null);
                }
            }, 150);

            /*TransitionDrawable trans = new TransitionDrawable(new ColorDrawable[]{new ColorDrawable(Color.BLACK), new ColorDrawable(Color.BLUE), new ColorDrawable(Color.BLACK)});
            view.setBackgroundDrawable(trans);
            trans.startTransition(500);                             */
        }

        OnClickListener combinedListener = new OnClickListener() {
            @Override
            public void onClick(View view) {
                Object tag = view.getTag();
                if (!(tag instanceof ViewId)) return;
                ViewId id = (ViewId) tag;
                switch (id.level) {
                    case State:
                        if (!isExpanded(id.a)) {
                            expand(id.a,true);
                            if (cities.get(id.a).size() == 1 && !isExpanded(id.a, 0)) expand(id.a, 0,true);
                        } else collapse(id.a);

                        break;
                    case City: if (!isExpanded(id.a, id.b)) expand(id.a, id.b,true); else collapse(id.a, id.b); break;
                    case Lib: onLeafClick(id.a, id.b, id.c); break;
                }
            }
        };

        private void offsetParent(Rect r, View view){
            if (view.getParent() != LibraryListView.this && view.getParent() instanceof View) {
                Rect temp = new Rect();
                ((View)view.getParent()).getHitRect(temp);
                r.top += temp.top;
                r.bottom += temp.top;
            }
        }

        private void smoothScrollTo(final View view, final View header){
            view.postDelayed(new Runnable() {
                @Override
                public void run() {
                    Rect viewRect = new Rect(), scrollRect = new Rect();
                    view.getHitRect(viewRect);
                    offsetParent(viewRect, view);
                    scrollView.getDrawingRect(scrollRect);

                    //Log.i("VL", "center: "+center+ " temp "+temp.top+" "+temp.bottom);

                    if (viewRect.bottom >= scrollRect.top && viewRect.bottom <= scrollRect.bottom) return;
                    Rect headerRect = new Rect();
                    header.getHitRect(headerRect);
                    offsetParent(headerRect, header);

                    int target;
                    if (headerRect.height() + viewRect.height() + 20 /* arbitrary offset */ >= scrollRect.height() )
                        target = headerRect.top;
                    else
                        target = viewRect.bottom - scrollRect.height();

                    scrollView.smoothScrollTo(getMeasuredWidth() / 2, target);
                }
            }, 150); //wait for post onMeasure
        }

    }


    @Override
    boolean onDialogResult(int dialogId, int buttonId, Bundle more) {
        switch (dialogId) {
            case DialogId.SPECIAL_LIBRARY_NOT_IN_LIST:
                Intent intent;
                switch (buttonId) {
                    case 0:case 1:
                        intent = new Intent(this, NewLibrary.class);
                        intent.putExtra("mode", buttonId == 1 ? 0 : NewLibrary.MODE_LIBRARY_ENTER_NEW_DATA);
                        startActivity(intent);
                        break;
                    case 2:
                        startActivity(new Intent(Intent.ACTION_VIEW, Uri.parse("http://www.videlibri.de/help/neuebibliothek.html")));
                        break;
                    case 3:
                        intent = new Intent(this, Feedback.class);
                        startActivity(intent);
                        break;
                }
                return true;
        }
        return super.onDialogResult(dialogId, buttonId, more);
    }
}
