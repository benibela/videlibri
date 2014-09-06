package de.benibela.videlibri;

import android.app.Activity;
import android.app.AlertDialog;
import android.content.Context;
import android.content.Intent;
import android.os.Bundle;
import android.util.Log;
import android.util.TypedValue;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.*;
import android.widget.BaseExpandableListAdapter;

import java.util.*;


public class LibraryList extends VideLibriBaseActivity {

    static String lastSelectedLibId, lastSelectedLibShortName, lastSelectedLibName; //result of the activity
    static long lastSelectedTime = 0;                                               //(passing as intent did not work on every device (perhaps the caller is killed?)
    static final long SELECTION_REUSE_TIME = 10*1000;

    final List<String> states = new ArrayList<String>();
    final List<List<String>> cities = new ArrayList<List<String>>();
    final List<List<List<Map<String, String>>>> localLibs = new ArrayList<List<List<Map<String, String>>>>();

    ExpandableListView topLv;
    int rowHeight = 20;

    void makeLibView(final ExpandableListView lv){
        Bridge.Library[] libs = Bridge.getLibraries();

        states.clear(); cities.clear(); localLibs.clear();

        //final TreeMap<String, String> shortNames = new TreeMap<String, String>();
        //final TreeMap<String, String> ids = new TreeMap<String, String>();

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

        lv.setAdapter(new LibraryListAdapter());

        for (int i=0;i<autoExpand;i++)
            lv.expandGroup(i);

        lv.setOnChildClickListener(new ExpandableListView.OnChildClickListener() {
            @Override
            public boolean onChildClick(ExpandableListView expandableListView, View view, int i, int i2, long l) {
                Log.i("VIDELIBRI 1st level child click", "::"+i+" "+i2+ " "+l);
                return false;
            }
        });
        lv.setOnGroupClickListener(new ExpandableListView.OnGroupClickListener() {
            @Override
            public boolean onGroupClick(ExpandableListView expandableListView, View view, int i, long l) {
                Log.i("VIDELIBRI 1st level group click", "::"+i+" "+l);
                return false;
            }
        });  // */
        /*lv.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View view) {
                Log.i("VIDELIBRI 1st level click", "");
            }
        });*/
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


        TypedValue typedValue = new TypedValue();
        if (getTheme().resolveAttribute(android.R.attr.listPreferredItemHeight, typedValue, true))
            if (typedValue.type == TypedValue.TYPE_DIMENSION)
                rowHeight = (int) typedValue.getDimension(getResources().getDisplayMetrics());

        ExpandableListView lv = (ExpandableListView) findViewById(R.id.libListView);
        topLv = lv;
        makeLibView(lv);
    }



    public abstract class LibraryListBaseAdapter extends BaseExpandableListAdapter{
        @Override
        public Object getChild(int groupPosition, int childPosition)
        {
            return getChildId(groupPosition, childPosition);
        }

        @Override
        public long getChildId(int groupPosition, int childPosition)
        {
            return (groupPosition << 15) + childPosition + 1;
        }

        @Override
        public Object getGroup(int groupPosition)
        {
            return getGroupId(groupPosition);
        }

        @Override
        public long getGroupId(int groupPosition)
        {
            return (groupPosition << 15);
        }

        @Override
        public boolean hasStableIds()
        {
            return true;
        }

        @Override
        public boolean isChildSelectable(int groupPosition, int childPosition)
        {
            return true;
        }
    }


    public class LibraryListAdapter extends LibraryListBaseAdapter
    {
        Map<Integer, ExpandableListView> cache = new TreeMap<Integer, ExpandableListView>();
        @Override
        public View getChildView(final int groupPosition, final int childPosition,
                                 boolean isLastChild, View convertView, ViewGroup parent)
        {
            if (cache.containsKey(groupPosition*10000 + childPosition))
                return cache.get(groupPosition*10000 + childPosition);
            final LibraryListCityView l = new LibraryListCityView();
            l.setScrollContainer(false);
            l.setPadding((int)(60 * getResources().getDisplayMetrics().density), l.getPaddingTop(), l.getPaddingRight(), l.getPaddingBottom());
            //l.setLayoutParams(new ViewGroup.LayoutParams(ViewGroup.LayoutParams.FILL_PARENT, ViewGroup.LayoutParams.WRAP_CONTENT));
            //l.setFocusable(true);
            //l.setDescendantFocusability(ListView.FOCUS_BLOCK_DESCENDANTS);
            l.rows = 1;
            l.setOnGroupExpandListener(new ExpandableListView.OnGroupExpandListener() {
                @Override
                public void onGroupExpand(int i) {
                    l.rows += localLibs.get(groupPosition).get(childPosition).size() ;
                }
            });
            l.setOnGroupCollapseListener(new ExpandableListView.OnGroupCollapseListener() {
                @Override
                public void onGroupCollapse(int i) {
                    l.rows -= localLibs.get(groupPosition).get(childPosition).size();
                }
            });

            l.setOnChildClickListener(new ExpandableListView.OnChildClickListener() {
                @Override
                public boolean onChildClick(ExpandableListView expandableListView, View view, int i, int i2, long l) {
                    Log.i("VIDELIBRI 2 level child click"+groupPosition + " "+childPosition, "::"+i+" "+i2+ " "+l);
                    return false;
                }
            });
            l.setOnGroupClickListener(new ExpandableListView.OnGroupClickListener() {
                @Override
                public boolean onGroupClick(ExpandableListView expandableListView, View view, int i, long l) {
                    Log.i("VIDELIBRI 2  level group click"+groupPosition + " "+childPosition, "::"+i+" "+l);
                    return false;
                }
            });    //    */
            /*l.setOnClickListener(new View.OnClickListener() {
                @Override
                public void onClick(View view) {
                    Log.i("VIDELIBRI 2 level click"+groupPosition + " "+childPosition, "");
                }
            });      */


            l.setAdapter(new LibraryListCityAdapter(l, groupPosition,childPosition));
            if (cities.get(groupPosition).size() == 1) l.expandGroup(0);

            l.setOnChildClickListener(new ExpandableListView.OnChildClickListener() {
                @Override
                public boolean onChildClick(ExpandableListView expandableListView, View view, int i, int lib, long l) {
                    onLeafClick(groupPosition,childPosition,lib);
                    return false;
                }
            });

            cache.put(groupPosition*10000 + childPosition, l);
            return l;
        }

        @Override
        public int getChildrenCount(int groupPosition)
        {
            return cities.get(groupPosition).size();
        }

        @Override
        public int getGroupCount()
        {
            return states.size();
        }

        @Override
        public View getGroupView(int groupPosition, boolean isExpanded,
                                 View convertView, ViewGroup parent)
        {
            View row = //convertView != null && convertView instanceof TextView ? convertView :
                    getLayoutInflater().inflate(android.R.layout.simple_expandable_list_item_1, parent, false);
            ((TextView) row).setText(states.get(groupPosition));
            return row;
        }
    }

    public class LibraryListCityView extends ExpandableListView
    {
        //int intGroupPosition, intChildPosition, intGroupid;
        int rows;
        public LibraryListCityView()
        {
            super(LibraryList.this);
        }
        protected void onMeasure(int widthMeasureSpec, int heightMeasureSpec)
        {
            widthMeasureSpec = MeasureSpec.makeMeasureSpec(MeasureSpec.getSize(widthMeasureSpec), MeasureSpec.EXACTLY);
            heightMeasureSpec = MeasureSpec.makeMeasureSpec(rows * rowHeight, MeasureSpec.EXACTLY);
            Log.i("VL", "rowheight: "+rowHeight+" rows: "+rows);
            super.onMeasure(widthMeasureSpec, heightMeasureSpec);
        }

        //from http://stackoverflow.com/questions/19298155/issue-with-expanding-multi-level-expandablelistview
        @Override
        protected void onDetachedFromWindow() {
            try {
                super.onDetachedFromWindow();
            } catch (IllegalArgumentException e) {
                // TODO: Workaround for http://code.google.com/p/android/issues/detail?id=22751
            }
        }
    }

    public class LibraryListCityAdapter extends LibraryListBaseAdapter
    {
        int state,city;
        LibraryListCityView lv;

        LibraryListCityAdapter (LibraryListCityView parent, int a, int b) {
            lv = parent;
            state = a;
            city = b;
        }

        @Override
        public View getChildView(final int groupPosition, final int childPosition,
                                 boolean isLastChild, View convertView, ViewGroup parent)
        {
            //todo: use convertView (but do not want to mix groupView/childView up)
            View row = getLayoutInflater().inflate(R.layout.libraryinlistview , parent, false);
            ((TextView) row).setText(localLibs.get(state).get(city).get(childPosition).get("NAME"));
            row.setOnClickListener(new View.OnClickListener() {
                @Override
                public void onClick(View view) {
                    //Log.i("VideLibri tv 3rd level", groupPosition+ " "+childPosition);
                    onLeafClick(state, city, childPosition);
                }
            });

            return row;
        }

        @Override
        public int getChildrenCount(int groupPosition)
        {
            return localLibs.get(state).get(city).size();
        }


        @Override
        public int getGroupCount()
        {
            return 1;
        }

        @Override
        public View getGroupView(final int groupPosition, final boolean isExpanded,
                                 View convertView, ViewGroup parent)
        {
            final View row = getLayoutInflater().inflate(R.layout.librarycityinlistview, parent, false);
            ((TextView) row).setText(cities.get(state).get(city));
             row.setOnClickListener(new View.OnClickListener() {
                @Override
                public void onClick(View view) {
                    Log.i("VideLibri tv 2nd level", ""+groupPosition);
                    if (lv.isGroupExpanded(groupPosition)) lv.collapseGroup(groupPosition);
                    else lv.expandGroup(groupPosition/*, true needs api lvl 14*/);
                    row.postDelayed(new Runnable() {
                        @Override
                        public void run() {
                            topLv.requestLayout();
                            topLv.invalidate();
                        }
                    }, 100);
                }
            });
            return row;
        }

    }

}
