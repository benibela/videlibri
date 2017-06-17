package de.benibela.videlibri;


import android.app.Activity;
import android.content.DialogInterface;
import android.content.Intent;
import android.graphics.Paint;
import android.os.Bundle;
import android.util.Log;
import android.view.View;
import android.widget.ArrayAdapter;
import android.widget.Button;
import android.widget.Spinner;
import android.widget.TextView;
import android.view.Menu;
import android.view.MenuItem;


import java.util.ArrayList;
import java.util.Arrays;
import java.util.Stack;

public class Search extends VideLibriBaseActivity implements Bridge.SearchEventHandler{
    static final int REQUEST_CHOOSE_LIBRARY = 1234;
    static ArrayList< Bridge.SearcherAccess> searchers = new ArrayList<Bridge.SearcherAccess>();

    String libId, libName;

    Bridge.SearcherAccess searcher;

    static final int SEARCHER_HEARTH_BEAT_TIMEOUT = 5*60*1000;
    static final int SEARCHER_STATE_INIT = 0; //connecting
    static final int SEARCHER_STATE_CONNECTED = 1;
    static final int SEARCHER_STATE_SEARCHING = 2;
    static final int SEARCHER_STATE_FAILED = 3;

    static public void gcSearchers(){
        for (int i=searchers.size()-1;i>=0;i--)  {
            //Log.d("VideLibri", " GC Searcher: " + i + "/"+searchers.size()+ ": "+searchers.get(i).nativePtr+" // "+(System.currentTimeMillis() - searchers.get(i).heartBeat));
            if (System.currentTimeMillis() - searchers.get(i).heartBeat > SEARCHER_HEARTH_BEAT_TIMEOUT
                    || searchers.get(i).state == SEARCHER_STATE_FAILED) {
                searchers.get(i).free();
                searchers.remove(i);
            }
        }
    }



    private void obtainSearcher(){
        gcSearchers();
        if (searchers.size() > 0) {
            Bridge.SearcherAccess candidate = searchers.get(searchers.size() - 1);
            if (candidate.libId.equals(libId))
                switch (candidate.state) {
                    case SEARCHER_STATE_INIT:
                    case SEARCHER_STATE_CONNECTED:
                        searcher = candidate;
                        return;
                };
        }
        searcher = new Bridge.SearcherAccess(libId);
        searcher.heartBeat = System.currentTimeMillis();
        searcher.state = SEARCHER_STATE_INIT;
        searcher.connect();
        if (searcher.nativePtr != 0){ //if
            beginLoading(LOADING_SEARCH_CONNECTING);
            searchers.add(searcher);
        }
    }

    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.searchlayout);

        if (savedInstanceState != null) {
            libId = savedInstanceState.getString("libId");
            libName = savedInstanceState.getString("libName");
        } else {
            libId = getIntent().getStringExtra("libId");
            libName = getIntent().getStringExtra("libName");
        }

        TextView lib = ((TextView) findViewById(R.id.library));
        lib.setText(libName + " ("+tr(R.string.change)+")");
        lib.setPaintFlags(lib.getPaintFlags() | Paint.UNDERLINE_TEXT_FLAG);
        if (libId == null || libId.equals("") || (getIntent().getBooleanExtra("showLibList", false) && savedInstanceState == null )) {
            if ((System.currentTimeMillis() - LibraryList.lastSelectedTime) < LibraryList.SELECTION_REUSE_TIME){
                libId = LibraryList.lastSelectedLibId;
                libName = LibraryList.lastSelectedLibName;
            } else {
                libId = "";
                changeSearchLib();
            }
        }

        ((TextView) findViewById(R.id.library)).setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View view) {
                changeSearchLib();
            }
        });

        ((Button) findViewById(R.id.button)).setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View view) {
                obtainSearcher();

                if ("debug".equals(getTextViewText(R.id.year))) {
                    activateHiddenDebugMode();
                    return;
                }

                Intent intent = new Intent(Search.this, SearchResult.class);
                Bridge.Book book = new Bridge.Book();
                book.title = getTextViewText(R.id.title);
                book.author = getTextViewText(R.id.author);
                book.setProperty("keywords", getTextViewText(R.id.keywords));
                book.setProperty("year", getTextViewText(R.id.year));
                book.setProperty("isbn", getTextViewText(R.id.isbn));
                intent.putExtra("searchQuery", book);
                if (findViewById(R.id.homeBranchLayout).getVisibility() != View.GONE)
                    intent.putExtra("homeBranch", ((Spinner)findViewById(R.id.homeBranch)).getSelectedItemPosition());
                if (findViewById(R.id.searchBranchLayout).getVisibility() != View.GONE)
                    intent.putExtra("searchBranch", ((Spinner)findViewById(R.id.searchBranch)).getSelectedItemPosition());
                startActivity(intent);
            }
        });

        setTitle(tr(R.string.search_title));

        if (libId != null && !libId.equals("")) {
            obtainSearcher();
            setBranchViewes();
        }
    }

    @Override
    protected void onResume() {
        super.onResume();
        obtainSearcher();
        if (searcher != null && searcher.nativePtr != 0) {
            for (Bridge.SearchEvent event: searcher.pendingEvents)
                onSearchEvent(searcher, event);
        }
        if (searcher != null){
            if (searcher.pendingEvents != null)
                searcher.pendingEvents.clear();
            if (searcher.state != SEARCHER_STATE_INIT)
                endLoadingAll(LOADING_SEARCH_CONNECTING);
        }
    }

    @Override
    public boolean onPrepareOptionsMenu(Menu menu) {
        boolean x= super.onPrepareOptionsMenu(menu);
        menu.findItem(R.id.search).setVisible(false);
        return x;
    }

    @Override
    protected void onDestroy() {
        gcSearchers();
        super.onDestroy();
    }


    void changeSearchLib(){
        Intent intent = new Intent(this, LibraryList.class);
        intent.putExtra("defaultLibId", libId);
        intent.putExtra("reason", tr(R.string.search_selectlib));
        intent.putExtra("search", true);
        startActivityForResult(intent, REQUEST_CHOOSE_LIBRARY);
    }

    void setBranchViewes( ) {
        String[] homeBranches = searcher.homeBranches;
        String[] searchBranches = searcher.searchBranches;
        if (homeBranches == null || homeBranches.length == 0)
            findViewById(R.id.homeBranchLayout).setVisibility(View.GONE);
        else {
            findViewById(R.id.homeBranchLayout).setVisibility(View.VISIBLE);
            ArrayAdapter<String> adapter = new ArrayAdapter<String>(Search.this, android.R.layout.simple_spinner_item, homeBranches);
            adapter.setDropDownViewResource(android.R.layout.simple_spinner_dropdown_item);
            ((Spinner) findViewById(R.id.homeBranch)).setAdapter(adapter);
        }
        if (searchBranches == null || searchBranches.length == 0)
            findViewById(R.id.searchBranchLayout).setVisibility(View.GONE);
        else {
            findViewById(R.id.searchBranchLayout).setVisibility(View.VISIBLE);
            ArrayAdapter<String> adapter = new ArrayAdapter<String>(Search.this, android.R.layout.simple_spinner_item, searchBranches);
            adapter.setDropDownViewResource(android.R.layout.simple_spinner_dropdown_item);
            ((Spinner) findViewById(R.id.searchBranch)).setAdapter(adapter);
        }
    }

    @Override
    protected void onActivityResult(int requestCode, int resultCode, Intent data) {
        if (requestCode == REQUEST_CHOOSE_LIBRARY) {
            if (resultCode == LibraryList.RESULT_OK){
                libId = LibraryList.lastSelectedLibId;
                libName = LibraryList.lastSelectedLibName;
                ((TextView) findViewById(R.id.library)).setText(libName);

                obtainSearcher();
                setBranchViewes();
            } else if ("".equals(libId)) finish();
        } else super.onActivityResult(requestCode, resultCode, data);
    }

    @Override
    public boolean onSearchEvent(Bridge.SearcherAccess access, Bridge.SearchEvent event) {
        if (debugTester != null && debugTester.onSearchEvent(access, event)) return true;
        if (access != searcher) return false;
        switch (event.kind) {
            case CONNECTED:
                endLoadingAll(LOADING_SEARCH_CONNECTING);
                searcher.heartBeat = System.currentTimeMillis();
                access.state = SEARCHER_STATE_CONNECTED;
                access.homeBranches = (String[])event.obj1;
                access.searchBranches = (String[])event.obj2;
                setBranchViewes();
                return true;
            case EXCEPTION:
                endLoadingAll(LOADING_SEARCH_CONNECTING);
                searcher.state = SEARCHER_STATE_FAILED;
                searcher = null;
                gcSearchers();
                VideLibriApp.showPendingExceptions();
                return true;
        }
        return false;
    }


    static class SearchDebugTester{
        Bridge.Library[] libs;
        int pos;
        Bridge.SearcherAccess searcher;
        Bridge.Book query;
        SearchDebugTester(Bridge.Book query, String startId){
            this.query = query;
            libs = Bridge.getLibraries();
            pos = 0;
            while (pos < libs.length && !startId.equals(libs[pos].id) ) pos++;
            start();
        }
        private void start(){
            Log.i("VIDELIBRI", "============================================================");
            Log.i("VIDELIBRI", "Testing search: " + libs[pos].namePretty);
            Log.i("VIDELIBRI", "============================================================");
            searcher = new Bridge.SearcherAccess(libs[pos].id);
            searcher.connect();
            if (VideLibriApp.currentActivity instanceof Search) {
                Search s = (Search)VideLibriApp.currentActivity;
                s.libId = libs[pos].id;
                s.libName = libs[pos].namePretty;
                ((TextView) s.findViewById(R.id.library)).setText(libs[pos].namePretty);
                s.beginLoading(LOADING_SEARCH_SEARCHING);
            }
        }
        public boolean onSearchEvent(Bridge.SearcherAccess access, Bridge.SearchEvent event) {
            if (access != searcher) return false;
            switch (event.kind) {
                case CONNECTED:
                    searcher.start(query, -1, -1);
                    break;
                case FIRST_PAGE:
                    if (searcher.nextPageAvailable) {
                        searcher.nextPage();
                        break;
                    }
                 case NEXT_PAGE:
                     searcher.free();
                     pos++;
                     if (pos < libs.length) start();
                     else endComplete();
                     break;
                 case EXCEPTION:
                     searcher.free();
                     endComplete();
                     VideLibriApp.showPendingExceptions();
                     break;
            }
            return access == searcher;
        }

        private void endComplete(){
            searcher = null;
            if (!(VideLibriApp.currentActivity instanceof Search)) return;
            ( (Search)VideLibriApp.currentActivity).endLoadingAll(LOADING_SEARCH_SEARCHING);
            ( (Search)VideLibriApp.currentActivity).debugTester = null;
        }
    }

    SearchDebugTester debugTester;
    public void activateHiddenDebugMode(){
        Util.showMessage(DialogId.DEBUG_SEARCH_BEGIN, "You have activated the secret debug mode");
    }


    @Override
    boolean onDialogResult(int dialogId, int buttonId, Bundle more) {
        switch (dialogId){
            case DialogId.DEBUG_SEARCH_BEGIN:
                Util.showMessageYesNo(DialogId.DEBUG_SEARCH_ALL, "Do you want to search ALL libraries? ");
                return true;
            case DialogId.DEBUG_SEARCH_ALL:
                if (buttonId == DialogInterface.BUTTON_POSITIVE) {
                    Bridge.Book book = new Bridge.Book();
                    book.title = getTextViewText(R.id.title);
                    book.author = getTextViewText(R.id.author);
                    debugTester = new SearchDebugTester(book,libId);
                }
                return true;
        }
        return super.onDialogResult(dialogId, buttonId, more);
    }
}