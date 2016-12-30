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
        for (int i=searchers.size()-1;i>=0;i--)
            if (System.currentTimeMillis() - searchers.get(i).heartBeat > SEARCHER_HEARTH_BEAT_TIMEOUT
                    || searchers.get(i).state == SEARCHER_STATE_FAILED) {
                searchers.get(i).free();
                searchers.remove(i);
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
        searchers.add(searcher);
        beginLoading(LOADING_SEARCH_CONNECTING);
        searcher.connect();
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
            if (searcher.state == SEARCHER_STATE_CONNECTED) setBranchViewes();
        }
    }

    @Override
    protected void onResume() {
        super.onResume();
        obtainSearcher();
        if (searcher != null) {
            for (Bridge.SearchEvent event: searcher.pendingEvents)
                onSearchEvent(searcher, event);
            searcher.pendingEvents.clear();
            if (searcher.state != SEARCHER_STATE_INIT) endLoadingAll(LOADING_SEARCH_CONNECTING);
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

    volatile int debugWaiting ;
    public void activateHiddenDebugMode(){
        Util.showMessage(DialogId.DEBUG_SEARCH_BEGIN, "You have activated the secret debug mode");
    }

    @Override
    boolean onDialogResult(int dialogId, int buttonId, Bundle more) {
        /*
        switch (dialogId){
            case DialogId.DEBUG_SEARCH_BEGIN:
                Util.showMessageYesNo(DialogId.DEBUG_SEARCH_ALL, "Do you want to search ALL libraries? ");
                return true;
            case DialogId.DEBUG_SEARCH_ALL:
                if (buttonId == DialogInterface.BUTTON_POSITIVE) {
                    Bridge.Library[] libs = Bridge.getLibraries();
                    boolean start = false;
                    for (int i = 0; i < libs.length; i++) {
                        if (start || libId.equals(libs[i].id)) {
                            Log.i("VIDELIBRI", "============================================================");
                            Log.i("VIDELIBRI", "Testing search: " + libs[i].namePretty);
                            Log.i("VIDELIBRI", "============================================================");

                            start = true;
                            final Bridge.SearcherAccess searcher = new Bridge.SearcherAccess(Search.this, libs[i].id);
                            debugWaiting = 0;
                            searcher.setDisplay(new Bridge.SearchResultDisplay() {
                                @Override
                                public void onSearchFirstPageComplete(Bridge.Book[] books) {
                                    if (searcher.nextPageAvailable) searcher.nextPage();
                                    else onSearchNextPageComplete(books);
                                }

                                @Override
                                public void onSearchNextPageComplete(Bridge.Book[] books) {
                                    debugWaiting = 1;
                                }

                                @Override
                                public void onSearchDetailsComplete(Bridge.Book book) {

                                }

                                @Override
                                public void onOrderComplete(Bridge.Book book) {

                                }

                                @Override
                                public void onOrderConfirm(Bridge.Book book) {

                                }

                                @Override
                                public void onTakePendingMessage(int kind, String caption, String[] options) {

                                }

                                @Override
                                public void onPendingMessageCompleted() {

                                }

                                @Override
                                public void onConnected(String[] homeBranches, String[] searchBranches) {

                                }

                                @Override
                                public void onException() {
                                    debugWaiting = -1;
                                }
                            });
                            Bridge.Book book = new Bridge.Book();
                            book.title = getTextViewText(R.id.title);
                            book.author = getTextViewText(R.id.author);
                            searcher.start(book, -1, -1);

                            while (debugWaiting == 0)
                                try {
                                    Thread.sleep(50);
                                } catch (InterruptedException e) {

                                }
                            if (debugWaiting == -1) {
                                Search.this.onException();
                                return true;
                            }
                            searcher.free();
                        }
                    }
                }
                return true;
        }                                              */
        return super.onDialogResult(dialogId, buttonId, more);
    }
}