package de.benibela.videlibri;

import android.*;
import android.R;
import android.app.Activity;
import android.content.Context;
import android.content.DialogInterface;
import android.content.Intent;
import android.graphics.Bitmap;
import android.graphics.Color;
import android.os.Handler;
import android.os.Message;
import android.util.Log;
import android.util.Pair;

import java.io.Serializable;
import java.util.*;

public class Bridge {
    public static class LibraryDetails {
        String homepageBase, homepageCatalogue;
        String prettyName;
        String id;
        String templateId;
        String variableNames[];
        String variableValues[];
        boolean segregatedAccounts;
    }

    public static class Account implements Serializable{
        String libId, name, pass, prettyName;
        int type;
        boolean extend;
        int extendDays;
        boolean history;
        public boolean equals(Object o) {
            if (!(o instanceof Account)) return  false;
            Account a = (Account) o;
            return  (a.libId == libId && a.prettyName == prettyName);
        }
        String internalId(){
            return libId+"#"+name;
        }
        Library getLibrary(){ //warning slow
            Library[] libs = getLibraries();
            for (Library lib: libs)
                if (lib.id.equals(libId)) return lib;
            return null;
        }
    }

    static int currentPascalDate;
    public static class SimpleDate {
        private int year, month, day;
        public int pascalDate;
        SimpleDate (int year, int month, int day, int pascalDate){
            this.year = year;
            this.month = month;
            this.day = day;
            this.pascalDate = pascalDate;
        }
        Date getTime(){
            return new Date(year - 1900, month, day);
        }
    }

    public static class Book implements Serializable{

        public static class Pair implements Serializable{   //android.os.Pair is not serializable
            String first, second;
            Pair (String a, String b){
                first = a;
                second = b;
            }
        }


        Account account;
        String author = "";
        String title = "";
        SimpleDate issueDate; //might be null
        SimpleDate dueDate; //might be null
        boolean history;
        ArrayList<Pair> more = new ArrayList<Pair>();
        private int status;

        Book holdings[];

        Bitmap image; //handled on Javasite only

        @Override
        public String toString() {
            return title;
        }

        int getStatusColor(){
            int c = Color.GREEN;
            if (this.history) c = -1;
            else if ((account != null || more == VideLibri.crazyHeaderHack)  && dueDate != null && this.dueDate.pascalDate - currentPascalDate <= 3)
                c = Color.RED;
            else switch (getStatus()){
                //lend
                case Normal: return Color.GREEN;
                case Problematic: return Color.YELLOW;
                case Ordered: return Color.CYAN;
                case Provided: return Color.MAGENTA;
                //search
                case Available: return Color.GREEN;
                case Lend: return Color.RED;
                case Virtual: return Color.CYAN;
                case Presentation: return Color.RED;
                case InterLoan: return Color.RED;


                default: return Color.YELLOW; //Template did not set status. Assume not renewable
            }
            return c;
        }

        enum StatusEnum { Unknown, Normal, Problematic, Ordered, Provided,
                          Available, Lend, Virtual, Presentation, InterLoan};

        StatusEnum getStatus() {
            switch (status) {
                case 1: return StatusEnum.Normal;
                case 2: return StatusEnum.Problematic;
                case 3: return StatusEnum.Ordered;
                case 4: return StatusEnum.Provided;
                case 100: return StatusEnum.Available;
                case 101: return StatusEnum.Lend;
                case 102: return StatusEnum.Virtual;
                case 103: return StatusEnum.Presentation;
                case 104: return StatusEnum.InterLoan;
                default: return StatusEnum.Unknown;
            }
        }
        void setStatus(StatusEnum se) {
            switch (se) {
                case Normal: status = 1; break;
                case Problematic: status = 2; break;
                case Ordered: status = 3; break;
                case Provided: status = 4; break;
                case Available: status = 100; break;
                case Lend: status = 101; break;
                case Virtual: status = 102; break;
                case Presentation: status = 103; break;
                case InterLoan: status = 104; break;
            }
        }

        boolean isOrderable(){ //defaults to false
            String orderable = getProperty("orderable");
            return (orderable != null && !"".equals(orderable) && !"0".equals(orderable) && !"false".equals(orderable));
        }
        boolean isOrderableHolding(){ //defaults to true
            String order = getProperty("orderable");
            return !( "false".equals(order));
        }
        boolean isCancelable(){
            String cancelable = getProperty("cancelable");;
            return cancelable == null || !"false".equals(cancelable);
        }

        boolean isGroupingHeaderFakeBook(){
            return more == VideLibri.crazyHeaderHack;
        }

        //called from Videlibri midend
        void setProperty(String name, String value){
            more.add(new Pair(name, value));
        }

        String getProperty(String name){
            for (int i=more.size()-1;i>=0;i--)       //backward for simple overriding
                if (more.get(i).first.equals(name))
                    return more.get(i).second;
            if ("title".equals(name)) return title;
            if ("author".equals(name)) return author;
            return "";
        }
        String getProperty(String name, String def){
            String res = getProperty(name);
            return Util.isEmptyString(res) ? def : res;
        }

        boolean hasProperty(String name){
            for (int i=more.size()-1;i>=0;i--)
                if (more.get(i).first.equals(name))
                    return true;
            return false;
        }

        boolean matchesFilter(String filter, String key){
            if (key != null && !"".equals(key)) return getProperty(key).toLowerCase().indexOf(filter) >= 0;
            if (author.toLowerCase().indexOf(filter) >= 0 || title.toLowerCase().indexOf(filter) >= 0) return true;
            return false;
        }

        String getNormalizedISBN(boolean removeSeps, boolean convertTo13) {
            String isbn = getProperty("isbn").trim();
            if (isbn.length() >= 5) {
                if (isbn.charAt(1) == '-') {
                    isbn = isbn.substring(0,13);
                    if (convertTo13) {
                        isbn = "978-" + isbn;
                        int check = 0, multiplier = 1;
                        for (int i=0;i<isbn.length() - 1; i++)
                            if (isbn.charAt(i) >= '0' && isbn.charAt(i) <= 9) {
                                check += multiplier * (int)(isbn.charAt(i) - '0');
                                multiplier = (multiplier + 2) & 3;
                            }
                        isbn = isbn.substring(0, 12) + (10 - check % 10) % 10;
                    }
                }
                if (isbn.charAt(3) == '-' || isbn.charAt(3) == ' ') isbn = isbn.substring(0, 17);
            }
            if (removeSeps)
                isbn = isbn.replaceAll("[- ]", "");
            return isbn;
        }

        public boolean equalsBook(Book q) {
            if (!Util.equalStrings(title, q.title)) return false;
            if (!Util.equalStrings(author, q.author)) return false;
            if (history != q.history) return false;
            if (!(account == null ? q.account == null : Util.equalStrings(account.libId, q.account.libId) && Util.equalStrings(account.name, q.account.name) )) return false;
            for (int i=0;i<more.size();i++) {
                if (i < q.more.size() && Util.equalStrings(more.get(i).first, q.more.get(i).first) && Util.equalStrings(more.get(i).second, q.more.get(i).second))
                    continue;

                if (!Util.equalStrings(more.get(i).second, q.getProperty(more.get(i).first))) return false;
               /*boolean ok = false;
                    ok = Util.equalStrings(more.get(i).second, q.more.get(i).second);
                    for (Pair b: q.more)
                        if (Util.equalStrings())*/
            }
            return true;
        }
    }

    public static class InternalError extends RuntimeException {
        public InternalError() {}
        public InternalError(String msg) { super(msg); }
    }

    public static class PendingException{
        String accountPrettyNames, error, library, searchQuery, details, anonymousDetails;
    }

    public static class Options{
        boolean logging;
        int nearTime, refreshInterval;
        String roUserLibIds[];
    }

    public static class TemplateDetails{
        String variablesNames[];
        String variablesDescription[];
        String variablesDefault[];
    }

    static private native void VLInit(VideLibriContext videlibri);
    static public native String[] VLGetLibraries(); //id|pretty location|name|short name
    static public native LibraryDetails VLGetLibraryDetails(String id);
    static public native void VLSetLibraryDetails(String id, LibraryDetails details);
    static public native void VLInstallLibrary(String url);
    static public native String[] VLGetTemplates(); //array of ids
    static public native TemplateDetails VLGetTemplateDetails(String id);
    static public native Account[] VLGetAccounts();
    static public native void VLAddAccount(Account acc);
    static public native void VLChangeAccount(Account oldacc, Account newacc);
    static public native void VLDeleteAccount(Account acc);
    static public native Book[] VLGetBooks(Account acc, boolean history);
    static public native boolean VLUpdateAccount(Account acc, boolean autoUpdate, boolean forceExtend);
    static final int BOOK_OPERATION_RENEW = 1;
    static final int BOOK_OPERATION_CANCEL = 2;
    static public native void VLBookOperation(Book[] books, int operation);
    static public native PendingException[] VLTakePendingExceptions();

    static public native String[] VLGetNotifications();

    static public native void VLSearchConnect(SearcherAccess searcher, String libId);
    static public native void VLSearchStart(SearcherAccess searcher, Book query, int homeBranch, int searchBranch);
    static public native void VLSearchNextPage(SearcherAccess searcher);
    static public native void VLSearchDetails(SearcherAccess searcher, Book book);
    static public native void VLSearchOrder(SearcherAccess searcher, Book[] book);
    static public native void VLSearchOrder(SearcherAccess searcher, Book[] book, int[] holding);
    static public native void VLSearchOrderConfirmed(SearcherAccess searcher, Book[] book);
    static public native void VLSearchCompletePendingMessage(SearcherAccess searcher, int result);
    static public native void VLSearchEnd(SearcherAccess searcher);

    static public native void VLSetOptions(Options options);
    static public native Options VLGetOptions();

    static public native Book[] VLXQuery(String query);

    static public native void VLFinalize();


    //SearcherAccess helper class like in Pascal-VideLibri
    //All methods run asynchronously in a Pascal Thread
    //All events are called in the same thread ()
    public static class SearcherAccess{
        //set from Pascal side
        long nativePtr;
        volatile int totalResultCount;
        volatile boolean nextPageAvailable;
        volatile String[] homeBranches, searchBranches; //from java and pascal

        //set in Java
        final String libId;
        final Handler queueHandler;
        final ArrayList<SearchEvent> pendingEvents = new ArrayList<SearchEvent>();

        //set in (Java) activity
        int state;
        long heartBeat;
        final ArrayList<Bridge.Book> bookCache = new ArrayList<Book>();
        //The detail search runs in the background, for a single book.
        //But the user might request other detail searches, before the search is complete.
        //Then wait for the old search to complete, and then start the newest search, unless the user has closed the view
        int waitingForDetails;    //nr of book currently searched. Only set when the search is started or has ended (-1 if no search is running)
        int nextDetailsRequested; //nr of the book that *should* be searched. Set when requesting a new search, or to -1 to cancel the current search
        Bridge.Account orderingAccount;

        SearcherAccess(String libId){
            this.libId = libId;
            queueHandler = new Handler(){
                @Override
                public void handleMessage(Message msg) {
                    SearchEvent event = (SearchEvent)(msg.obj);
                    if (VideLibriApp.currentActivity instanceof SearchEventHandler) {
                        SearchEventHandler handleAct = (SearchEventHandler) VideLibriApp.currentActivity;
                        if (handleAct.onSearchEvent(SearcherAccess.this, event)) return;
                    }
                    pendingEvents.add(event);
                }
            };
        }
        public void connect(){
            VLSearchConnect(this, libId);
        }
        public void start(Book query, int homeBranch, int searchBranch){
            VLSearchStart(this, query, homeBranch, searchBranch);
        }
        public void nextPage(){
            VLSearchNextPage(this);
        }
        public void details(Book book){
            VLSearchDetails(this, book);
        }
        public void order(Book book){
            VLSearchOrder(this, new Book[]{book});
        }
        public void order(Book book, int holdingId){
            VLSearchOrder(this, new Book[]{book}, new int[]{holdingId});
        }
        public void orderConfirmed(Book book){
            VLSearchOrderConfirmed(this, new Book[]{book});
        }
        public void completePendingMessage(int result){
            VLSearchCompletePendingMessage(this, result);
        }
        public void free(){
            VLSearchEnd(this);
        }

        private SearchEvent newEvent(SearchEventKind kind) {  return newEvent(kind, 0, null, null); }
        private SearchEvent newEvent(SearchEventKind kind, Object obj) {  return newEvent(kind, 0, obj, null); }
        private SearchEvent newEvent(SearchEventKind kind, int arg1, Object obj1, Object obj2) {
            SearchEvent event = new SearchEvent();
            event.kind = kind;
            event.arg1 = arg1;
            event.obj1 = obj1;
            event.obj2 = obj2;
            return event;
        }
        private void send(SearchEvent event) {
            queueHandler.sendMessage(queueHandler.obtainMessage(0, event));
        }


        public void onConnected(String[] homeBranches, String[] searchBranches){ send(newEvent(SearchEventKind.CONNECTED, 0, homeBranches, searchBranches)); }
        public void onSearchFirstPageComplete(Book[] books) { send(newEvent(SearchEventKind.FIRST_PAGE, books)); }
        public void onSearchNextPageComplete(Book[] books) { send(newEvent(SearchEventKind.NEXT_PAGE, books)); }
        public void onSearchDetailsComplete(Book book) { send(newEvent(SearchEventKind.DETAILS, book)); }
        public void onOrderComplete(Book book) { send(newEvent(SearchEventKind.ORDER_COMPLETE, book)); }
        public void onOrderConfirm(Book book) { send(newEvent(SearchEventKind.ORDER_CONFIRM, book)); }
        public void onTakePendingMessage(int kind, String caption, String[] options) { send(newEvent(SearchEventKind.TAKE_PENDING_MESSAGE, kind, caption, options)); }
        public void onPendingMessageCompleted() { send(newEvent(SearchEventKind.PENDING_MESSAGE_COMPLETE)); }
        public void onException() { send(newEvent(SearchEventKind.EXCEPTION)); }
    }

    static enum SearchEventKind {
        CONNECTED, //obj1 = String[] homeBranches, obj2 = String[] searchBranches
        FIRST_PAGE, //obj1 = Book[] books
        NEXT_PAGE,  //obj1 = Book[] books
        DETAILS,    //obj1 = Book book
        ORDER_COMPLETE, //obj1 = Book book
        ORDER_CONFIRM,  //obj1 = Book book
        TAKE_PENDING_MESSAGE, //arg1 = int kind, obj1 = String caption, obj2 = String[] options
        PENDING_MESSAGE_COMPLETE,
        EXCEPTION
    }
    public static class SearchEvent{
        SearchEventKind kind;
        int arg1;
        Object obj1, obj2;
    }

    interface SearchEventHandler {
        boolean onSearchEvent(SearcherAccess access, SearchEvent event);
    }

    static class Library{
        String id, country, fullStatePretty, locationPretty, namePretty, nameShort;
        void putInIntent(Intent intent){
            intent.putExtra("libName", namePretty);
            intent.putExtra("libShortName", nameShort);
            intent.putExtra("libId", id);
        }
    }
    static Library[] getLibraries(){
        String libs[] =  VLGetLibraries();
        ArrayList<Library> important = new ArrayList<Library>();
        Library[] result = new Library[libs.length];
        for (int i=0;i<libs.length;i++){
            result[i] = new Library();
            String[] temp = libs[i].split("\\|");
            result[i].id = temp[0];
            String[] tmpCountry = temp[1].split("[ -]");
            result[i].country = tmpCountry.length > 0 ? tmpCountry[0] : "";
            result[i].fullStatePretty = temp[1];
            result[i].locationPretty = temp[2];
            result[i].namePretty = temp[3];
            result[i].nameShort = temp[4];
            if (result[i].namePretty.contains("(Neu)")) important.add(result[i]);
        }
        Locale locale = Locale.getDefault();
        final String location = locale != null ? locale.getCountry() : "DE";

        Arrays.sort(result, new Comparator<Library>() {
            @Override
            public int compare(Library library, Library library2) {
                int r = library.country.compareTo(library2.country);
                if (r != 0){
                    if (Util.isEmptyString(library.country)) return -1;
                    if (Util.isEmptyString(library2.country)) return 1;
                    if (library.country.equals(location)) return -1;
                    if (library2.country.equals(location)) return 1;
                    return r;
                }
                r = library.fullStatePretty.compareTo(library2.fullStatePretty);
                if (r != 0) return r;
                r = library.locationPretty.compareTo(library2.locationPretty);
                if (r != 0) return r;
                return library.namePretty.compareTo(library2.namePretty);
            }
        });
        if (important.size() > 0) {
            important.addAll(Arrays.asList(result));
            result = important.toArray(new Library[important.size()]);
        }
        return result;
    }

    //const libID: string; prettyName, aname, pass: string; extendType: TExtendType; extendDays:integer; history: boolean

    //mock functions

    /*static public Account[] VLGetAccounts(){
        return new Account[0];
    }
    static public void VLAddAccount(Account acc){

    }
    static public void VLChangeAccount(Account oldacc, Account newacc){

    }
    static public void VLDeleteAccount(Account acc){}*/
    //static public Book[] VLGetBooks(Account acc, boolean history){return  new Book[0];}
    //static public void VLUpdateAccount(Account acc){}


    public static class ImportExportData {
        public static final int CURRENT  = 0x01;
        public static final int HISTORY  = 0x02;
        public static final int CONFIG   = 0x04;
        public static final int PASSWORD = 0x08;
        
        String accountsToImport[];
        int flags;
        long nativePtr; //this is a very large object which must be destroyed with a call to  VLImportAccounts
    }
    public static native void VLExportAccounts(String filename, Account accountsToExport[], int flags);
    public static native ImportExportData VLImportAccountsPrepare(String filename);
    public static native void VLImportAccounts(ImportExportData data);


    //called from VideLibri

    public static interface VideLibriContext{
        String userPath();
    }


    static void allThreadsDone(){
        if (VideLibriApp.allThreadsDoneHandler == null) return;
        VideLibriApp.allThreadsDoneHandler.sendEmptyMessage(0);

    }

    static void installationDone(final int status){
        if (VideLibriApp.installationDoneHandler == null) return;
        VideLibriApp.installationDoneHandler.sendEmptyMessage(status);
    }

    //init

    static public void log(final String message){
        Log.i("VideLibri", message);
    }

    static boolean initialized = false;
    static public void initialize(VideLibriContext context){
        if (initialized) return;
        initialized = true;
        try
        {
            Log.i("Videlibri", "Trying to load liblclapp.so");
            System.loadLibrary("lclapp");
            Log.i("Videlibri", "Initializing Windows VM and Pascal layer");
            VLInit(context);
        }
        catch(UnsatisfiedLinkError ule)
        {
            Log.e("Videlibri", "WARNING: Could not load liblclapp.so: " + ule.getMessage());
            ule.printStackTrace();
        }
    }
}
