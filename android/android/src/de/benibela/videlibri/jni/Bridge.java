package de.benibela.videlibri.jni;

import android.content.Intent;
import android.graphics.Bitmap;
import android.os.Handler;
import android.util.Log;

import java.io.Serializable;
import java.util.*;


@SuppressWarnings( {"JniMissingFunction", "unused"} )
public class Bridge {
    public static class LibraryDetails {
        public String homepageBase, homepageCatalogue;
        public String prettyName, prettyNameShort;
        public String id;
        public String templateId;
        public String variableNames[];
        public String variableValues[];
        public boolean segregatedAccounts;

        public LibraryDetails(){}
        public LibraryDetails(String homepageBase, String homepageCatalogue,
                              String prettyName, String prettyNameShort,
                              String id,
                              String templateId,
                              String variableNames[],
                              String variableValues[],
                              boolean segregatedAccounts
        ){
            this.homepageBase = homepageBase;
            this.homepageCatalogue = homepageCatalogue;
            this.prettyName = prettyName;
            this.prettyNameShort = prettyNameShort;
            this.id = id;
            this.templateId = templateId;
            this.variableNames = variableNames;
            this.variableValues = variableValues;
            this.segregatedAccounts = segregatedAccounts;
        }

        public static String decodeIdEscapes(String s) {
            if (!s.contains("+")) return s;
            return s.replace("+ue", "ü")
                    .replace("+oe", "ö")
                    .replace("+ae", "ä")
                    .replace("+sz", "ß")
                    .replace("++", " ");
        }
    }

    public static class Account implements Serializable{
        public String libId, name, pass, prettyName;
        public int type;
        public boolean extend;
        public int extendDays;
        public boolean history;
        public Account () {}
        public Account (String libId, String name, String pass, String prettyName,
                 int type, boolean extend,
                 int extendDays, boolean history) {
            this.libId = libId;
            this.name = name;
            this.pass = pass;
            this.prettyName = prettyName;
            this.type = type;
            this.extend = extend;
            this.extendDays = extendDays;
            this.history = history;
        }
        public boolean equals(Object o) {
            if (!(o instanceof Account)) return false;
            Account a = (Account) o;
            return  Util.equalStrings(a.libId, libId) && Util.equalStrings(a.prettyName, prettyName);
        }
    }

    public static int currentPascalDate;
    static private Calendar referenceCalendar = new GregorianCalendar(1899, 11, 30); //1899-12-30
    public static Date pascalDateToDate(int pascalDate){
        Calendar c = (Calendar)(referenceCalendar.clone());
        c.add(Calendar.DATE, pascalDate);
        return c.getTime();
    }

    private static class Util{
        static public boolean equalStrings(String s, String t) {
            return s == null ? t == null : s.equals(t);
        }

        public static boolean isEmptyString(String s) {
            return s == null || "".equals(s);
        }
    }


    public static class Book implements Serializable{
        public Book(){
            author = title = id = year = "";
            additionalProperties = new ArrayList<>(12);
        }
        public Book(String title){
            author = id = year = "";
            this.title = title;
            additionalProperties = new ArrayList<>(0);
        }
        public Book(int additionalPropertyCount, Account account, String id, String author, String title, String year){
            this.account = account;
            this.id = id;
            this.author = author;
            this.title = title;
            this.year = year;
            this.additionalProperties = new ArrayList<>(additionalPropertyCount);
        }

        public Account account;
        public String id, author, title, year;
        public int issueDate, dueDate; //Pascal date, 0 if undefined
        public boolean history;
        public ArrayList<String> additionalProperties;

        private int status;

        public Book holdings[];

        public Bitmap image; //handled on Javasite only

        @Override
        public String toString() {
            return title; //used for copy to clipboard. where else? todo: probably add author
        }

        public enum StatusEnum { Unknown, Normal, Problematic, Ordered, Provided,
                          Available, Lend, Virtual, Presentation, InterLoan}

        final public StatusEnum getStatus() {
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
                default: return account == null ? StatusEnum.Unknown : StatusEnum.Problematic;
            }
        }
        final public void setStatus(StatusEnum se) {
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
        final public boolean hasOrderedStatus(){
            switch (getStatus()) {
                case Ordered: case Provided: return true;
                default: return false;
            }
        }


        public boolean isOrderable(){ //defaults to false
            String orderable = getProperty("orderable");
            return (orderable != null && !"".equals(orderable) && !"0".equals(orderable) && !"false".equals(orderable));
        }
        public boolean isOrderableHolding(){ //defaults to true
            String order = getProperty("orderable");
            return !( "false".equals(order));
        }
        public boolean isCancelable(){
            String cancelable = getProperty("cancelable");
            return cancelable == null || !"false".equals(cancelable);
        }


        //called from Videlibri midend
        final public void setProperty(String name, String value){
            //more.add(new Pair(name, value));
            additionalProperties.add(name);
            additionalProperties.add(value);
        }

        final public String getProperty(String name) {
            assert (additionalProperties.size() & 1) == 0;
            for (int i = additionalProperties.size() - 2; i >= 0; i -= 2)       //backward for simple overriding
                if (additionalProperties.get(i).equals(name))
                    return additionalProperties.get(i + 1);
            switch (name){
                case "title":
                return title;
                case "author":
                return author;
                case "id":
                return id;
                case "year":
                return year;
            }
            return "";
        }
        final public String getProperty(String name, String def){
            String res = getProperty(name);
            return Util.isEmptyString(res) ? def : res;
        }

        final public boolean hasProperty(String name){
            assert (additionalProperties.size() & 1) == 0;
            for (int i = additionalProperties.size() - 2; i >= 0; i -= 2)
                if (additionalProperties.get(i).equals(name))
                    return true;
            return false;
        }

        public boolean matchesFilter(String filter, String key){
            if (key != null && !"".equals(key)) return getProperty(key).toLowerCase().indexOf(filter) >= 0;
            if (author.toLowerCase().indexOf(filter) >= 0 || title.toLowerCase().indexOf(filter) >= 0) return true;
            return false;
        }

        public enum ISBNNormalization { ISBN_NO_CONVERSION, ISBN_CONVERT_TO_10, ISBN_CONVERT_TO_13}
        public String getNormalizedISBN(boolean removeSeps, ISBNNormalization normalize) {
            String isbn = getProperty("isbn");
            if (Util.isEmptyString(isbn)) return "";
            int normalizationCode;
            switch (normalize) {
                case ISBN_CONVERT_TO_10: normalizationCode = 10; break;
                case ISBN_CONVERT_TO_13: normalizationCode = 13; break;
                case ISBN_NO_CONVERSION: default: normalizationCode = 0; break;
            }
            return VLNormalizeISBN(isbn, removeSeps, normalizationCode);
        }

        public boolean equalsBook(Book q) {
            if (!Util.equalStrings(id, q.id)) return false;
            if (!Util.equalStrings(title, q.title)) return false;
            if (!Util.equalStrings(author, q.author)) return false;
            if (!Util.equalStrings(year, q.year)) return false;
            if (history != q.history) return false;
            if (!(account == null ? q.account == null : Util.equalStrings(account.libId, q.account.libId) && Util.equalStrings(account.name, q.account.name) )) return false;
            for (int i=0;i<additionalProperties.size();i+=2) {
                if (i + 1 < q.additionalProperties.size()
                        && Util.equalStrings(additionalProperties.get(i), q.additionalProperties.get(i))
                        && Util.equalStrings(additionalProperties.get(i+1), q.additionalProperties.get(i+1)))
                    continue;

                if (!Util.equalStrings(additionalProperties.get(i+1), q.getProperty(additionalProperties.get(i))))
                    return false;
            }
            return true;
        }

        public boolean isGroupingHeader(){
            return false;
        }
    }

    public static class InternalError extends RuntimeException {
        public InternalError() {}
        public InternalError(String msg) { super(msg); }
    }

    public static class PendingException{
        static final public int KIND_UNKNOWN = 0;
        static final public int KIND_INTERNET = 1;
        static final public int KIND_LOGIN = 2;

        public int kind;
        public String accountPrettyNames, error, library, searchQuery, details, anonymousDetails;
        public String firstAccountUser, firstAccountLib;
    }

    public static class Options{
        public boolean logging;
        public int nearTime, refreshInterval;
        public String roUserLibIds[];
    }
    public static Options globalOptions;

    public static class TemplateDetails{
        public String variablesNames[];
        public String variablesDescription[];
        public String variablesDefault[];
    }

    static private native void VLInit(VideLibriContext videlibri);
    static public native String[] VLGetLibraryIds();
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
    static public native String VLNormalizeISBN(String isbn, boolean removeSep, int conversion);
    static public native Book VLGetCriticalBook();
    static public native boolean VLUpdateAccount(Account acc, boolean autoUpdate, boolean forceExtend);
    public static final int BOOK_OPERATION_RENEW = 1;
    public static final int BOOK_OPERATION_CANCEL = 2;
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
        public long nativePtr;
        public volatile int totalResultCount;
        public volatile boolean nextPageAvailable;
        public volatile String[] homeBranches, searchBranches; //from java and pascal

        //set in Java
        public final String libId;
        public final ArrayList<SearchEvent> pendingEvents = new ArrayList<>();

        //set in (Java) activity
        public int state;
        public long heartBeat;
        public final ArrayList<Bridge.Book> bookCache = new ArrayList<>();
        //The detail search runs in the background, for a single book.
        //But the user might request other detail searches, before the search is complete.
        //Then wait for the old search to complete, and then start the newest search, unless the user has closed the view
        public int waitingForDetails;    //nr of book currently searched. Only set when the search is started or has ended (-1 if no search is running)
        public int nextDetailsRequested; //nr of the book that *should* be searched. Set when requesting a new search, or to -1 to cancel the current search
        public Bridge.Account orderingAccount;

        public SearcherAccess(String libId){
            this.libId = libId;
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
            if (searchEventHandler == null) return;
            event.searcherAccess = this;
            searchEventHandler.sendMessage(searchEventHandler.obtainMessage(0, event));
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

    public enum SearchEventKind {
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
        public SearcherAccess searcherAccess;
        public SearchEventKind kind;
        public int arg1;
        public Object obj1, obj2;
    }



    public static class ImportExportData {
        public static final int CURRENT  = 0x01;
        public static final int HISTORY  = 0x02;
        public static final int CONFIG   = 0x04;
        public static final int PASSWORD = 0x08;
        
        public String accountsToImport[];
        public int flags;
        long nativePtr; //this is a very large object which must be destroyed with a call to  VLImportAccounts
    }
    public static native void VLExportAccounts(String filename, Account accountsToExport[], int flags);
    public static native ImportExportData VLImportAccountsPrepare(String filename);
    public static native void VLImportAccounts(ImportExportData data);

    static public void log(final String message){
        Log.i("VideLibri", message);
    }


    //called from VideLibri

    static void allThreadsDone(){
        if (allThreadsDoneHandler == null) return;
        allThreadsDoneHandler.sendEmptyMessage(0);
    }

    static void installationDone(final int status){
        if (installationDoneHandler == null) return;
        installationDoneHandler.sendEmptyMessage(status);
    }

    //callbacks
    static public Handler allThreadsDoneHandler, installationDoneHandler, searchEventHandler;

    //init


    public interface VideLibriContext{
        String userPath();
    }

    static boolean initialized = false;
    static public void initialize(VideLibriContext context){
        if (initialized) return;
        initialized = true;
        Log.i("Videlibri", "Trying to load liblclapp.so");
        System.loadLibrary("lclapp");
        Log.i("Videlibri", "Initializing Windows VM and Pascal layer");
        VLInit(context);
        Bridge.globalOptions = Bridge.VLGetOptions();
    }
}
