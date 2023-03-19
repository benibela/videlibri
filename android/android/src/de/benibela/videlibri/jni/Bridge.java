package de.benibela.videlibri.jni;

import android.content.Context;
import android.graphics.Bitmap;
import android.os.Handler;
import android.util.Log;

import com.getkeepsafe.relinker.ReLinker;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.io.Serializable;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.util.*;



@Retention(RetentionPolicy.SOURCE)
@interface NotNullLateInit {}

@SuppressWarnings( {"JniMissingFunction", "unused"} )
public class Bridge {
    public static class Account implements Serializable{
        @NotNull public String libId, name, pass, prettyName;
        public int type;
        public boolean extend;
        public int extendDays;
        public boolean history;
        public int lastCheckDate;
        public @NotNull String expiration;
        public Account () {
            libId = name = pass = prettyName = expiration = "";
        }
        public Account (@NotNull String libId, @NotNull String name, @NotNull String pass, @NotNull String prettyName,
                 int type, boolean extend,
                 int extendDays, boolean history,
                 int lastCheckDate,
                 @NotNull String expiration
        ) {
            this.libId = libId;
            this.name = name;
            this.pass = pass;
            this.prettyName = prettyName;
            this.type = type;
            this.extend = extend;
            this.extendDays = extendDays;
            this.history = history;
            this.lastCheckDate = lastCheckDate;
            this.expiration = expiration;
        }
        @Override
        public boolean equals(Object o) {
            if (!(o instanceof Account)) return false;
            Account a = (Account) o;
            return  Util.equalStrings(a.libId, libId) && Util.equalStrings(a.name, name);
        }
        @Override
        public int hashCode() {
            return libId.hashCode() ^ name.hashCode();
        }
    }

    public static int currentPascalDate;

    private static class Util{
        static boolean equalStrings(@Nullable String s, @Nullable String t) {
            return s == null ? t == null : s.equals(t);
        }

        static boolean isEmptyString(@Nullable String s) {
            return s == null || "".equals(s);
        }
    }


    public static class Book implements Serializable{
        public Book(){
            author = title = id = year = "";
            additionalProperties = new ArrayList<>(12);
        }
        public Book(@NotNull String title){
            author = id = year = "";
            this.title = title;
            additionalProperties = new ArrayList<>(0);
        }
        public Book(int additionalPropertyCount,
                    @Nullable Account account,
                    @NotNull String id, @NotNull String author, @NotNull String title, @NotNull String year){
            this.account = account;
            this.id = id;
            this.author = author;
            this.title = title;
            this.year = year;
            this.additionalProperties = new ArrayList<>(additionalPropertyCount);
        }

        @Nullable public Account account;
        @NotNull public String id, author, title, year;
        public int issueDate, dueDate, firstExistsDate; //Pascal date, 0 if undefined
        public boolean history;
        @NotNull public ArrayList<String> additionalProperties;

        private int status;

        @Nullable public Book [] holdings;

        @Nullable public Bitmap image; //handled on Javasite only

        @Override @NotNull
        public String toString() {
            return title; //used for copy to clipboard. where else? todo: probably add author
        }

        public enum StatusEnum { Unknown, Normal, Problematic, Ordered, Provided,
                          Available, Lend, Virtual, Presentation, InterLoan}

        @NotNull
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
        final public void setProperty(@NotNull String name, @NotNull String value){
            //more.add(new Pair(name, value));
            additionalProperties.add(name);
            additionalProperties.add(value);
        }

        final public @NotNull String getProperty(@NotNull String name) {
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
        final public @NotNull String getProperty(@NotNull String name, @NotNull String def){
            String res = getProperty(name);
            return Util.isEmptyString(res) ? def : res;
        }

        final public boolean hasProperty(@NotNull String name){
            assert (additionalProperties.size() & 1) == 0;
            for (int i = additionalProperties.size() - 2; i >= 0; i -= 2)
                if (additionalProperties.get(i).equals(name))
                    return true;
            return false;
        }


        public boolean equalsBook(@NotNull Book q) {
            if (!Util.equalStrings(id, q.id)) return false;
            if (!Util.equalStrings(title, q.title)) return false;
            if (!Util.equalStrings(author, q.author)) return false;
            if (!Util.equalStrings(year, q.year)) return false;
            if (history != q.history) return false;
            if (!(account == null ? q.account == null : q.account != null && Util.equalStrings(account.libId, q.account.libId) && Util.equalStrings(account.name, q.account.name) )) return false;
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
        public InternalError(String msg, Throwable cause) { super(msg, cause); }
    }
    public static class InternalErrorJNI extends InternalError {
        public InternalErrorJNI() {}
        public InternalErrorJNI(String msg) { super(msg); }
        public InternalErrorJNI(String msg, Throwable cause) { super(msg, cause); }
    }
    public static class InternalErrorFile extends InternalError {
        public InternalErrorFile() {}
        public InternalErrorFile(String msg) { super(msg); }
        public InternalErrorFile(String msg, Throwable cause) { super(msg, cause); }
    }
    public static class InternalErrorExternal extends InternalError {
        public InternalErrorExternal() {}
        public InternalErrorExternal(String msg) { super(msg); }
        public InternalErrorExternal(String msg, Throwable cause) { super(msg, cause); }
    }

    public static class PendingException{
        static final public int KIND_UNKNOWN = 0;
        static final public int KIND_INTERNET = 1;
        static final public int KIND_LOGIN = 2;

        public int kind;
        @NotNullLateInit public String accountPrettyNames, error, library, searchQuery, details, anonymousDetails;
        @NotNullLateInit public String firstAccountUser, firstAccountLib;
    }

    static private native void VLInit(@NotNull Context context);
    static public native @NotNull String[] VLGetLibraryIds();
    static public native @Nullable LibraryDetails VLGetLibraryDetails(@NotNull String id, boolean needCatalogUrl);
    static public @Nullable LibraryDetails VLGetLibraryDetails(@NotNull String id){
        return VLGetLibraryDetails(id, false);
    }
    static public native void VLSetLibraryDetails(@NotNull String id, @Nullable LibraryDetails details);
    static public native void VLInstallLibrary(@NotNull String url);
    static public native @NotNull String[] VLGetTemplates(); //array of ids
    static public native @Nullable TemplateDetails VLGetTemplateDetails(@NotNull String id);
    static public native void VLReloadLibrary(@NotNull String id);
    static public native void VLReloadTemplate(@NotNull String id);
    static public native @NotNull Account[] VLGetAccounts();
    static public native void VLAddAccount(@NotNull Account acc);
    static public native void VLChangeAccount(@NotNull Account oldacc, @NotNull Account newacc);
    static public native void VLDeleteAccount(@NotNull Account acc);
    static public native @Nullable Book[] VLGetBooks(@NotNull Account acc, boolean history);
    //pass null for old/new to create/delete a book
    static public native void VLChangeBook(@Nullable Book oldBook, @Nullable Book newBook);
    static public native @NotNull String[] VLGetCoverURLs(@NotNull String isbn, int maxWidth, int maxHeight);
    static public native @Nullable Book VLGetCriticalBook();
    static public native boolean VLUpdateAccount(@NotNull Account acc, boolean autoUpdate, boolean forceExtend);
    public static final int BOOK_OPERATION_RENEW = 1;
    public static final int BOOK_OPERATION_CANCEL = 2;
    static public native void VLBookOperation(@NotNull Book[] books, int operation);
    static public native @Nullable PendingException[] VLTakePendingExceptions();
    static public native boolean VLSendFeedback(String[] feedBack);


    static public native @Nullable String[] VLGetNotifications();

    static public native void VLSearchConnect(@NotNull SearcherAccess searcher, @NotNull String libId);
    static public native void VLSearchStart(@NotNull SearcherAccess searcher, @NotNull Book query);
    static public native void VLSearchNextPage(@NotNull SearcherAccess searcher);
    static public native void VLSearchDetails(@NotNull SearcherAccess searcher, @NotNull Book book);
    static public native void VLSearchOrder(@NotNull SearcherAccess searcher, @NotNull Book[] book);
    static public native void VLSearchOrder(@NotNull SearcherAccess searcher, @NotNull Book[] book, @NotNull int[] holding);
    static public native void VLSearchCompletePendingMessage(@NotNull SearcherAccess searcher, int result);
    static public native void VLSearchEnd(@NotNull SearcherAccess searcher);

    static public native void VLSetOptions(@NotNull OptionsShared options);
    static public native @NotNull OptionsShared VLGetOptions();
    static public native OptionsAndroidOnly VLGetOptionsAndroidOnly();
    static public native void VLSetOptionsAndroidOnly(OptionsAndroidOnly oao);

    static public native @NotNull Book[] VLXQuery(@NotNull String query);

    static public native VersionInfo VLGetVersion();

    static public native void VLFinalize();


    //SearcherAccess helper class like in Pascal-VideLibri
    //All methods run asynchronously in a Pascal Thread
    //All events are called in the same thread ()
    public static class SearcherAccess{
        //set from Pascal side
        public long nativePtr;
        public volatile int totalResultCount;
        public volatile boolean nextPageAvailable;
        @NotNullLateInit public volatile FormParams searchParams; //from java and pascal

        //set in Java
        @NotNull public final String libId;
        @NotNull public final ArrayList<SearchEvent> pendingEvents = new ArrayList<>();

        //set in (Java) activity
        public int state;
        public long heartBeat;
        public boolean nextPageSearchPending;
        @NotNull public final ArrayList<Bridge.Book> bookCache = new ArrayList<>();
        public boolean loadingTaskList, loadingTaskDetails, loadingTaskOrder, loadingTaskOrderHolding, loadingTaskMessage;
        //The detail search runs in the background, for a single book.
        //But the user might request other detail searches, before the search is complete.
        //Then wait for the old search to complete, and then start the newest search, unless the user has closed the view
        public int waitingForDetails;    //nr of book currently searched. Only set when the search is started or has ended (-1 if no search is running)
        public int nextDetailsRequested; //nr of the book that *should* be searched. Set when requesting a new search, or to -1 to cancel the current search
        @Nullable public Bridge.Account orderingAccount;

        public SearcherAccess(@NotNull String libId){
            this.libId = libId;
        }
        public void connect(){
            VLSearchConnect(this, libId);
        }
        public void start(@NotNull Book query){
            VLSearchStart(this, query);
        }
        public void nextPage(){
            VLSearchNextPage(this);
        }
        public void details(@NotNull Book book){
            VLSearchDetails(this, book);
        }
        public void order(@NotNull Book book){
            VLSearchOrder(this, new Book[]{book});
        }
        public void order(@NotNull Book book, int holdingId){
            VLSearchOrder(this, new Book[]{book}, new int[]{holdingId});
        }
        public void completePendingMessage(int result){
            VLSearchCompletePendingMessage(this, result);
        }
        public void free(){
            VLSearchEnd(this);
        }

        private void send(SearchEvent event) {
            if (searchEventHandler == null) return;
            event.setSearcherAccess(this);
            searchEventHandler.sendMessage(searchEventHandler.obtainMessage(0, event));
        }


        public void onConnected(@NotNull FormParams params){ send(new SearchEvent.Connected(params)); }
        public void onSearchFirstPageComplete(@NotNull Book[] books) { send(new SearchEvent.FirstPage(books)); }
        public void onSearchNextPageComplete(@NotNull Book[] books) { send(new SearchEvent.NextPage(books)); }
        public void onSearchDetailsComplete(@NotNull Book book) { send(new SearchEvent.Details(book)); }
        public void onOrderComplete(@NotNull Book book) { send(new SearchEvent.OrderComplete(book)); }
        public void onTakePendingMessage(int kind, @NotNull String caption, @NotNull String[] options) {
            send(new SearchEvent.TakePendingMessage(kind, caption, options));
        }
        public void onPendingMessageCompleted() { send(new SearchEvent.PendingMessageComplete()); }
        public void onException() { send(new SearchEvent.Exception()); }
    }



    public static class ImportExportData {
        public static final int CURRENT  = 0x01;
        public static final int HISTORY  = 0x02;
        public static final int CONFIG   = 0x04;
        public static final int PASSWORD = 0x08;
        
        @Nullable public String accountsToImport[];
        public int flags;
        long nativePtr; //this is a very large object which must be destroyed with a call to  VLImportAccounts
    }
    public static native void VLExportAccounts(@NotNull String filename, @NotNull Account accountsToExport[], int flags);
    public static native @NotNull ImportExportData VLImportAccountsPrepare(@NotNull String filename);
    public static native void VLImportAccounts(@NotNull ImportExportData data);

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
    @Nullable static public Handler allThreadsDoneHandler, installationDoneHandler, searchEventHandler;

    //init



    static public boolean initialized = false;
    static public String userPath = "";

    static public void initialize(Context context){
        if (initialized) return;
        initialized = true;
        Log.i("Videlibri", "Trying to load liblclapp.so");
        try {
            System.loadLibrary("lclapp");
        } catch (UnsatisfiedLinkError e) {
            Log.i("Videlibri", "Android is broken, trying Relinker.");
            if (context != null) ReLinker.loadLibrary(context, "lclapp");
            else throw e;
        }
        Log.i("Videlibri", "Initializing Windows VM and Pascal layer");
        VLInit(context);
        //throw new Error("test");
    }
}
