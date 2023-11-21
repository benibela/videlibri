package de.benibela.videlibri.jni;

import static de.benibela.videlibri.jni.BookStatus.Provided;
import static de.benibela.videlibri.jni.BookStatus.Reserved;

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

        public int status;

        @Nullable public Book [] holdings;

        @Nullable public Bitmap image; //handled on Javasite only

        @Override @NotNull
        public String toString() {
            return title; //used for copy to clipboard. where else? todo: probably add author
        }

        final public boolean hasOrderedStatus(){
            switch (status) {
                case BookStatus.Ordered: case BookStatus.Provided: case BookStatus.Reserved: return true;
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

    static public native void VLInit(@NotNull Context context);
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
    static public native @Nullable PendingExceptions VLTakePendingExceptions();
    static public native boolean VLSendFeedback(String[] feedBack);


    static public native @Nullable String[] VLGetNotifications();

    static public native void VLSearchConnect(@NotNull SearcherAccessPascal searcher, @NotNull String libId);
    static public native void VLSearchStart(@NotNull SearcherAccessPascal searcher, @NotNull Book query);
    static public native void VLSearchNextPage(@NotNull SearcherAccessPascal searcher);
    static public native void VLSearchDetails(@NotNull SearcherAccessPascal searcher, @NotNull Book book);
    static public native void VLSearchOrder(@NotNull SearcherAccessPascal searcher, @NotNull Book[] book);
    static public native void VLSearchOrder(@NotNull SearcherAccessPascal searcher, @NotNull Book[] book, @NotNull int[] holding);
    static public native void VLSearchCompletePendingMessage(@NotNull SearcherAccessPascal searcher, int result);
    static public native void VLSearchEnd(@NotNull SearcherAccessPascal searcher);

    static public native void VLSetOptions(@NotNull OptionsShared options);
    static public native @NotNull OptionsShared VLGetOptions();
    static public native OptionsAndroidOnly VLGetOptionsAndroidOnly();
    static public native void VLSetOptionsAndroidOnly(OptionsAndroidOnly oao);

    static public native @NotNull Book[] VLXQuery(@NotNull String query);

    static public native VersionInfo VLGetVersion();

    static public native void VLFinalize();





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



    static public String userPath = "";

}
