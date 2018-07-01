package de.benibela.videlibri;


import android.graphics.Color;

import java.util.EnumSet;

class BookFormatter {
    static String tr_booklist_from = "from", tr_provided = "provided", tr_ordered = "ordered";

    static int getStatusColor(Bridge.Book book){
        int c = Color.GREEN;
        if (book.history) c = -1;
        else if ((book.account != null || book.more == VideLibri.crazyHeaderHack)
                && book.dueDate != null && book.dueDate.pascalDate - Bridge.currentPascalDate <= 3)
            c = Color.RED;
        else switch (book.getStatus()){
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

    static boolean isGroupingHeaderFakeBook(Bridge.Book book){
        return book.more == VideLibri.crazyHeaderHack;
    }


    static String shortened(String s){
        if (s.length() < 300) return s;
        else return s.substring(0,300) + "...";
    }

    static String getBookMoreText(Bridge.Book book){
        String more = "";
        if (!book.author.trim().equals(""))
            if (!book.author.startsWith("von") && !book.author.startsWith("by")) more = tr_booklist_from + " " + shortened(book.author);
            else more = " " + shortened(book.author);
        String year = book.getProperty("year");
        if (year != null && !"".equals(year)) more += " ; " + year;
        String id = book.getProperty("id");
        if (id != null && !"".equals(id)) more += " ; " + id;
        return more;
    }

    static String getBookDateText(Bridge.Book book, EnumSet<BookOverviewAdapter.DisplayEnum> options){
        if (book.account != null && !book.history ) { //lend book
            switch (book.getStatus()) {
                case Provided:  return tr_provided;
                case Ordered:  return tr_ordered;
                default:
                    String t = Util.formatDate(book.dueDate);
                    if (options.contains(BookOverviewAdapter.DisplayEnum.ShowRenewCount)) {
                        String renewCount = book.getProperty("renewCount");
                        if (!"".equals(renewCount) && !"0".equals(renewCount)) t = renewCount + "V " + t;
                    }
                    return t;
            }
        } else {
            String s = "";
            switch (book.getStatus()) {
                case Available: s = "\u2713"; break;
                case Lend: s = "\u2717"; break;
                case Virtual: s = "?"; break;
                case Presentation: s = "\u2717"; break;
                case InterLoan: s = "\u2717"; break;
            }
            return s;
        }
    }

    static String getStatusText(Bridge.Book book){
        String status = book.getProperty("status");
        if (status == null) status = "";
        if ("".equals(status))
            switch (book.getStatus()){
                case Problematic: status = Util.tr(R.string.book_status_problematic); break;
                case Ordered: status = Util.tr(R.string.book_status_ordered); break;
                case Provided: status = Util.tr(R.string.book_status_provided); break;
            }
        return status;
    }
}
