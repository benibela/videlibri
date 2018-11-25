package de.benibela.videlibri

import android.annotation.SuppressLint
import android.content.Intent
import android.os.Build
import android.view.Menu
import android.view.MenuInflater
import de.benibela.videlibri.jni.Bridge

@SuppressLint("Registered")
open class BookListActivity: BookListActivityOld(){

    override fun onCreateOptionsMenuOverflow(menu: Menu, inflater: MenuInflater) {
        super.onCreateOptionsMenuOverflow(menu, inflater)
        inflater.inflate(R.menu.booklistmenu, menu)
    }

    fun currentBook(): Bridge.Book? =
        if (detailsVisible() && details.book != null) details.book
        else if (currentBookPos in bookCache.indices) bookCache[currentBookPos]
        else null


    override fun onOptionsItemIdSelected(id: Int): Boolean {
        when (id) {
            R.id.share -> {
                val sendIntent = Intent()
                sendIntent.action = Intent.ACTION_SEND
                sendIntent.putExtra(Intent.EXTRA_TEXT,
                        (if (listVisible()) list.exportShare(false) + "\n\n" else "")
                                + (if (detailsVisible()) details.exportShare(false) + "\n\n" else "")
                                + tr(R.string.share_export_footer)
                )
                if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.JELLY_BEAN)
                    sendIntent.putExtra(Intent.EXTRA_HTML_TEXT,
                            ((if (listVisible()) list.exportShare(true) + "<br>\n<br>\n" else "")
                                    + (if (detailsVisible()) details.exportShare(true) + "<br>\n<br>\n" else "")
                                    + tr(R.string.share_export_footer))
                    )
                sendIntent.type = "text/plain"
                startActivity(Intent.createChooser(sendIntent, getText(R.string.menu_share)))
                return true
            }
        }
        return super.onOptionsItemIdSelected(id)
    }

}