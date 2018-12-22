package de.benibela.videlibri

import android.view.Menu
import android.view.MenuInflater

class LibraryList: LibraryListOld() {
    override fun onCreateOptionsMenuOverflow(menu: Menu, inflater: MenuInflater) {
        super.onCreateOptionsMenuOverflow(menu, inflater)
        inflater.inflate(R.menu.librarylistmenu, menu)
    }

    companion object {
        fun lastSelectedFallbackLibraryId(): String? =
                if (System.currentTimeMillis() - lastSelectedTime < SELECTION_REUSE_TIME)
                    lastSelectedLibId
                else
                    null
    }

}