package de.benibela.videlibri

import android.view.Menu
import android.view.MenuInflater

open class BookListActivity: BookListActivityOld(){

    override fun onCreateOptionsMenuOverflow(menu: Menu, inflater: MenuInflater) {
        super.onCreateOptionsMenuOverflow(menu, inflater)
        inflater.inflate(R.menu.sharemenu, menu)
    }
}