package de.benibela.videlibri

import android.view.Menu
import android.view.MenuInflater

class LendingList: VideLibriOld(){

    override fun onCreateOptionsMenuPrimary(menu: Menu, inflater: MenuInflater) {
        super.onCreateOptionsMenuPrimary(menu, inflater)
        inflater.inflate(R.menu.filtermenu, menu)
    }

    override fun onCreateOptionsMenuOverflow(menu: Menu, inflater: MenuInflater) {
        inflater.inflate(R.menu.lendinglistmenu, menu)
        super.onCreateOptionsMenuOverflow(menu, inflater)
    }

    override fun setOptionMenuVisibility(menu: Menu?) {
        super.setOptionMenuVisibility(menu)
        val hasAccounts = VideLibriApp.accounts.size > 0
        menu?.findItem(R.id.refresh)?.isVisible = VideLibriApp.runningUpdates.isEmpty()
        menu?.findItem(R.id.filter)?.isVisible = hasAccounts
        menu?.findItem(R.id.renew)?.isVisible = hasAccounts
        menu?.findItem(R.id.renewlist)?.isVisible = hasAccounts
    }
}