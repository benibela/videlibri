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
        menu?.findItem(R.id.research_menu)?.isVisible = hasAccounts
    }

    override fun onOptionsItemIdSelected(id: Int): Boolean {
        when (id) {
            R.id.account_information -> {
                val info = if (VideLibriApp.accounts.isEmpty()) getString(R.string.main_no_accounts)
                           else VideLibriApp.accounts.joinToString (
                              separator = "\n\n",
                              transform = { "${it.prettyName}:\n${tr(R.string.main_last_refreshS, BookFormatter.formatDateFull(it.lastCheckDate))}" }
                           )
                showMessage(message = info, title = getString(R.string.menu_account_information))
            }
            R.id.research_same, R.id.research_author, R.id.research_title -> {
                currentBook()?.let {
                    if (it.account != null) {
                        val query = mapOf(
                                "title" to if (id == R.id.research_same || id == R.id.research_title) it.title else "",
                                "author" to if (id == R.id.research_same || id == R.id.research_author) it.author else "")
                        startActivity<Search>(
                                "libId" to it.account.libId,
                                "query" to query)
                    }
                }
            }
            else -> return super.onOptionsItemIdSelected(id)
        }
        return true
    }
}