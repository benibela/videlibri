package de.benibela.videlibri

import de.benibela.videlibri.jni.Bridge
import java.util.*

object Accounts: Collection<Bridge.Account> {
    private var accounts = arrayOf<Bridge.Account>()
    override fun contains(element: Bridge.Account): Boolean = this.accounts.contains(element)
    override fun containsAll(elements: Collection<Bridge.Account>): Boolean = elements.all { this.contains(it) }
    override fun isEmpty(): Boolean = accounts.isEmpty()
    override val size: Int
        get() = accounts.size
    override fun iterator(): Iterator<Bridge.Account> {
        return accounts.iterator()
    }


    operator fun get(i: Int) = accounts.getOrNull(i)

    val toArray get() = accounts

    @JvmStatic fun get(libId: String, userName: String): Bridge.Account? {
        accounts.forEach { acc ->
            if (acc.libId == libId && acc.name == userName)
                return acc
        }
        return null
    }


    @JvmStatic fun add(acc: Bridge.Account) {
        Bridge.VLAddAccount(acc)
        refreshAll()
        VideLibriApp.updateAccount(acc, false, false)
    }

    @JvmStatic fun delete(acc: Bridge.Account?) {
        if (acc == null) return
        Bridge.VLDeleteAccount(acc)
        refreshAll()
    }

    @JvmStatic fun change(old: Bridge.Account, newacc: Bridge.Account) {
        Bridge.VLChangeAccount(old, newacc)
        if (old.isHidden) {
            old.isShown = true
            newacc.isHidden = true
        }
        val updating = old.isUpdating
        if (updating && old != newacc) {
            old.isUpdating = false
            newacc.isUpdating = true
        }
        refreshAll()
        if (!updating)
            VideLibriApp.updateAccount(newacc, false, false)
    }

    @JvmStatic fun refreshAll() {
        refreshAccounts()
        refreshDisplay()
    }

    @JvmStatic fun refreshAccounts() {
        val oldHidden = hiddenAccounts.keys.toTypedArray()
        this.accounts = Bridge.VLGetAccounts()
        hiddenAccounts.clear()
        oldHidden.forEach { acc -> get(acc.libId, acc.name)?.let { hiddenAccounts.put(acc, true) } }
    }

    @JvmStatic fun refreshDisplay() {
        LendingList.refreshDisplayedLendBooks()
    }


    fun allUpdatesComplete(){
        runningUpdates.clear()
    }
    fun filterWithRunningUpdate() = runningUpdates

    //private fun filterReal() = this.accounts.filter { it.isReal }



    fun showAll(){
        hiddenAccounts.clear()
        refreshDisplay()
    }
    fun hideAll(){
        hiddenAccounts.clear()
        forEach { hiddenAccounts[it] = true }
        refreshDisplay()
    }
    fun filterHidden() = hiddenAccounts.keys
}
val accounts = Accounts



private var runningUpdates = mutableSetOf<Bridge.Account>()
var Bridge.Account.isUpdating: Boolean
    get() = runningUpdates.contains(this)
    set(value){
        if (value) runningUpdates.add(this)
        else runningUpdates.remove(this)
    }



private var hiddenAccounts = IdentityHashMap<Bridge.Account, Boolean>()
var Bridge.Account.isHidden: Boolean
    get() = hiddenAccounts[this] == true
    set(h){
        if (h == isHidden) return
        if (h) hiddenAccounts[this] = true else hiddenAccounts.remove(this)
        accounts.refreshDisplay()
    }
var Bridge.Account.isShown: Boolean
    inline get() = !isHidden
    inline set(value){ isHidden = !value}
val Bridge.Account.isReal: Boolean
    inline get() = name.isNotEmpty() || pass.isNotEmpty()
fun Bridge.Account.showAlone() {
    hiddenAccounts.clear()
    accounts.filter { it !== this }.forEach { hiddenAccounts[it] = true }
    accounts.refreshDisplay()
}