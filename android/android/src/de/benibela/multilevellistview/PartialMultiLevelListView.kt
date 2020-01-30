package de.benibela.multilevellistview

interface PartialMultiLevelListView {
    fun expand(id: Long): Int
    var autoExpandSingleChildren: Boolean
}
