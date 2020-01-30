package de.benibela.multilevellistview

import android.content.Context
import android.util.AttributeSet

interface PartialMultiLevelListView {
    fun expand(id: Long): Int
    var autoExpandSingleChildren: Boolean
}
