@file:Suppress("MemberVisibilityCanBePrivate", "unused")

package de.benibela.videlibri.components

import android.content.Context
import android.service.controls.templates.ControlTemplate
import androidx.annotation.ArrayRes
import androidx.annotation.StringRes
import androidx.preference.*
import kotlin.reflect.KMutableProperty0

class PreferenceScreenBuilder(val context: Context, val screen: PreferenceScreen, init: PreferenceScreenBuilder.() -> Unit) {
    var count = 0
    //val categories = mutableListOf<PreferenceCategory>()
    fun category(@StringRes title: Int, init: CategoryBuilder.() -> Unit)
        = category(context.getString(title), init)
    fun category(title: String, init: CategoryBuilder.() -> Unit) =
        PreferenceCategory(context).also { c ->
            c.title = title
            screen.addPreference(c)
            init(CategoryBuilder(context, c))
        }
/*    fun build(){
        categories.forEach {screen.addPreference(it)}
        categories.clear()
    }*/
    init {
        init(this)
    }
}

class CategoryBuilder(val ctx: Context, val category: PreferenceCategory){
    private  fun<B: AbstractPreferenceBuilder<*,*>> make(b: B, init: B.() -> Unit)
            = b.also(init).preference.also { category.addPreference(it) }
    fun preference(customPreference: Preference? = null, init: PreferenceBuilder.() -> Unit)
            = make(PreferenceBuilder(ctx, customPreference), init)
    fun switchCompat(customPreference: SwitchPreferenceCompat? = null, init: SwitchPreferenceCompatBuilder.() -> Unit)
        = make(SwitchPreferenceCompatBuilder(ctx, customPreference), init)
    fun list(customPreference: ListPreference? = null, init: ListPreferenceBuilder.() -> Unit)
        = make(ListPreferenceBuilder(ctx, customPreference), init)
    fun seekBar(customPreference: SeekBarPreference? = null, init: SeekBarBuilder.() -> Unit)
        = make(SeekBarBuilder(ctx, customPreference), init)
}

abstract class PreferenceChangeListenerWrapper<V>: Preference.OnPreferenceChangeListener {
    var setter: ((V) -> Unit)? = null
    var outerListener: ((Preference?, V) -> Boolean)? = null
    var afterListener: ((Preference?, V) -> Unit)? = null
    inline fun<reified W: V> onPreferenceChangeReified(preference: Preference?, newValue: Any?): Boolean =
            when {
                newValue !is W -> false
                outerListener?.invoke(preference, newValue) == false -> false
                else -> {
                    setter?.invoke(newValue)
                    afterListener?.invoke(preference, newValue)
                    true
                }
            }

}
class StringPreferenceChangeListenerWrapper: PreferenceChangeListenerWrapper<String>(){
    override fun onPreferenceChange(preference: Preference?, newValue: Any?): Boolean
            = onPreferenceChangeReified<String>(preference, newValue)
}
class BooleanPreferenceChangeListenerWrapper: PreferenceChangeListenerWrapper<Boolean>(){
    override fun onPreferenceChange(preference: Preference?, newValue: Any?): Boolean
            = onPreferenceChangeReified<Boolean>(preference, newValue)
}
class IntPreferenceChangeListenerWrapper: PreferenceChangeListenerWrapper<Int>(){
    override fun onPreferenceChange(preference: Preference?, newValue: Any?): Boolean
            = onPreferenceChangeReified<Int>(preference, newValue)
}
class UnitPreferenceChangeListenerWrapper: PreferenceChangeListenerWrapper<Unit>(){
    override fun onPreferenceChange(preference: Preference?, newValue: Any?): Boolean
            = true
}

abstract class AbstractPreferenceBuilder<T: Preference, V>(val ctx: Context, val preference: T){
    val wrappedChangeListener: PreferenceChangeListenerWrapper<V> by lazy {
        makeWrappedChangeListener().also ( preference::setOnPreferenceChangeListener )
    }
    protected abstract fun makeWrappedChangeListener(): PreferenceChangeListenerWrapper<V>
    fun title(@StringRes title: Int) = title(ctx.getString(title))
    fun title(title: String) { preference.title = title }
    fun summary(@StringRes summary: Int) = summary(ctx.getString(summary))
    fun summary(summary: String) { preference.summary = summary }
    //fun dependency(key: String) { preference.dependency = key } this crashes??
    open fun property(p: KMutableProperty0<V>){
        wrappedChangeListener.setter = p::set
    }
    fun onChange(listener: (Preference?, V) -> Boolean){
        wrappedChangeListener.outerListener = listener
    }
    fun onChanged(listener: (Preference?, V) -> Unit){
        wrappedChangeListener.afterListener = listener
    }
    //fun intent(intent: Intent) { pref.intent = intent }
    fun onClick(listener: Preference.OnPreferenceClickListener) { preference.onPreferenceClickListener = listener }
    fun onClick(listener: () -> Unit) { preference.onPreferenceClickListener = Preference.OnPreferenceClickListener {
        run { listener(); true }
    } }
    init {
        preference.isPersistent = false
        preference.key = "#temp${totalAutoCreatedPreferences}" //we do not need a key, but without key ListPreference crashes. Do not reuse keys as that might interfere with view caching
        totalAutoCreatedPreferences += 1
    }
}

var totalAutoCreatedPreferences = 0

class PreferenceBuilder(ctx: Context, customPreference: Preference?):
        AbstractPreferenceBuilder<Preference, Unit>(ctx, customPreference ?: Preference(ctx)) {
    override fun makeWrappedChangeListener(): PreferenceChangeListenerWrapper<Unit> = UnitPreferenceChangeListenerWrapper()
}

class SwitchPreferenceCompatBuilder(ctx: Context, customPreference: SwitchPreferenceCompat?):
        AbstractPreferenceBuilder<SwitchPreferenceCompat, Boolean>(ctx, customPreference ?: SwitchPreferenceCompat(ctx)){
    override fun makeWrappedChangeListener(): PreferenceChangeListenerWrapper<Boolean> = BooleanPreferenceChangeListenerWrapper()
    fun summaryOn(@StringRes summaryOn: Int) = summaryOn(ctx.getString(summaryOn))
    fun summaryOn(summaryOn: String) { preference.summaryOn = summaryOn }
    fun summaryOff(@StringRes summaryOff: Int) = summaryOff(ctx.getString(summaryOff))
    fun summaryOff(summaryOff: String) { preference.summaryOff = summaryOff }
    override fun property(p: KMutableProperty0<Boolean>) {
        super.property(p)
        preference.isChecked = p.get()
    }
}

class ListPreferenceBuilder(ctx: Context, customPreference: ListPreference?):
        AbstractPreferenceBuilder<ListPreference, String>(ctx, customPreference ?: ListPreference(ctx)){
    override fun makeWrappedChangeListener(): PreferenceChangeListenerWrapper<String> = StringPreferenceChangeListenerWrapper()
    fun dialogTitle(@StringRes dialogTitle: Int) = dialogTitle(ctx.getString(dialogTitle))
    fun dialogTitle(dialogTitle: String) { preference.dialogTitle = dialogTitle }
    fun entries(@ArrayRes entries: Int) { preference.setEntries(entries) }
    fun entryValues(@ArrayRes entryValues: Int) { preference.setEntryValues(entryValues) }
    override fun property(p: KMutableProperty0<String>) {
        super.property(p)
        preference.value = p.get()
    }
}

class SeekBarBuilder(ctx: Context, customPreference: SeekBarPreference?):
        AbstractPreferenceBuilder<SeekBarPreference, Int>(ctx, customPreference ?: SeekBarPreference(ctx)){
    override fun makeWrappedChangeListener(): PreferenceChangeListenerWrapper<Int> = IntPreferenceChangeListenerWrapper()
    fun min(v: Int){ preference.min = v }
    fun max(v: Int){ preference.max = v }
    override fun property(p: KMutableProperty0<Int>) {
        super.property(p)
        preference.value = p.get()
    }
}

/*
class ListBuilder(context: Context): PreferenceBuilder(context, Preference(context)){

    fun dialogTitle(@StringRes dialogTitle: Int) = dialogTitle(context.getString(dialogTitle))
    fun dialogTitle(dialogTitle: String) { }// pref.dialogTitle = dialogTitle }
    fun entries(@ArrayRes entries: Int) {  }//pref.setEntries(entries) }
    fun entryValues(@ArrayRes entryValues: Int) {  }//pref.setEntryValues(entryValues) }
    fun property(p: KMutableProperty0<String>){
        pref.value = p()
        pref.setOnPreferenceClickListener { run { p.set(pref.value); true }}
    }
    init {
        pref.isPersistent = false
    }
}

class ListBuilder(context: Context): PreferenceBuilder(context, DropDownPreference(context)){
    override val pref = super.pref as DropDownPreference
    fun dialogTitle(@StringRes dialogTitle: Int) = dialogTitle(context.getString(dialogTitle))
    fun dialogTitle(dialogTitle: String) { pref.dialogTitle = dialogTitle }
    fun entries(@ArrayRes entries: Int) { pref.setEntries(entries) }
    fun entryValues(@ArrayRes entryValues: Int) { pref.setEntryValues(entryValues) }
    fun property(p: KMutableProperty0<String>){
        pref.value = p()
        pref.setOnPreferenceClickListener { run { p.set(pref.value); true }}
    }
    init {
        pref.isPersistent = false
    }
}*/