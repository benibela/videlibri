@file:Suppress("EqualsOrHashCode", "unused")
package de.benibela.videlibri.jni;

  open class FormInput( 
    val name: String = "",
    val caption: String = "",
    val value: String = ""
  )   {
    override fun equals(other: Any?): Boolean =
       other != null && javaClass == other.javaClass && other is FormInput && name == other.name && caption == other.caption && value == other.value

  } 
  open class FormSelect( 
    name: String,
    caption: String,
    value: String,
    val optionCaptions: Array<String>,
    val optionValues: Array<String>
  ) : FormInput(name, caption, value)  {
    override fun equals(other: Any?): Boolean =
       other != null && javaClass == other.javaClass && other is FormSelect && optionCaptions.contentEquals(other.optionCaptions) && optionValues.contentEquals(other.optionValues) && name == other.name && caption == other.caption && value == other.value

  } 
  open class FormParams( 
    val inputs: Array<FormInput>
  )   {
    override fun equals(other: Any?): Boolean =
       other != null && javaClass == other.javaClass && other is FormParams && inputs.contentEquals(other.inputs)

  } 
  open class VersionInfo( 
    val version: String = "",
    val platform: String = ""
  )   {
    override fun equals(other: Any?): Boolean =
       other != null && javaClass == other.javaClass && other is VersionInfo && version == other.version && platform == other.platform

  } 
  data class BookListDisplayOptions( 
    @JvmField var showHistory: Boolean = false,
    @JvmField var noBorrowedBookDetails: Boolean = false,
    @JvmField var showRenewCount: Boolean = true,
    @JvmField var groupingKey: String = "_dueWeek",
    @JvmField var sortingKey: String = "dueDate",
    @JvmField var filterKey: String = "",
    @JvmField var alwaysFilterOnHistory: Boolean = true
  )   {
    override fun equals(other: Any?): Boolean =
       other != null && javaClass == other.javaClass && other is BookListDisplayOptions && noDetailsInOverview == other.noDetailsInOverview && showRenewCount == other.showRenewCount && groupingKey == other.groupingKey && sortingKey == other.sortingKey && filterKey == other.filterKey

  } 
