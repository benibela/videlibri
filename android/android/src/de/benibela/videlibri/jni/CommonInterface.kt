@file:Suppress("EqualsOrHashCode", "unused")
package de.benibela.videlibri.jni

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
    val platform: String = "",
    val buildId: String = ""
  )   {
    override fun equals(other: Any?): Boolean =
       other != null && javaClass == other.javaClass && other is VersionInfo && version == other.version && platform == other.platform && buildId == other.buildId

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
       other != null && javaClass == other.javaClass && other is BookListDisplayOptions && showHistory == other.showHistory && noBorrowedBookDetails == other.noBorrowedBookDetails && showRenewCount == other.showRenewCount && groupingKey == other.groupingKey && sortingKey == other.sortingKey && filterKey == other.filterKey && alwaysFilterOnHistory == other.alwaysFilterOnHistory

  } 
  open class NotificationConfig( 
    @JvmField var enabled: Boolean = true,
    @JvmField var serviceDelay: Int = 15,
    @JvmField var lastTime: Long = 0,
    @JvmField var lastTitle: String = "",
    @JvmField var lastText: String = ""
  )   {
    override fun equals(other: Any?): Boolean =
       other != null && javaClass == other.javaClass && other is NotificationConfig && enabled == other.enabled && serviceDelay == other.serviceDelay && lastTime == other.lastTime && lastTitle == other.lastTitle && lastText == other.lastText

  } 
  open class OptionsAndroidOnly( 
    @JvmField var logging: Boolean,
    @JvmField var bookListDisplayOptions: BookListDisplayOptions,
    @JvmField var filterHistory: Array<String>,
    @JvmField var importExportFileName: String,
    @JvmField var additionalCertificatesBase64: Array<String>,
    @JvmField var notifications: NotificationConfig,
    @JvmField var hasBeenStartedAtLeastOnce: Boolean,
    @JvmField var accountCountBackup: Int = -1
  )   {
    override fun equals(other: Any?): Boolean =
       other != null && javaClass == other.javaClass && other is OptionsAndroidOnly && logging == other.logging && bookListDisplayOptions == other.bookListDisplayOptions && filterHistory.contentEquals(other.filterHistory) && importExportFileName == other.importExportFileName && additionalCertificatesBase64.contentEquals(other.additionalCertificatesBase64) && notifications == other.notifications && hasBeenStartedAtLeastOnce == other.hasBeenStartedAtLeastOnce && accountCountBackup == other.accountCountBackup

  } 
  open class OptionsShared( 
    @JvmField var nearTime: Int,
    @JvmField var refreshInterval: Int,
    @JvmField var userLibIds: Array<String>
  )   {
    override fun equals(other: Any?): Boolean =
       other != null && javaClass == other.javaClass && other is OptionsShared && nearTime == other.nearTime && refreshInterval == other.refreshInterval && userLibIds.contentEquals(other.userLibIds)

  } 
