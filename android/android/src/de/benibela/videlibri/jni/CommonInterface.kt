//This file has been generated automatically. Do not edit it, do not read it.
//Refer to the interface.pretty file
@file:Suppress("unused")
package de.benibela.videlibri.jni



//Classes to represent input elements of an HTML form

  open class FormInput( 
    val name: String = "",
    val caption: String = "",
    val value: String = ""
  )   {
    override fun equals(other: Any?): Boolean =
       other != null && javaClass == other.javaClass && other is FormInput && name == other.name && caption == other.caption && value == other.value
    override fun hashCode(): Int =
      super.hashCode() xor name.hashCode().rotateLeft(1) xor caption.hashCode().rotateLeft(2) xor value.hashCode().rotateLeft(3)
  }

  open class FormSelect( 
    name: String = "",
    caption: String = "",
    value: String = "",
    val optionCaptions: Array<String> = emptyArray(),
    val optionValues: Array<String> = emptyArray()
  ) : FormInput(name, caption, value)  {
    override fun equals(other: Any?): Boolean =
       other != null && javaClass == other.javaClass && other is FormSelect && optionCaptions.contentEquals(other.optionCaptions) && optionValues.contentEquals(other.optionValues) && name == other.name && caption == other.caption && value == other.value
    override fun hashCode(): Int =
      super.hashCode() xor optionCaptions.contentHashCode().rotateLeft(1) xor optionValues.contentHashCode().rotateLeft(2)
  }

  open class FormParams( 
    val inputs: Array<FormInput> = emptyArray()
  )   {
    override fun equals(other: Any?): Boolean =
       other != null && javaClass == other.javaClass && other is FormParams && inputs.contentEquals(other.inputs)
    override fun hashCode(): Int =
      super.hashCode() xor inputs.contentHashCode().rotateLeft(1)
  }

  open class VersionInfo( 
    val version: String = "",
    val platform: String = "",
    val buildId: String = ""
  )   {
    override fun equals(other: Any?): Boolean =
       other != null && javaClass == other.javaClass && other is VersionInfo && version == other.version && platform == other.platform && buildId == other.buildId
    override fun hashCode(): Int =
      super.hashCode() xor version.hashCode().rotateLeft(1) xor platform.hashCode().rotateLeft(2) xor buildId.hashCode().rotateLeft(3)
  }


//Classes to store user configuration options

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
    override fun hashCode(): Int =
      super.hashCode() xor showHistory.hashCode().rotateLeft(1) xor noBorrowedBookDetails.hashCode().rotateLeft(2) xor showRenewCount.hashCode().rotateLeft(3) xor groupingKey.hashCode().rotateLeft(4) xor sortingKey.hashCode().rotateLeft(5) xor filterKey.hashCode().rotateLeft(6) xor alwaysFilterOnHistory.hashCode().rotateLeft(7)
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
    override fun hashCode(): Int =
      super.hashCode() xor enabled.hashCode().rotateLeft(1) xor serviceDelay.hashCode().rotateLeft(2) xor lastTime.hashCode().rotateLeft(3) xor lastTitle.hashCode().rotateLeft(4) xor lastText.hashCode().rotateLeft(5)
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
    override fun hashCode(): Int =
      super.hashCode() xor logging.hashCode().rotateLeft(1) xor bookListDisplayOptions.hashCode().rotateLeft(2) xor filterHistory.contentHashCode().rotateLeft(3) xor importExportFileName.hashCode().rotateLeft(4) xor additionalCertificatesBase64.contentHashCode().rotateLeft(5) xor notifications.hashCode().rotateLeft(6) xor hasBeenStartedAtLeastOnce.hashCode().rotateLeft(7) xor accountCountBackup.hashCode().rotateLeft(8)
  }

  open class OptionsShared( 
    @JvmField var nearTime: Int = 0,
    @JvmField var refreshInterval: Int = 0,
    @JvmField var userLibIds: Array<String> = emptyArray()
  )   {
    override fun equals(other: Any?): Boolean =
       other != null && javaClass == other.javaClass && other is OptionsShared && nearTime == other.nearTime && refreshInterval == other.refreshInterval && userLibIds.contentEquals(other.userLibIds)
    override fun hashCode(): Int =
      super.hashCode() xor nearTime.hashCode().rotateLeft(1) xor refreshInterval.hashCode().rotateLeft(2) xor userLibIds.contentHashCode().rotateLeft(3)
  }


//Classes to represent metadata of a library
typealias LibraryTestingInfoInt = Int
object LibraryTestingInfo {
    const val Unknown = 0
    const val Yes = 1
    const val No = 2
    const val Broken = 3
}


  open class LibraryVariable( 
    @JvmField val name: String = "",
    @JvmField val value: String = ""
  )   {
    override fun equals(other: Any?): Boolean =
       other != null && javaClass == other.javaClass && other is LibraryVariable && name == other.name && value == other.value
    override fun hashCode(): Int =
      super.hashCode() xor name.hashCode().rotateLeft(1) xor value.hashCode().rotateLeft(2)
  }

  open class LibraryDetails( 
    @JvmField var id: String = "",
    @JvmField var prettyName: String = "",
    @JvmField var prettyNameShort: String = "",
    @JvmField var fhomepageUrl: String = "",
    @JvmField var fcatalogueUrl: String = "",
    @JvmField var fcatalogueUrlFromTemplate: String = "",
    @JvmField var tableComment: String = "",
    @JvmField var accountComment: String = "",
    @JvmField var templateId: String = "",
    @JvmField var variables: Array<LibraryVariable> = emptyArray(),
    @JvmField var segregatedAccounts: Boolean = false,
    @JvmField var email: String = "",
    @JvmField var testingSearch: LibraryTestingInfoInt = 0,
    @JvmField var testingAccount: LibraryTestingInfoInt = 0
  )   {
    override fun equals(other: Any?): Boolean =
       other != null && javaClass == other.javaClass && other is LibraryDetails && id == other.id && prettyName == other.prettyName && prettyNameShort == other.prettyNameShort && fhomepageUrl == other.fhomepageUrl && fcatalogueUrl == other.fcatalogueUrl && fcatalogueUrlFromTemplate == other.fcatalogueUrlFromTemplate && tableComment == other.tableComment && accountComment == other.accountComment && templateId == other.templateId && variables.contentEquals(other.variables) && segregatedAccounts == other.segregatedAccounts && email == other.email && testingSearch == other.testingSearch && testingAccount == other.testingAccount
    override fun hashCode(): Int =
      super.hashCode() xor id.hashCode().rotateLeft(1) xor prettyName.hashCode().rotateLeft(2) xor prettyNameShort.hashCode().rotateLeft(3) xor fhomepageUrl.hashCode().rotateLeft(4) xor fcatalogueUrl.hashCode().rotateLeft(5) xor fcatalogueUrlFromTemplate.hashCode().rotateLeft(6) xor tableComment.hashCode().rotateLeft(7) xor accountComment.hashCode().rotateLeft(8) xor templateId.hashCode().rotateLeft(9) xor variables.contentHashCode().rotateLeft(10) xor segregatedAccounts.hashCode().rotateLeft(11) xor email.hashCode().rotateLeft(12) xor testingSearch.hashCode().rotateLeft(13) xor testingAccount.hashCode().rotateLeft(14)
  }


//Information about a supported library system

  open class TemplateDetails( 
    val description: String = "",
    val variablesNames: Array<String> = emptyArray(),
    val variablesDescription: Array<String> = emptyArray(),
    val variablesDefault: Array<String> = emptyArray()
  )   {
    override fun equals(other: Any?): Boolean =
       other != null && javaClass == other.javaClass && other is TemplateDetails && description == other.description && variablesNames.contentEquals(other.variablesNames) && variablesDescription.contentEquals(other.variablesDescription) && variablesDefault.contentEquals(other.variablesDefault)
    override fun hashCode(): Int =
      super.hashCode() xor description.hashCode().rotateLeft(1) xor variablesNames.contentHashCode().rotateLeft(2) xor variablesDescription.contentHashCode().rotateLeft(3) xor variablesDefault.contentHashCode().rotateLeft(4)
  }


//Book information
typealias BookStatusInt = Int
object BookStatus {
    const val Unknown = 0
    const val Problematic = 5
    const val Normal = 6
    const val Ordered = 8
    const val Provided = 9
    const val Reserved = 10
    const val Available = 100
    const val Lend = 101
    const val Virtual = 102
    const val Presentation = 103
    const val InterLoan = 104
}



//Error handling
typealias PendingExceptionKindInt = Int
object PendingExceptionKind {
    const val Unknown = 0
    const val Internet = 1
    const val Login = 2
}


  open class PendingException( 
    val kind: PendingExceptionKindInt = 0,
    val accountPrettyNames: String = "",
    val error: String = "",
    val libraryIds: String = "",
    val searchQuery: String = "",
    val details: String = "",
    val anonymousDetails: String = "",
    val firstAccountUser: String = "",
    val firstAccountLib: String = ""
  )   {
    override fun equals(other: Any?): Boolean =
       other != null && javaClass == other.javaClass && other is PendingException && kind == other.kind && accountPrettyNames == other.accountPrettyNames && error == other.error && libraryIds == other.libraryIds && searchQuery == other.searchQuery && details == other.details && anonymousDetails == other.anonymousDetails && firstAccountUser == other.firstAccountUser && firstAccountLib == other.firstAccountLib
    override fun hashCode(): Int =
      super.hashCode() xor kind.hashCode().rotateLeft(1) xor accountPrettyNames.hashCode().rotateLeft(2) xor error.hashCode().rotateLeft(3) xor libraryIds.hashCode().rotateLeft(4) xor searchQuery.hashCode().rotateLeft(5) xor details.hashCode().rotateLeft(6) xor anonymousDetails.hashCode().rotateLeft(7) xor firstAccountUser.hashCode().rotateLeft(8) xor firstAccountLib.hashCode().rotateLeft(9)
  }

  open class PendingExceptions( 
    val exceptions: Array<PendingException> = emptyArray()
  )   {
    override fun equals(other: Any?): Boolean =
       other != null && javaClass == other.javaClass && other is PendingExceptions && exceptions.contentEquals(other.exceptions)
    override fun hashCode(): Int =
      super.hashCode() xor exceptions.contentHashCode().rotateLeft(1)
  }
