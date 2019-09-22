@file:Suppress("EqualsOrHashCode")
package de.benibela.videlibri.jni;
class PropertyArray(val names: Array<String>, values: Array<String>)

  open class FormInput( 
    val name: String,
    val caption: String,
    val value: String
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
