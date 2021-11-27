package de.benibela.videlibri.jni

fun BookListDisplayOptions.isGrouped() = groupingKey != ""

fun OptionsAndroidOnly.save(){
    Bridge.VLSetOptionsAndroidOnly(this)
}

lateinit var globalOptionsAndroid: OptionsAndroidOnly
lateinit var globalOptionsShared: OptionsShared