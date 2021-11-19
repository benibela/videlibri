package de.benibela.videlibri.jni

fun BookListDisplayOptions.isGrouped() = groupingKey != ""

fun OptionsAndroidOnly.save(){
    Bridge.VLSetOptionsAndroidOnly(globalOptionsAndroid)
}

lateinit var globalOptionsAndroid: OptionsAndroidOnly