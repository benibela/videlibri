-keep class de.benibela.** { *; }
-keep class okhttp3.** { *; }

-keep class java.util.Locale { *; }
-keep class java.io.InputStream { *; }

-keep class java.util.ArrayList { *; }

-keep class android.content.Context { *; }

-keep class Landroid.content.res.AssetManager { *; }

# parcelable
-keepclassmembers class * implements android.os.Parcelable {
      public static final android.os.Parcelable$Creator *;
      public <init>(***);
}

