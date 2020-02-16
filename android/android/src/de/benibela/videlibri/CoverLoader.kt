package de.benibela.videlibri

import android.content.Context
import android.graphics.Bitmap
import android.util.Log
import com.bumptech.glide.Glide
import com.bumptech.glide.GlideBuilder
import com.bumptech.glide.Priority
import com.bumptech.glide.load.data.DataFetcher
import com.bumptech.glide.load.engine.bitmap_recycle.BitmapPool
import com.bumptech.glide.load.model.GenericLoaderFactory
import com.bumptech.glide.load.model.ModelLoader
import com.bumptech.glide.load.model.ModelLoaderFactory
import com.bumptech.glide.load.model.stream.StreamModelLoader
import com.bumptech.glide.load.resource.bitmap.BitmapTransformation
import com.bumptech.glide.load.resource.bitmap.TransformationUtils
import com.bumptech.glide.module.GlideModule
import com.bumptech.glide.request.animation.GlideAnimation
import com.bumptech.glide.request.target.SimpleTarget
import de.benibela.videlibri.jni.Bridge
import java.io.InputStream


@Suppress("unused")
internal class BookCoverGlideModule : GlideModule {
    override fun applyOptions(context: Context?, builder: GlideBuilder?) {}
    override fun registerComponents(context: Context?, glide: Glide) {
        glide.register(Bridge.Book::class.java, InputStream::class.java, BookCoverModelLoader.Factory())
    }
}

//see https://github.com/bumptech/glide/issues/606
internal class BookCoverModelLoader(private val loader: ModelLoader<String, InputStream>) : StreamModelLoader<Bridge.Book?> {
    override fun getResourceFetcher(book: Bridge.Book?, width: Int,
                                    height: Int): DataFetcher<InputStream> {
        return object : DataFetcher<InputStream> {
            private var fetcher: DataFetcher<InputStream>? = null
            override fun getId(): String =
                book?.getProperty("isbn")?.takeNonEmpty() ?: book?.getProperty("image-url")?.takeNonEmpty() ?: "unknown"

            @Throws(Exception::class)
            override fun loadData(priority: Priority?): InputStream? {
                var lastException: java.lang.Exception? = null
                CoverLoader.processCoverUrls(book ?: return null) { url ->
                    if (url.isBlank()) null
                    else try {
                        fetcher = loader.getResourceFetcher(url, width, height)
                        fetcher?.loadData(priority)
                    } catch (ex: Exception) {
                        fetcher?.cleanup();
                        lastException = ex
                        null
                    }
                }?.let { return it }
                lastException?.let { throw it }
                return null
            }

            override fun cleanup() { fetcher?.cleanup() }
            override fun cancel() { fetcher?.cancel() }
        }
    }

    internal class Factory : ModelLoaderFactory<Bridge.Book?, InputStream?> {
        override fun build(context: Context?,
                           factories: GenericLoaderFactory): ModelLoader<Bridge.Book?, InputStream?>? {
            return BookCoverModelLoader(factories.buildModelLoader(String::class.java, InputStream::class.java))
        }

        override fun teardown() {}
    }

}

class BitmapScaler(context: Context): BitmapTransformation(context){
    override fun getId(): String = "scaled"

    override fun transform(pool: BitmapPool?, cover: Bitmap?, maxWidth: Int, maxHeight: Int): Bitmap? {
        if (cover == null) return null
        val minWidth = maxWidth
        val minHeight = maxHeight
        var scale = 1.0
        if (cover.width < minWidth || cover.height < minHeight) {
            scale = Math.max(minWidth * 1.0 / cover.width, minHeight * 1.0 / cover.height)
        }
        if (cover.width * scale > maxWidth || cover.height * scale > maxHeight) {
            scale = Math.min(maxWidth * 1.0 / cover.width, maxHeight * 1.0 / cover.height)
        }
        Log.i("IMAGE SIZING", "${cover.width}x${cover.height}    ---  ${maxWidth}x${maxHeight}     ${minWidth}x${minHeight}  --- ${scale}")
        if (scale < 1.0 ) return TransformationUtils.fitCenter(cover, pool, maxWidth, maxHeight)
        if (scale > 1.0) return Bitmap.createScaledBitmap(cover, (cover.width * scale).toInt(), (cover.height * scale).toInt(), true)
        return cover
    }

}

class CoverLoader {
    companion object {
        @JvmStatic fun loadBookCover(bookDetails: BookDetails, book: Bridge.Book) {
            Glide.with(bookDetails.activity ?: return)
                    .load(book)
                    .asBitmap()
                    .transform(BitmapScaler(bookDetails.activity))
                    .into(object : SimpleTarget<Bitmap?>(bookDetails.coverTargetWidth, bookDetails.coverTargetHeight){
                        override fun onResourceReady(resource: Bitmap?, glideAnimation: GlideAnimation<in Bitmap?>?) {
                            Log.i("IMAGE FINAL", "${resource?.width}x${resource?.height}")
                            bookDetails.setCoverBitmap(book, resource)
                        }
                    })
        }

        fun<T> processCoverUrls(book: Bridge.Book, load: (String) -> T?): T?{
            book.getProperty("image-url").split("[\r\n]").forEach { url ->
                load(url)?.let { return it }
            }

            val isbn = book.getProperty("isbn").trim()
            if (isbn.isNotEmpty()) {
                val normalizedISBN10 = book.getNormalizedISBN(true, Bridge.Book.ISBNNormalization.ISBN_CONVERT_TO_10)
                load("http://images-eu.amazon.com/images/P/$normalizedISBN10.03.L.jpg")?.let { return it }
                load("http://covers.openlibrary.org/b/isbn/$normalizedISBN10-M.jpg?default=false")?.let { return it }

                val normalizedISBN13 = book.getNormalizedISBN(true, Bridge.Book.ISBNNormalization.ISBN_CONVERT_TO_13)
                load("https://www.buchhandel.de/cover/$normalizedISBN13/$normalizedISBN13-cover-m.jpg")?.let { return it }
            }
            return null
        }

    }


}

fun BookDetails.setCoverBitmap(book: Bridge.Book , bmp: Bitmap? ){
    book.image = bmp
    val bla = currentActivity<BookListActivity>()
    bla?.endLoading(VideLibriBaseActivityOld.LOADING_COVER_IMAGE)
    if (bla === activity && book === this.book) this.updateImage()

}