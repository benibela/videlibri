package de.benibela.videlibri

import android.graphics.Bitmap
import android.graphics.BitmapFactory
import android.os.Handler
import android.os.Looper
import android.util.DisplayMetrics
import android.util.Log
import de.benibela.videlibri.jni.Bridge
import java.net.URL
import java.util.concurrent.Executors
import java.util.concurrent.atomic.AtomicInteger
import kotlin.math.max
import kotlin.math.min


class CoverLoadingSize(val maxWidth: Int, val maxHeight: Int){
    val minWidth = maxWidth / 4
    val minHeight = maxHeight / 4

    constructor(details: BookDetails): this(details.coverTargetWidth,details.coverTargetHeight)
}

class CoverLoadingTask(val book: Bridge.Book, val size: CoverLoadingSize){
    val isbn = book.getProperty("isbn").trim()
    private val imageUrl = book.getProperty("image-url").split("[\r\n]").map { it.trim() }.filter { it.isNotEmpty() }
    val hasCoverUrl get() = isbn.isNotEmpty() || imageUrl.isNotEmpty()
    fun<T> forEachCoverUrl(load: (String) -> T?): T?{
        imageUrl.forEach { url ->
            load(url)?.let { return it }
        }

        if (isbn.isNotEmpty()) {
            val isbn10 = Bridge.VLNormalizeISBN(isbn, true, 10)
            load("http://images-eu.amazon.com/images/P/$isbn10.03.L.jpg")?.let { return it }
            load("http://covers.openlibrary.org/b/isbn/$isbn10-M.jpg?default=false")?.let { return it }

            val isbn13 = Bridge.VLNormalizeISBN(isbn, true, 13)
            load("https://www.buchhandel.de/cover/$isbn13/$isbn13-cover-m.jpg")?.let { return it }
        }
        return null
    }
}


class PriorityTaskExecutor<T>(val runnable: (T) -> Unit){
    private val executor = Executors.newSingleThreadExecutor()
    private var currentTask: T? = null
    private val activeThreads = AtomicInteger()
    var onTaskComplete: (() -> Unit)? = null
    private fun nextTask(){
        takeTask()?.let {
            activeThreads.incrementAndGet()
            runnable(it)
            activeThreads.decrementAndGet()
            onTaskComplete?.invoke()
        }
    }
    @Synchronized fun execute(task: T){
        currentTask = task
        executor.execute(::nextTask)
    }
    @Synchronized fun takeTask(): T? = currentTask.also {currentTask = null }
    @Synchronized fun isActive() = currentTask != null || activeThreads.get() > 0
}

class CoverLoader {
    companion object {
        private var executor: PriorityTaskExecutor<CoverLoadingTask>? = null
        private var handler: Handler? = null
        @JvmStatic fun loadBookCover(bookDetails: BookDetails, book: Bridge.Book) {
            if (executor == null) executor = PriorityTaskExecutor<CoverLoadingTask> { loadBookCover(it) }.also{
                it.onTaskComplete = { handler?.post { currentActivity<VideLibriBaseActivity>()?.refreshLoadingIcon() } }
            }
            if (handler == null) handler = Handler(Looper.getMainLooper())
            val task = CoverLoadingTask(book, CoverLoadingSize((bookDetails)))
            if (task.hasCoverUrl) {
                executor?.execute(task)
                currentActivity<VideLibriBaseActivity>()?.refreshLoadingIcon()
            }
        }
        fun isActive() = executor?.isActive() ?: false


        @JvmStatic private fun loadBookCover(task: CoverLoadingTask) {
            val bitmapOptions = BitmapFactory.Options().apply {
                inDensity = DisplayMetrics.DENSITY_DEFAULT
                inTargetDensity = BookDetails.displayMetrics.densityDpi
                inScreenDensity = BookDetails.displayMetrics.densityDpi
                inScaled = true
            }

            //load cover
            var bestCover: Bitmap? = null
            task.forEachCoverUrl { url ->
                try {
                    Log.i("URL", url)
                    val stream = URL(url).openStream()
                    val cover = BitmapFactory.decodeStream(stream, null, bitmapOptions) ?: return@forEachCoverUrl null
                    val oldCover = bestCover
                    if (oldCover == null) bestCover = cover
                    else if ( cover.width > oldCover.width && cover.height > oldCover.height ) {
                        oldCover.recycle()
                        bestCover = cover
                    } else cover.recycle()
                } catch (e: Throwable) {
                }
                bestCover?.takeIf { it.width >= task.size.minWidth && it.height >= task.size.minHeight }
            }

            //improve cover
            bestCover?.let { cover ->
                task.size.apply {
                    var scale = 1.0
                    if (cover.width < minWidth || cover.height < minHeight) {
                        scale = max(minWidth * 1.0 / cover.width, minHeight * 1.0 / cover.height)
                    }
                    if (cover.width * scale > maxWidth || cover.height * scale > maxHeight) {
                        scale = min(maxWidth * 1.0 / cover.width, maxHeight * 1.0 / cover.height)
                    }
                    Log.i("IMAGE SIZING", "${cover.width}x${cover.height}    ---  ${maxWidth}x${maxHeight}     ${minWidth}x${minHeight}  --- $scale")
                    if (scale != 1.0) {
                        bestCover = Bitmap.createScaledBitmap(cover, (cover.width * scale).toInt(), (cover.height * scale).toInt(), true)
                        cover.recycle()
                    }
                    Log.i("IMAGE SIZING", "Final size: ${bestCover?.width}x${bestCover?.height}  ${bestCover?.density}")
                }
            }

            //cache cover

            //show cover
            handler?.post {
                val bla = currentActivity<BookListActivity>()
                bestCover?.let { cover ->
                    task.book.image = cover
                    if (bla?.currentBook()  === task.book) bla.details.updateImage()
                }
            }
        }

    }


}

