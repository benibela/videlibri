package de.benibela.videlibri

import android.app.ActivityManager
import android.content.Context
import android.graphics.Bitmap
import android.graphics.BitmapFactory
import android.util.Log
import android.util.LruCache
import de.benibela.videlibri.activities.BookListActivity
import de.benibela.videlibri.activities.VideLibriBaseActivity
import de.benibela.videlibri.components.BookDetails
import de.benibela.videlibri.jni.Bridge
import de.benibela.videlibri.utils.currentActivity
import de.benibela.videlibri.utils.runOnUiThread
import de.benibela.videlibri.utils.takeNonEmpty
import java.io.File
import java.io.FileOutputStream
import java.io.IOException
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
    private val imageUrl = book.getProperty("image-url").split("\r", "\n").map { it.trim() }.filter { it.isNotEmpty() }
    val id = (isbn.takeNonEmpty() ?: imageUrl.getOrElse(0) {""}).replace(replaceRegex, "")

    val cacheInMemory get() = true
    val cacheOnDisk get() = book.account != null && isbn.isNotEmpty()

    val hasCoverUrl get() = isbn.isNotEmpty() || imageUrl.isNotEmpty()

    fun<T> forEachCoverUrl(load: (String) -> T?): T?{
        imageUrl.toSet().forEach { url ->
            load(url)?.let { return it }
        }

        if (isbn.isNotEmpty()) {
            val isbn10 = Bridge.VLNormalizeISBN(isbn, true, 10)
            load("http://images-eu.amazon.com/images/P/$isbn10.03.${if (size.maxHeight > 150) "L" else "M"}.jpg")?.let { return it }
            load("http://covers.openlibrary.org/b/isbn/$isbn10-${if (size.maxWidth > 180) "L" else "M"}.jpg?default=false")?.let { return it }

            val isbn13 = Bridge.VLNormalizeISBN(isbn, true, 13)
            load("https://www.buchhandel.de/cover/$isbn13/$isbn13-cover-${if (size.maxWidth > 200) "l" else "m"}.jpg")?.let { return it }
        }
        return null
    }
    companion object {
        val replaceRegex = Regex("[^-a-zA-Z0-9]+")
    }
}


class PriorityTaskExecutor<T>(val runnable: (T) -> Unit){
    private val executor = Executors.newSingleThreadExecutor()
    private var currentTask: T? = null
    private val activeThreads = AtomicInteger()
    var onTaskComplete: (() -> Unit)? = null
    private fun nextTask(){
        takeTask()?.let { task ->
            activeThreads.incrementAndGet()
            runnable(task)
            activeThreads.decrementAndGet()
            onTaskComplete?.let { runOnUiThread(it) }
        }
    }
    @Synchronized fun execute(task: T){
        currentTask = task
        executor.execute(::nextTask)
    }
    @Synchronized fun takeTask(): T? = currentTask.also {currentTask = null }
    @Synchronized fun isActive() = currentTask != null || activeThreads.get() > 0
}

interface BitmapCache{
    operator fun get(id: String): Bitmap?
    operator fun set(id: String, bmp: Bitmap)
    operator fun get(task: CoverLoadingTask): Bitmap? = get(task.id)
    operator fun set(task: CoverLoadingTask, bmp: Bitmap) =
        task.id.takeNonEmpty()?.let { set(it, bmp) }
}

class MemoryBitmapCache(maxSize: Int): LruCache<String, Bitmap>(maxSize), BitmapCache{
    override fun set(id: String, bmp: Bitmap) {
        super.put(id, bmp)
    }

    override fun sizeOf(key: String?, value: Bitmap?): Int {
        return value?.run { width * height * 4 } ?: 0
    }

}

class DiskBitmapCache(cacheDir: File): BitmapCache{
    private val coverCacheDir = File(cacheDir, "covers/large/").also { it.mkdirs() }

    private fun cacheFile(id: String) = File(coverCacheDir, "${id}.jpg")

    override fun get(id: String): Bitmap? {
        try {
            val f = cacheFile(id)
            if (f.exists())
                return BitmapFactory.decodeFile(f.absolutePath)
        } catch (e: IOException) {
            e.printStackTrace()
        }
        return null
    }

    override fun set(id: String, bmp: Bitmap) {
        try {
            FileOutputStream(cacheFile(id)).use { out ->
                bmp.compress(Bitmap.CompressFormat.JPEG, 100, out)
            }
        } catch (e: IOException) {
            e.printStackTrace()
        }
    }

}

class DeadBitmapCache: BitmapCache{
    override fun get(id: String): Bitmap? = null
    override fun set(id: String, bmp: Bitmap) { }
}


object CoverLoader {
    private val executor = PriorityTaskExecutor<CoverLoadingTask> {
        loadBookCover(it)
    }.also{
        it.onTaskComplete = { currentActivity<VideLibriBaseActivity>()?.refreshLoadingIcon() }
    }
    private val memoryCache: BitmapCache
    private val diskCache: BitmapCache

    init {
        val context = VideLibriApp.currentContext()
        var availMemory = 8*1024*1024
        if (context != null) {
            diskCache = DiskBitmapCache(context.cacheDir)
            val am = context.getSystemService(Context.ACTIVITY_SERVICE) as? ActivityManager
            am?.let {
                val memInfo = ActivityManager.MemoryInfo()
                am.getMemoryInfo(memInfo)
                availMemory = (memInfo.threshold / 10).coerceAtMost(Int.MAX_VALUE.toLong()).toInt()
            }
        } else diskCache = DeadBitmapCache()
        memoryCache = MemoryBitmapCache(availMemory)
    }

    @JvmStatic fun loadBookCover(bookDetails: BookDetails, book: Bridge.Book) {
        val task = CoverLoadingTask(book, CoverLoadingSize((bookDetails)))
        if (task.hasCoverUrl) {
            executor.execute(task)
            currentActivity<VideLibriBaseActivity>()?.refreshLoadingIcon()
        }
    }
    fun isActive() = executor.isActive()


    @JvmStatic private fun loadBookCover(task: CoverLoadingTask) {
        if (task.book.image != null) return

        //load cover
        var bestCover: Bitmap? = memoryCache[task] ?: diskCache[task]
        if (bestCover == null) {
            val bitmapOptions = BitmapFactory.Options().apply {
                /*inDensity = DisplayMetrics.DENSITY_DEFAULT
                inTargetDensity = BookDetails.displayMetrics.densityDpi
                inScreenDensity = BookDetails.displayMetrics.densityDpi*/
                inScaled = false
            }
            task.forEachCoverUrl { url ->
                try {
                    Log.i("URL", url)
                    val stream = URL(url).openStream()
                    val cover = stream.use {  BitmapFactory.decodeStream(it, null, bitmapOptions) }
                            ?: return@forEachCoverUrl null
                    val oldCover = bestCover
                    if (oldCover == null) bestCover = cover
                    else if (cover.width > oldCover.width && cover.height > oldCover.height) {
                        oldCover.recycle()
                        bestCover = cover
                    } else cover.recycle()
                } catch (e: Throwable) {
                }
                bestCover?.takeIf { it.width >= task.size.minWidth && it.height >= task.size.minHeight }
            }
        }


        bestCover?.let { cover ->
            if (cover.width < 5 || cover.height < 5)
                return //some apis return single pixel images on failure

            //cache cover
            if (task.cacheOnDisk) diskCache[task] = cover

            //improve cover
            task.size.apply {
                var scale = 1.0
                if (cover.width < minWidth || cover.height < minHeight) {
                    scale = max(minWidth * 1.0 / cover.width, minHeight * 1.0 / cover.height)
                }
                if (cover.width * scale > maxWidth || cover.height * scale > maxHeight) {
                    scale = min(maxWidth * 1.0 / cover.width, maxHeight * 1.0 / cover.height)
                }
                Log.i("IMAGE SIZING", "${cover.width}x${cover.height}    ---  ${maxWidth}x${maxHeight}     ${minWidth}x${minHeight}  --- $scale")
                if (scale != 1.0) try {
                    bestCover = Bitmap.createScaledBitmap(cover, (cover.width * scale).toInt(), (cover.height * scale).toInt(), true)
                    cover.recycle()
                } catch (e: Throwable) {
                    return@loadBookCover
                }
                Log.i("IMAGE SIZING", "Final size: ${bestCover?.width}x${bestCover?.height}  ${bestCover?.density}")
                bestCover?.density = Bitmap.DENSITY_NONE
            }
        }

        if (task.cacheInMemory) bestCover?.let {  memoryCache[task] = it }

        //show cover
        runOnUiThread {
            val bla = currentActivity<BookListActivity>()
            bestCover?.let { cover ->
                task.book.image = cover
                if (bla?.currentBook() === task.book) bla.details.updateImage()
            }
        }
    }


}

