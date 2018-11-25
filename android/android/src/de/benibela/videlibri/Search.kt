package de.benibela.videlibri

import android.content.Intent
import android.graphics.Paint
import android.os.Bundle
import android.preference.PreferenceManager
import android.view.View
import android.widget.Spinner
import android.widget.TextView
import de.benibela.videlibri.jni.Bridge

class Search: SearchOld(){
    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setVideLibriView(R.layout.searchlayout)

        libId = savedInstanceState?.getString("libId") ?: intent?.getStringExtra("libId") ?: ""
        libName = savedInstanceState?.getString("libName") ?: intent?.getStringExtra("libName") ?: ""


        if (libName.isNullOrEmpty() && libId != "")
            libName = Bridge.VLGetLibraryDetails(libId)?.prettyName ?: ""

        val lib = findViewById<TextView>(R.id.library)
        lib.setText(libName + " (" + tr(R.string.change) + ")")
        lib.paintFlags = lib.paintFlags or Paint.UNDERLINE_TEXT_FLAG
        if (libId == null || libId == "" || getIntent().getBooleanExtra("showLibList", false) && savedInstanceState == null) {
            if (System.currentTimeMillis() - LibraryListOld.lastSelectedTime < LibraryListOld.SELECTION_REUSE_TIME) {
                libId = LibraryListOld.lastSelectedLibId ?: ""
                libName = LibraryListOld.lastSelectedLibName ?: ""
            } else {
                libId = ""
                changeSearchLib()
            }
        }

        val defaultQuery = intent.getSerializableExtra("query");
        if (defaultQuery is Map<*, *>) {
            findViewById<TextView>(R.id.title).text = defaultQuery.get("title").toString()
            findViewById<TextView>(R.id.author).text = defaultQuery.get("author").toString()
        }

        findViewById<View>(R.id.library).setOnClickListener( { changeSearchLib() })

        findViewById<View>(R.id.button).setOnClickListener(View.OnClickListener {
            obtainSearcher()

            if ("debug" == getTextViewText(R.id.year)) {
                activateHiddenDebugMode()
                return@OnClickListener
            }

            val intent = Intent(this@Search, SearchResult::class.java)
            val book = Bridge.Book()
            book.title = getTextViewText(R.id.title)
            book.author = getTextViewText(R.id.author)
            book.setProperty("keywords", getTextViewText(R.id.keywords))
            book.setProperty("year", getTextViewText(R.id.year))
            book.setProperty("isbn", getTextViewText(R.id.isbn))
            intent.putExtra("searchQuery", book)

            for (i in BRANCH_NAMES.indices) {
                if (findViewById<View>(BRANCH_LAYOUT_IDS[i]).getVisibility() != View.GONE) {
                    val spinner = findViewById<View>(BRANCH_IDS[i]) as Spinner
                    intent.putExtra(BRANCH_NAMES[i], spinner.selectedItemPosition)
                    val branch = spinner.selectedItem as String
                    val sp = PreferenceManager.getDefaultSharedPreferences(this@Search)
                    val editor = sp.edit()
                    editor.putString("Search|" + libId + "|" + BRANCH_NAMES[i], branch)
                    editor.apply()
                }
            }
            startActivity(intent)
        })

        setTitle(tr(R.string.search_title))

        if (libId != null && libId != "") {
            obtainSearcher()
            setBranchViewes(true)
        }
    }


}