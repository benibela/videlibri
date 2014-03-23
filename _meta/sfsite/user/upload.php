 <?php
    $i = 1;
    $link = mysql_connect('mysql-v', 'v359854rw', '9VbVsgfyrl57');
    if (!$link) {
        die('Verbindung zum Datenbankserver fehlgeschlagen');
    }    
    mysql_select_db("v359854_upload", $link);
    mysql_query("INSERT INTO id () values ()", $link);
    $i = mysql_insert_id($link);    
    mysql_close($link);
    

    
    mkdir("./".$i);
    mkdir("./".$i."/template");
    
    file_put_contents ( "./".$i."/meta.xml" , $_POST["meta"]);
    file_put_contents ( "./".$i."/bib.html" , $_POST["links"]);
    file_put_contents ( "./".$i."/template/template" , $_POST["template"]);
    
    print "<span>Hochgeladen zu: <code>http://videlibri.sf.net/user/".$i."/bib.html</code></span>";
?> 