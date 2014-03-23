 <?php
    $fp = fopen("user.id", "r+");
    if (flock($fp, LOCK_EX)) {
        //print "Got lock!\n";
        $i = 0;
        fscanf($fp, "%d", $i);
        
        $i = $i + 1;
        fseek($fp, 0);

        fprintf($fp, "%d", $i);
        
        flock($fp, LOCK_UN);
        fclose($fp);
        
        mkdir("./".$i);
        mkdir("./".$i."/template");
        
        file_put_contents ( "./".$i."/meta.xml" , $_POST["meta"]);
        file_put_contents ( "./".$i."/bib.html" , $_POST["links"]);
        file_put_contents ( "./".$i."/template/template" , $_POST["template"]);
        
        print "<span>Hochgeladen zu: <code>http://videlibri.sf.net/user/".$i."/bib.html</code></span>";
    } else {
       print "Automatisches Hochladen fehlgeschlagen! "; 
    }
?> 