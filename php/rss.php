<?
/* Paramètre de ligne de commande :
vide            : génère la liste des flux
rss=nom         : génère le flux RSS choisi
cmd=settings    : génère un flux sans items et format de date "d/m/yyyy hh:mm:ss" local
cmd=add&rss=nom : ajoute un item (méthode POST) au flux rss
cmd=upd&rss=nom : mise à jour (méthode POST) du flux rss
*/

/* La table 'channels' aura au moins 6 champs :
id            : ID du channel
title         : nom du flux RSS
link          : url du site
description   : description du flux
lastBuildDate : date de mise à jour
ttl           : nombre de minute pour mise en cache
*/

/* La table 'feed_items' aura au moins 6 champs :
id            : ID du channel
guid          : numéro de la ligne
title         : titre
description   : description
author        : email de l'auteur
pubDate       : date de publication
*/

/* config.php définit les variables
$dbhost
$dblogin
$dbpassword
$dbbase
*/
require_once("config.php");

$dbprefix = "cvfc_";
$tbchannels = $dbprefix."feed_channels";
$tbitems = $dbprefix."feed_items";
// Connexion
$sql_con = mysql_connect($dbhost,$dblogin,$dbpassword);
mysql_select_db($dbbase);

if ($_GET[cmd] == "add") {
  // on a un post d'ajout
   $sql = "INSERT INTO $tbitems VALUES
   ('$_POST[id]','$_POST[guid]','$_POST[title]','$_POST[description]','$_POST[author]','$_POST[pubDate]')";
   if (mysql_query($sql))
     echo "OK";
   else
     echo mysql_error($sql_con);
 }

elseif ($_GET[cmd] == "upd") {
  // on a un post de MàJ
   $sql = "UPDATE $tbchannels SET lastBuildDate='$_POST[lastBuildDate]' WHERE id='$_GET[rss]'";
   if (mysql_query($sql))
     echo "OK";
   else
     echo mysql_error($sql_con);
 }

elseif ($_GET[cmd] == "settings") {
  Header("content-type: application/xml");
  echo "<"."?xml version=\"1.0\" encoding=\"ISO-8859-1\"?".">\n";
  echo "<settings>\n";
  // Sélection des fiches
  $RC = mysql_query("SELECT * FROM $tbchannels");
  while($val=mysql_fetch_array($RC)) {
    echo "   <channel>\n";
    echo "      <id>".$val["id"]."</id>\n";
    echo "      <title>".$val["title"]."</title>\n";
    echo "      <link>".$val["link"]."</link>\n";
    echo "      <description>".$val["description"]."</description>\n";
    echo "      <lastBuildDate>".date('d/m/Y H:i:s',StrToTime($val["lastBuildDate"]))."</lastBuildDate>\n";
    echo "      <ttl>".$val["ttl"]."</ttl>\n";
    echo "   </channel>\n";
  } // while
  echo "</settings>\n";
 } // if

elseif (($_GET[cmd] == "") and ($_GET[rss] != "")) {
  Header("content-type: application/xml");
  echo "<"."?xml version=\"1.0\" encoding=\"ISO-8859-1\"?".">\n";
  echo "<rss version=\"2.0\">\n";

  // Sélection des fiches
  $RC = mysql_query("SELECT * FROM $tbchannels WHERE id='".$_GET[rss]."'");

  // Listing
  while($val=mysql_fetch_array($RC)) {
    echo "   <channel>\n";
    echo "      <title>".$val["title"]."</title>\n";
    echo "      <link>".$val["link"]."</link>\n";
    echo "      <description>".$val["description"]."</description>\n";
    echo "      <lastBuildDate>".gmdate('D, d M Y H:i:s',StrToTime($val["lastBuildDate"]))." GMT</lastBuildDate>\n";
    echo "      <ttl>".$val["ttl"]."</ttl>\n";
    $RITEMS = mysql_query("SELECT * FROM $tbitems WHERE id='$_GET[rss]' ORDER BY cvfc_feed_items.pubDate DESC LIMIT 0 , 30");

    while($item=mysql_fetch_array($RITEMS)) {
      echo "       <item>\n";
      echo "         <guid isPermaLink=\"false\">http://cvfc.univ-fcomte.fr/cv/www/bureau/cremesform.asp?num=".$item["guid"]."</guid>\n";
      echo "         <link>http://cvfc.univ-fcomte.fr/cv/www/bureau/litfil.asp?ligne=".$item["guid"]."</link>\n";
      echo "         <title>".$item["id"]." : ".$item["title"]."</title>\n";
      echo "         <description>".$item["description"]."</description>\n";
      echo "         <author>".$item["author"]."</author>\n";
      echo "         </item>\n";
    }
    echo "   </channel>\n";
  }
  echo "</rss>\n";
}

elseif (($_GET[cmd] == "") and ($_GET[rss] == "")) {
//  Header("content-type: application/xml");
//  echo "<"."?xml version=\"1.0\" encoding=\"ISO-8859-1\"?".">\n";
//  echo "<settings>\n";
  // Sélection des fiches
  $RC = mysql_query("SELECT * FROM $tbchannels");
  while($val=mysql_fetch_array($RC)) {
    $URL = "http://" . $_SERVER["SERVER_NAME"].$_SERVER["REQUEST_URI"]."?rss=".$val["id"];
    echo $val["title"]."<BR>\n";
    echo $val["description"]."<BR>\n";
    echo "<a href=\"".$URL."\">Flux RSS ".$val["title"]."</a><BR><BR>";
  } // while
 } // if


// Déconnexion
 mysql_close();

?>