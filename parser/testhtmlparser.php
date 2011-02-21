<?
/*
  HtmlParser(
   $code,	html код или им€ файла, во втором случае надо в $isfile передавать 1
		the html code or file name. In second case you need set $isfile to 1

   $grammar,	ѕрекомпилена€ грамматика - указатель на массив
		precompiled grammar - the pointer to array

   $dataname,	Ќазвание данных, можно оставл€ть пустым
		name of the data, you can leave it empty

   $isfile=0	ќпредел€ет €вл€етс€ ли $code именем файла или строкой кода
		It is flag, which define state of $code, if it's 0 then $code is HTML code, if 1 then $code is filename
  )


  ѕример обхода дерева тегов можно посмотреть в common.inc
  в функции GetPageSrc
  example of the walking through tag tree, you can to see it in the
  common.inc in function GetPageSrc

  ƒерево тегов выгл€дит следующим образом:
  Tag tree consists of

  tagarray(
    "contentpos"=>value  число элементов в ветви-1, нумераци€ начинаетс€ с нул€
			number of elements in branch. Numeration begins from 0 as in C language
    "0"=>array(
           "type"=>"tag|text|comment",
           "data"=>"text"|array(
				"name"=>"tagname"
				"type"=>"open|close"
                          ),
  	   "pars"=>array( у тегов типа close и если type=text набор параметров отсутствует
			  if type=text or close then pars collection is absent

	   	        "parname"=>array(
		      		  "quot"=>"|\"|'" тип кавычек
						  quot type
			          "value"=>value  значение параметра тега
						  parameter value
			          "single"=>""	 если есть значит параметр без значени€ (например disable|enabled|checked и т.д.)
						  if it exists then parameter hav't value like disable|enabled|checked etc.
			          ),
 		   	 .....
   	           ),
           "xmlclose"=>"0|1",  тип закрыти€ XML Style или нет
				if it exists then tag has xml style close <tagname ... />
	   "content"=>tagarray(....)  если есть значит в теге есть вложени€ других тегов
				if it exists then tag includes the own branch of tags
   	 ),
    .....
  )

*/
include("common.inc");
include("htmlparser.inc");
$p=new HtmlParser("test.html",unserialize(Read_File("htmlgrammar.cmp")),"test.html",1);
$p->Parse();
$src="";
GetPageSrc(&$p->content,&$src);
print $src;
print "<br><h1>ƒерево тегов|Tag tree.</h1>";
PrintArray(&$p->content);
?>