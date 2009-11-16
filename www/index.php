
<!-- This is the project specific website template -->
<!-- It can be changed as liked or replaced by other content -->

<?php

$domain=ereg_replace('[^\.]*\.(.*)$','\1',$_SERVER['HTTP_HOST']);
$group_name=ereg_replace('([^\.]*)\..*$','\1',$_SERVER['HTTP_HOST']);
$themeroot='http://r-forge.r-project.org/themes/rforge/';

echo '<?xml version="1.0" encoding="UTF-8"?>';
?>
<!DOCTYPE html
	PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
	"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en   ">

  <head>
	<meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
	<title><?php echo $group_name; ?></title>
	<link href="<?php echo $themeroot; ?>styles/estilo1.css" rel="stylesheet" type="text/css" />
  </head>

<body>

<!-- R-Forge Logo -->
<table border="0" width="100%" cellspacing="0" cellpadding="0">
<tr><td>
<a href="/"><img src="<?php echo $themeroot; ?>/images/logo.png" border="0" alt="R-Forge Logo" /> </a> </td> </tr>
</table>


<!-- get project title  -->
<!-- own website starts here, the following may be changed as you like -->

<?php if ($handle=fopen('http://'.$domain.'/export/projtitl.php?group_name='.$group_name,'r')){
$contents = '';
while (!feof($handle)) {
	$contents .= fread($handle, 8192);
}
fclose($handle);
echo $contents; } ?>

<!-- end of project description -->

<p> This is cvr. <br />
It is (will be) a current versioning system for R workspaces. It logs any command which modifies the R workspace (modifications are detected by the md5 hash of the R workspace). <br />
Users can then read the history of the transformations done, plot a flowchart, or rewind to any step in time. Each command can also be tagged by a comment.<br />
<p>Here is an example: </p>
<p>...following the fist few operations...</p>

<p>library(cvr)<br />
cvrdo('a &lt;- 1', 'first change')<br />
cvrdo('b &lt;- 2', 'second change')<br />
cvrdo('a &lt;- 1', 'this will not be logged')<br />
cvrdo('d &lt;- a + b', 'simple operation')<br />
cvrgraph()<br /></p>
<p align="left"><a href="images/cvr1.png"><img src="images/cvr1.png" width="297" height="301" /></a></p>
<p>...rewinding...</p>

<p>cvrrewind(7536)<br />
cvrgraph()<br /></p>

<p align="left"><a href="images/cvr2.png"><img src="images/cvr2.png" width="297" height="301" /></a></p>

<p>...branching...</p>

<p>cvrdo('e &lt;- max(a,b)', 'simple operation')<br />
cvrgraph()<br /></p>

<p align="left"><a href="images/cvr3.png"><img src="images/cvr3.png" width="297" height="301" /></a></p>

<p>(clearly, some work has yet to be done for correct positioning... :-)</p>

</p>

<p> The <strong>project summary page</strong> you can find <a href="http://<?php echo $domain; ?>/projects/<?php echo $group_name; ?>/"><strong>here</strong></a>. </p>

</body>
</html>
