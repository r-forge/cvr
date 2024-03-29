
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


<p> The <strong>project summary page</strong> you can find <a href="http://<?php echo $domain; ?>/projects/<?php echo $group_name; ?>/"><strong>here</strong></a>. </p>

<p>As an image is worth a thousand words, here is an example of a structures session:</p>
<img src="images/cvr1.png" width="300" /><br />
(grey is where you started the session, red is where you are now. Each number corresponds to a "session state")

<p>You can go forward and backwards in the session (here I went back 1 step, as if I hit the cancel button):</p>
<img src="images/cvr2.png" width="300" /><br />

<p>after a while things get complicated, but you'll still se a nice overview of your session history...</p>
<img src="images/cvrgraph4.png" width="300" /><br />

<p> <em>January 22: great improvement in the cvrgraph function!! Look at how it was (left) and how it is (right)!</em></p>
<img src="images/cvrgraph.png" width="300" /><br />



</body>
</html>
