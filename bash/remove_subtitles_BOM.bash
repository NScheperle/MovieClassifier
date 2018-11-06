#loop through files and sed away the BOM if it exists, do nothing else
for file in "./Subtitles_files/*.tt*"; do
	echo $file
	sed -i '1s/^\xEF\xBB\xBF//' $file
	echo " "
done