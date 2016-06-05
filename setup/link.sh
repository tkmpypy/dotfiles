
for file in `\find $HOME/dotfiles -name '.*'`;
	do
		file_name=`basename $file`
		ln -s $file $HOME/$file_name
	done

