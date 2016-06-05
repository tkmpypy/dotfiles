DOT_FILES=(.bashrc .bash_profile .vimrc)
for file in ${DOT_FILES[@]}
	 do
		 mv ~/$file ~/dotfiles
	 done
