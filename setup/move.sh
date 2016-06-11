DOT_FILES=(.bashrc .bash_profile .vimrc .spacemacs)
for file in ${DOT_FILES[@]}
	 do
		 mv ~/$file ~/dotfiles
	 done
