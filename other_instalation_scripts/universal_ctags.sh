# Installing universal-ctags
if $(brew list universal-ctags > /dev/null 2>&1)
then
    echo -e "\e[32;1muniversal-ctags \e[37;1malready installed \e[0m"
else
    echo -e "\e[33;1mbrew \e[37;1minstall \e[32;1muniversal-ctags \e[0m"
    brew install --HEAD universal-ctags/universal-ctags/universal-ctags
fi
