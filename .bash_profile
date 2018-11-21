export GOROOT=/usr/local/Cellar/go/1.6/libexec
export GOPATH=$HOME/Go
export XDG_CONFIG_HOME=$HOME
export JAVA_HOME=$(/usr/libexec/java_home)
export ANT_HOME="/usr/bin/ant"
export PATH=$GOROOT/bin:$GOPATH/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:$HOME/.nodebrew/current/bin:$PATH:$JAVA_HOME:$ANT_HOME
export PYENV_ROOT="${HOME}/.pyenv"
if [ -d "${PYENV_ROOT}" ]; then
    export PATH=${PYENV_ROOT}/bin:$PATH
    eval "$(pyenv init -)"
fi


#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="/Users/takuma/.sdkman"
 [[ -s "/Users/takuma/.sdkman/bin/sdkman-init.sh" ]] && source "/Users/takuma/.sdkman/bin/sdkman-init.sh"
export ANDROID_HOME=/usr/local/opt/android-sdk
export JAVA_HOME=/Library/Java/Home
