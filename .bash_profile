export GOROOT=/usr/local/Cellar/go/1.6/libexec
export GOPATH=$HOME/Go
export XDG_CONFIG_HOME=$HOME
export PATH=$GOROOT/bin:$GOPATH/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:$HOME/.nodebrew/current/bin:$PATH
export PYENV_ROOT="${HOME}/.pyenv"
if [ -d "${PYENV_ROOT}" ]; then
    export PATH=${PYENV_ROOT}/bin:$PATH
    eval "$(pyenv init -)"
fi

