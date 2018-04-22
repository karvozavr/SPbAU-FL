#!/usr/bin/env bash

which pip3 > /dev/null
if [ $? -ne 0 ]; then
    sudo apt-get install python3-pip
fi

which virtualenv > /dev/null
if [ $? -ne 0 ]; then
    sudo pip3 install virtualenv 
fi

# source venv/bin/activate
