#!/bin/sh
if ! command -v stack &> /dev/null
    then
        echo "Installation failed. Please make sure that you have 'stack' installed"
    else
        echo "CaMPL was installed Successfully!";
fi