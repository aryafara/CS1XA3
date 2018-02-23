#!/bin/bash

git remote update
echo "Welcome to Arian's CompSci. 1XA3 Repo., once this script is finished feel free to read the README for more info.\n Number of commits on each branch thus far:\n 'git rev-list --all --count' "
if [$# -eq 0]
then echo "Type in the Help argument to get a list of valid functions!"
else
    upToDate() {#Tells the user whether they're up to date w/ remote, allows the user to pull if not
     if [git status -uno = ""]
     then echo "Local Tracked Directories/Files are up to date!"
     else 
          read -p "Local Tracked Directories/Files are not up to date, would you like to pull from remote?" : yesno
          if [yesno = "Y*" || yesno = "y*" ]
          then git pull
          fi
     fi
    }
    changes() {#Generates a file with all the tracked files who are either not in the remote or are changed from the remote's copy
        git diff > changes.log
        "This tracked changes log has been generated on 'date' \n" >> changes.log
    }
    toDo() {#Generates a file that lists all lines of files with the term #TODO
        grep -r -e "#TODO" > todo.log
        "This To-Do list has been generated on 'date'" >> todo.log
    }
    errorHaskell() {#Generates an error report for current haskell files
        "Haskell Errors in Repo. as of 'date'" > error.log
        find . -iname "*.hs" -exec ghc -fno-code {} \;  2>> error.log
    }
    Help() {#Lists out all possible arguments in this script
        echo " upToDate : Tells the user whether they're up to date w/ remote, allows the user to pull if not \n changes :Generates a file with all the tracked files who are either not in the remote or are changed from the remote's copy \n toDo : Generates a file that lists all lines of files with the term #TODO \n errorHaskell : Generates an error report for current haskell files \n all : runs all prior commands in tandem \n Help : this silly ole command"    
    }
    if   ["$1" = "upToDate"    ] then upToDate
    elif ["$1" = "changes"     ] then changes
    elif ["$1" = "to-Do"       ] then toDo
    elif ["$1" = "errorHaskell"] then errorHaskell
    elif ["$1" = "Help"        ] then Help
    elif ["$1" = "all"         ] then upToDate changes toDo errorHaskell
    else echo  "Type in the Help argument to get a list of valid functions!"
    fi
fi


