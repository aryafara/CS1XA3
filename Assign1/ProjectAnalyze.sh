#!/bin/bash

git remote update
echo -e "Welcome to Arian's CompSci. 1XA3 Repo., once this script is finished feel free to \nread the README for more info.\nNumber of commits on each branch thus far:  " $(git rev-list --all --count)
if [ $# -eq 0 ]
then echo -e "Type in the Help argument to get a list of valid functions!"
else
    upToDate() { #Tells the user whether they're up to date w/ remote, allows the user to pull if not
        loc=$(git rev-parse master)
        rem=$(git rev-parse origin/master)
        if [ $loc = $rem ]
        then echo -e "Local Tracked Directories/Files are up to date!"
        else 
             read -p "Local Tracked Directories/Files are not up to date, would you like to pull from remote?" yesno
            if [ yesno = "Y*" || yesno = "y*" ]
            then git pull
            fi
        fi
    }
    changes() { #Generates a file with all the tracked files who are either not in the remote or are changed from the remote's copy
        git diff > changes.log
        echo "This tracked changes log has been generated on "$(date)"\n" >> changes.log
        echo "Done changes()!"
    }
    toDo() { #Generates a file that lists all lines of files with the term #TODO
        grep -r -e "#TODO" > todo.log
        echo "This To-Do list has been generated on "$(date)"\n" >> todo.log
        echo "Done toDo()!"
    }
    errorHaskell() { #Generates an error report for current haskell files
        echo "Haskell Errors in Repo. as of "$(date) > error.log
        find . -iname "*.hs" -exec ghc -fno-code {} \;  2>> error.log
        echo "Done errorHaskell()!"
    }
    findAndDate() { #Lets User search for a file and prints the date of last modified of the file
        read -p "Enter the name of the file you're looking for: " uFile
        if [$(find . -iname $uFile -exec date -r {} \;) = ""]
        then echo "The file you are looking for doesn't exist"
        else
            echo $uFile " was last modified on " $(find . -iname $uFile -exec date -r {} \;)  
        fi
    }
    findAndDo() { #Lets User search for a item and do a bash command of their choice
        read -p "Enter your search term: " uSearch
        read -p "Enter your command " uCommand
        if [$(find . -iname $uSearch -exec $uCommand {} \;) = ""]
        then echo "Your given search term could not be found"
        else
            echo "Success! Here are your results:"
            echo $(find . -iname $uSearch -exec $uCommand {} \;)
        fi
    }
    Help() { #Lists out all possible arguments in this script
        echo -e " upToDate : Tells the user whether they're up to date w/ remote, allows the user to pull if not \n changes : Generates a file with all the tracked files who are either not in the remote or are changed from the remote's copy \n toDo : Generates a file that lists all lines of files with the term #TODO \n errorHaskell : Generates an error report for current haskell files \n all : runs all prior commands in tandem \n Help : this silly ole command \n findAndDate : Lets User search for a file and prints the date of last modified of the file \n findAndDo : Lets User search for a item and do a bash command of their choice"    
    }
    if   [ "$1" = "upToDate"     ] 
        then upToDate
    elif [ "$1" = "changes"      ] 
        then changes
    elif [ "$1" = "to-Do"        ] 
        then toDo
    elif [ "$1" = "errorHaskell" ] 
        then errorHaskell
    elif [ "$1" = "findAndDate"  ]
        then findAndDate
    elif [ "$1" = "findAndDo"    ]
        then findAndDo
    elif [ "$1" = "Help"         ] 
        then Help
    elif [ "$1" = "all"          ] 
        then upToDate changes toDo errorHaskell findAndDate findAndDo
    else echo -e "Type in the Help argument to get a list of valid functions!"
    fi
fi


