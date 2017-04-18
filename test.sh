#/usr/bin/bash

# Set temporary alias
alias ls=ls

for i in $(ls sbir-files/done/*); do
    echo "For file $i:"
    cat $i
    echo
    sbi.scm $i
done

# Restore alias
# But we don't need to do this because we're called bash on this script
# source ~/.bashrc
