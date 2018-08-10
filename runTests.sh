#! /usr/bin/env sh

export test_command="stack test --coverage --fast --ta '$@' -j1"

stack test --coverage --fast --ta "$@" -j1

# The line where the link is starts with these phrases
export report_link_header="An index of the generated HTML coverage reports is available at "

test_report=`stack test --coverage --fast --ta "$@" -j1 2>&1 | 
             egrep -i "$report_link_header" |
             sed "s/$report_link_header//g"`

echo "Your report lives in $test_report"

# Wouldn't work on macOS
# uname -a | egrep -i 'Linux' && sensible-browser $test_report
