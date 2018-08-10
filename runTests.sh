#! /usr/bin/env sh

stack test --coverage --fast --ta "$@" -j1

# Keep the commented out code. It's helpful if we want to make an option
# to open up the test report after running the tests 

# The line where the link is starts with these phrases
# export report_link_header="An index of the generated HTML coverage reports is available at "

# test_report=`stack test --coverage --fast --ta "$@" -j1 2>&1 | 
#              egrep -i "$report_link_header" |
#              sed "s/$report_link_header//g"`

# echo "Your report lives in $test_report"

# Wouldn't work on macOS
# uname -a | egrep -i 'Linux' && sensible-browser $test_report
