#!/usr/bin/env sh

stack test --fast --coverage --ta "$@" -j1

# Change this line if you want to auto-launch the report in your browser
export enable_report_launch=false

if [ "$enable_report_launch" = true ]
then
  # The line where the link is starts with these phrases
  export report_link_header="An index of the generated HTML coverage reports is available at "

  test_report=`stack test --fast --coverage --ta "$@" -j1 2>&1 |
              egrep -i "$report_link_header" |
              sed "s/$report_link_header//g"`

  echo "Your report lives in $test_report"

  case "$OSTYPE" in
    darwin*)  open $test_report ;;
    linux*)   sensible-browser $test_report ;;
    *)        echo "Please open $test_report in your favorite browser" ;;
  esac

fi
