#!/usr/bin/env bash

# Usage: ./coverage.sh [--launch] [rest of args]

export n=0
export enable_report_launch=false
unset args

# Gets rid of the `--launch` flag
for i in "$@"
do
  if [ "$i" = "--launch" ]
  then
    enable_report_launch=true
  else
    # Storing everything other than --launch in `args`
    args[$((n++))]="$i"
  fi
  shift
done

if [ "$enable_report_launch" = true ]
then
  # The line where the link is starts with this phrase
  export report_link_header="An index of the generated HTML coverage reports is available at "

  echo "Report is being generated. Please wait..."
  test_report=`./run_tests.sh --coverage "${args[@]}" 2>&1 |
              egrep -i "$report_link_header" |
              sed "s/$report_link_header//g"`
  echo "Report generation complete."

  echo "Your report lives in $test_report"
  echo "$OSTYPE"
  case "$OSTYPE" in
    "darwin"*)  open $test_report ;;
    "linux"*)   sensible-browser $test_report ;;
    *)           echo "Please open $test_report in your favorite browser" ;;
  esac
else
  ./run_tests.sh --coverage "${args[@]}"
fi
