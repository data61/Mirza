#!/usr/bin/env python3
"""
Usage: ./license_check.py # reads from stdin
Expects some lines in the format:<dependency-name> <license-name>
which is the format used by `stack` (haskell-stack) in the output of
`stack ls dependencies --license`
The list variable 'accepted_licenses' should be modified if your project
complies with a different level of permissiveness.
The program will exit with EXIT_FAILURE if one or more licenses are not
in the accepted_licenses list, and therefore can be used in a CI pipeline.
For example,
stack ls dependencies --license | ./license_check.py
"""


import sys

# Add to it ad hoc
accepted_licenses = [
    'Apache-2.0',
    'BSD2',
    'BSD3',
    'ISC',
    'LGPL', # binary copy of the server cannot be distributed
    'MIT',
    'OtherLicense',
    'PublicDomain',
]

# Handles special cases, like BSD-3 and BSD3
def normalise(dep, lcns):
    # BSD-3 -> BSD3
    if lcns == 'BSD-3':
        lcns = 'BSD3'
    # Add more special cases ad hoc
    return (dep, lcns)

def main():
    exit_status = 0
    for line in sys.stdin:
        (dep, lcns) = line.split()
        (dep, lcns) = normalise(dep, lcns)
        if lcns not in accepted_licenses:
            print('WARNING: License %s of dependency %s may not be permissive of our uses' % (lcns, dep))
            exit_status = 1

    exit(exit_status) # exiting with an appropriate status code for the CI

if __name__ == '__main__':
    main()
