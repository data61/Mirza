Keys for tests can an be generated using the `GenerateKeys.hs` file, which can
be run directly from the command line:

    ./GenerateKeys.hs


Edit the file to add more keys for tests. It can only be used to generate "valid"
JWKs, though they may be keys which are smaller than allowed by the spec -
generating other invalid JWKs need to be done with other tools, such as
https://mkjwk.org, or creating them by hand.