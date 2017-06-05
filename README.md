Make sure you have the latest copy of GS1Combinators in the parent directory.

To build the server, run:

stack build

To run it, run:

stack exec supplyChainServer-exe -- run 

Then you can check out the API at:

http://localhost:8000/swagger-ui/

To use ghci interactively, run 

stack ghci


