Make sure you have the latest copy of GS1Combinators in the parent directory.

If you do not have the latest version of stack, please remove whatever version of stack you have:

sudo apt remove haskell-stack

Then get the latest version of stack by running:

wget -qO- https://get.haskellstack.org/ | sh

To build the server, run:

stack build

To run it, run:

stack exec supplyChainServer-exe -- run 

Then you can check out the API at:

http://localhost:8000/swagger-ui/

To use ghci interactively, run 

stack ghci


