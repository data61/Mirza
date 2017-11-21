Make sure you have the latest copy of GS1Combinators in the parent directory.

If you do not have the latest version of stack, please remove whatever version of stack you have:

<code>sudo apt remove haskell-stack </code>

Then get the latest version of stack by running:

<code>wget -qO- https://get.haskellstack.org/ | sh </code>

To build the server, run:

<code>stack build </code>

To run it, run:

<code>stack exec supplyChainServer-exe -- run </code>

Then you can check out the API at:

http://localhost:8000/swagger-ui/

To use ghci interactively, run 

<code>stack ghci </code>


