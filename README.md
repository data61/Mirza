Make sure you have the latest copy of GS1Combinators in the parent directory.

If you do not have the latest version of stack, please remove whatever version of stack you have:

<code>sudo apt remove haskell-stack </code>

Then get the latest version of stack by running:

<code>wget -qO- https://get.haskellstack.org/ | sh </code>

To build the server, run:

<code>stack build </code>

Before you run the server, make sure you have PostgreSQL 10 or higher installed.

To install, you can follow the instructions <a href="http://yallalabs.com/linux/how-to-install-and-use-postgresql-10-on-ubuntu-16-04/" target="_blank"> here. </a>
Note that you may need to run the commands as a super user.

Create a Postgres account.
There are good instructions <a href="https://www.digitalocean.com/community/tutorials/how-to-install-and-use-postgresql-on-ubuntu-16-04" target="_blank"> here. </a>

Type in <code>psql</code> and see that you can open up a psql shell.

After that, to create the database, run

<code>chmod +x manage_db.sh</code><br>
<code>./manage_db.sh testsupplychainserver</code>

Note that you can change "testsupplychainserver" for any database name you like.

Finally, to run the server, do:

<code>stack exec supplyChainServer-exe --run --migrate </code>

Then you can check out the API at:

http://localhost:8000/swagger-ui/

To use ghci interactively, run 

<code>stack ghci </code>


