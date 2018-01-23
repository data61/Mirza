Make sure you have the latest copy of GS1Combinators in the parent directory.

If you do not have the latest version of stack, please remove whatever version of stack you have:

`sudo apt remove haskell-stack `

Then get the latest version of stack by running:

`wget -qO- https://get.haskellstack.org/ | sh `

To build the server, run:

`stack build `

Before you run the server, make sure you have PostgreSQL 10 or higher installed.

To install, you can follow the instructions <a href="http://yallalabs.com/linux/how-to-install-and-use-postgresql-10-on-ubuntu-16-04/" target="_blank"> here. </a>
Note that you may need to run the commands as a super user.

Create a Postgres account.
There are good instructions <a href="https://www.digitalocean.com/community/tutorials/how-to-install-and-use-postgresql-on-ubuntu-16-04" target="_blank"> here. </a>

Type in `psql` and see that you can open up a psql shell.

After that, to create the database, run

`stack exec supplyChainServer-exe -- -i -c testsupplychainserver`

Note that you can change "testsupplychainserver" for any database name you like.
Not giving it the `-c` flag will result in a database named testsupplychainserver.

Finally, to run the server, do:

`stack exec supplyChainServer-exe`

Then you can check out the API at:

http://localhost:8000/swagger-ui/

To use ghci interactively, run 

`stack ghci `

If you find yourself deleting and recreating the database frequently, run
`./restart.sh`. It will build your code, drop the database 
`testsupplychainserver`, create it anew and run the migration script
to create all the relevant tables.

To to a clean build, do `./restart.sh --clean`.


Things to do before next release:
- [x] Integrate ReaderT Monad
- [ ] Add tests for all the endpoints
- [ ] Finish backend implementation (queries) for all endpoints
- [ ] Decide on a `PrimaryKeyType`
- [ ] Define instances for selected `PrimaryKeyType`
- [ ] Add remaining tables (`EventHash`, etc)
- [ ] (Unlikely) Write a `setup.sh` for developers coming in
