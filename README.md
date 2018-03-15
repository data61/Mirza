Make sure you have the latest copy of GS1Combinators in the parent directory.

If you do not have the latest version of stack, please remove whatever version of stack you have:

`sudo apt remove haskell-stack `

Then get the latest version of stack by running:

`wget -qO- https://get.haskellstack.org/ | sh `

To build the server, run:

`stack build `

Before you run the server, make sure you have PostgreSQL 10 or higher installed.

On MacOS, you need to `brew install openssl` and then run:
`stack build --extra-include-dirs=/usr/local/opt/openssl/include --extra-lib-dirs=/usr/local/opt/openssl/lib`

This is because Apple has deprecated the use of OpenSSL in favour of its own SSL and TLS libs.

To install, you can follow the instructions [here]( http://yallalabs.com/linux/how-to-install-and-use-postgresql-10-on-ubuntu-16-04/)
Note that you may need to run the commands as a super user.

Create a Postgres account.
There are good instructions <a href="https://www.digitalocean.com/community/tutorials/how-to-install-and-use-postgresql-on-ubuntu-16-04" target="_blank"> here. </a>

Type in `psql` and see that you can open up a psql shell.

After that, to create the database, run

`stack exec supplyChainServer-exe -- -i -c devsupplychainserver`

Note that you can change "testsupplychainserver" for any database name you like.
Not giving it the `-c` flag will result in a database named
`devsupplychainserver`.

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

To do a clean build instead, do `./restart.sh --clean`.

### Useful Tools
- [GS1 Company Database](https://www.gs1us.org/tools/gs1-company-database-gepir) - You can search for GLN (Global Location Number) of GS1 Companies here

Things to do before next release:
- [x] Integrate ReaderT Monad
- [ ] Add tests for all the endpoints
- [ ] Finish backend implementation (queries) for all endpoints
- [x] Decide on a `PrimaryKeyType`
- [x] Define instances for selected `PrimaryKeyType`
- [x] Add remaining tables (`EventHash`, etc)
- [ ] (Unlikely) Write a `setup.sh` for new developers coming in
- [x] Add exception handling using ExceptT
