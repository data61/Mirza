# Trails

This readme describes the specific differences from the OR_SCS build and configuration for the trails service. You should read this readme with reference to the readme for the OR_SCS located here: [../or_scs/README.md](../or_scs/README.md)

## Development Environment Dependencies

Install stack and PostgreSQL as per the [OR_SCS](../or_scs/README.md) instructions.

## Build

To build the service run:
```shell
stack build
```

## Initalisation

As with the OR_SCS initalisation is only required to be performed once when you first run the application and no database previously exists.

Create a database for the service to run:
```shell
createdb 'devtrails'
```

To initalise the database run:
```shell
stack exec trails -- initdb -c "dbname=devtrails"
```

Again the `-c` option takes a database connection string in [libpq format](https://www.postgresql.org/docs/9.5/static/libpq-connect.html#LIBPQ-CONNSTRING) as with the OR_SCS.

## Running the server

To launch the trails service run:
```shell
stack exec trails -c "dbname=devtrails"
```

You can view and explore other commandline options with:
```shell
stack exec trails -- --help
```

Once the service is running you can check out the API at:
<http://localhost:8300/swagger-ui/>


## Running the tests

```shell
stack test --fast
```

Note: The test database does not need to be initalised beforehand, if the database doesn't exist the test database will be created and initalised automatically. If the test database is on a previous version you will either need to migrate the test database as you would with the production database (as at the time of writing there are no additional versions so at this point this is not yet documented) or drop the database with `dropdb testtrails` (for instance if you were using a previous development version that does not have an explicit migration path) so that a fresh and cleanly initalised database is created.


For development it is often handy to use the build command:
```shell
stack test --fast --file-watch
```
