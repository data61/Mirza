# Entity Data API (EDAPI)

## A reverse-proxy in front of the Supply Chain Server

This is a reverse proxy service in front of the service Supply Chain Server.

## Installing and Building

The easiest way to compile and run this service is via [haskell-stack](https://docs.haskellstack.org/en/stable/README/).


To compile, run:
```
stack build
```

Before you can run the service, some work needs to be done.

## Setting environment variables

Before running the EDAPI, you need to set some environment variables.

```bash
export MY_HOST="localhost" # Host for EDAPI
export MY_PORT="8020" # Port for EDAPI
export DEST_HOST="localhost" # Dest machine host
export DEST_PORT="8000" # Dest machine port
export JWK_URL="" # URL where your JWK is stored
export JWK_CLIENT_IDS='clientId1:clientId2' # list of client IDs separated by :
export EDAPI_DB_CONN='dbname=deventitydataapi' # Connection string to a database where user credentials are stored
export EDAPI_MODE='Proxy' # Proxy | API | Bootstrap
```

## Getting your credentials

The subject of your decoded JWT is treated as your credentials in this service.

If you have a valid JWT, you can put it through [this website](https://jwt.io/),
decode it and the `sub` of the decoded JWT is your credential.

To get a JWT, you may need to sign up/log in to the destination service and
look into the Local Storage of your browser (usually within the Application tab).
The `idToken` is your JWT.

## App Modes

### Bootstrap

Install Postgres 10 or higher, set up an account, and create a PostgreSQL database.
Run the SQL script in database/init.sql

```bash
psql -h hostname -d databasename -U username -f ~/Mirza/projects/entity-data-api/database/init.sql
```

Set the `EDAPI_MODE` environment variable to `Bootstrap` and run the application.

```bash
export EDAPI_MODE='Bootstrap'
stack exec entity-data-api-proxy
```

Enter your credential.
There is no validation in this mode, and this is to add the first user
to the database.

### UserManager

Once you have the first user entered, you can use this application mode. To do so:

```bash
export EDAPI_MODE='UserManager'
stack exec entity-data-api-proxy
```

You will be asked for your credential, and the user's credential who is being added.

### Proxy

This is the actual reverse-proxy service that strips out the auth header from requests
and forwards them to a service of choice.

To run:

```bash
export EDAPI_MODE='Proxy' # this is also the default
stack exec entity-data-api-proxy
```

Run the destination service alongside. Users whose credentials have been added to the
database can make requests.

