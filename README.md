# Development Quick Start Guide

Clone the repo:

```console
git clone https://github.com/data61/Mirza.git
```


# Installing Build Dependencies

This guide assumes that you have successfully got a development enviornment set
up for each of the sub projects and have been able to build all of the other
projects. See also readme's for the individual projects:
* [projects/or_scs/README.md](projects/or_scs/README.md)
* [projects/trails/README.md](projects/trails/README.md)
* [projects/entity-data-api/README.md](projects/entity-data-api/README.md)
* [projects/web/README.md](projects/web/README.md)


# Service Overview

For a full development stack you require the following services running:

Service                       | Service Abberviation | Default Port |  Swagger API URL
------------------------------|----------------------|--------------|-------------
Entity Data API               | edapi                | 8020         |
Supply Chain Server           | scs                  | 8000         |  http://localhost:8000/swagger-ui/#/
Organisation Registry Service | or                   | 8200         |  http://localhost:8200/swagger-ui/#/
Trails Service                | trails               | 8300         |  http://localhost:8300/swagger-ui/#/
Web Front End                 | web                  | 8080         |


# Build and Run all services for development on local machine

To complete these steps you require:
- OAuthSub:      auth0|00ff00ff00ff00ff00ff00ff
- JWK_CLIENT_IDS: XXxx00XXxx00XXxx00XXxx00XXxx00XX

One way to get your OAuth Subject is to go to your browser, visit the page serving the platform Mirza
(if running locally, it will typically be http://localhost:8080). In your developer tools,
look for the `Application` tab. Click on `Local Storage` and look at the cookies stored locally.
In the JSON blob, find the value of the property `idToken`.
You can decode that string via https://jwt.io/. In the decoded JSON, look for the
property `sub`. If you logged in via Auth0, then the OAuth Sub will look like `auth0|blahblahblah`.

`JWK_CLIENT_IDS` can be found in your auth provider (e.g, Auth0)'s settings.


The following is a configuration which will build and start all of the services:

```bash
# Fill in your proper details here:
project_root="~/some-path/"
OAuthSub="auth0|00ff00ff00ff00ff00ff00ff"
export JWK_CLIENT_ID="JvrGZu2gNR84nrEOu1kEC3gxpcJn9wkU"

# Trails
cd ${project_root}projects/trails
stack build --fast                                   # Look for: > Registering library for trails-0.1.0.0.. Completed 3 action(s).
createdb devtrails
stack exec trails -- initdb -c "dbname=devtrails"    # Look for: Right ()
stack exec trails -- server -c "dbname=devtrails" &  # Look for: (Logging will be to: stdout) http://localhost:8300/swagger-ui/


# OR_SCS
cd ${project_root}projects/or_scs
./restart.sh &
# Look for:
# - CREATE DATABASE devsupplychainserver; CREATE DATABASE sql script successful
# - CREATE DATABASE devmirzaorgregistry; CREATE DATABASE sql script successful
# - Right ()
# - Listening on http://localhost:8200/swagger-ui/
# - http://localhost:8000/swagger-ui/

# Add the development user to the OrgRegistry
echo ${OAuthSub} | stack exec orgRegistry -- -c "dbname=devorgregistry" user add
echo -e "0000000\nDevelopment Org\nhttp://localhost:8020" | stack exec orgRegistry -- -c "dbname=devorgregistry" org add
echo -e "0000000\n${OAuthSub}" | stack exec orgRegistry -- -c "dbname=devorgregistry" org addUser


# EDAPI
cd ${project_root}projects/entity-data-api
stack build --fast

export MY_HOST="localhost" # Host for EDAPI
export MY_PORT="8020" # Port for EDAPI
export DEST_HOST="localhost" # Dest machine host
export DEST_PORT="8000" # Dest machine port
export JWK_URL="https://mirza.au.auth0.com/.well-known/jwks.json" # URL where your JWK is stored
export JWK_CLIENT_IDS="${JWK_CLIENT_ID}" # list of client IDs separated by ,
export EDAPI_DB_CONN='dbname=devedapi' # Connection string to a database where user credentials are stored

# Create and initalise the database
createdb devedapi
psql -d devedapi -f ${project_root}projects/entity-data-api/database/init.sql
# Look for: CREATE TABLE

# You need to bootstrap the first user into the EDAPI
export EDAPI_MODE='Bootstrap' # Proxy | API | Bootstrap
echo ${OAuthSub} | stack exec entity-data-api-proxy
# Look for: Right ()

# Once the user has been succesfully bootstrapped you can start the server.
export EDAPI_MODE='Proxy' # Proxy | API | Bootstrap
echo ${OAuthSub} | stack exec entity-data-api-proxy &
# Look for: Starting service on localhost:8020

# Web Front End
cd ${project_root}projects/web

npm install
npm run start:dev
```

Once the service is running navigate to http://localhost:8080/ and either sign in or signup.

The following is the format of a URN: `urn:epc:id:sgtin:0000000.000000.0000`
