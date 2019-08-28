# Development Quick Start Guide

Clone the repo:
```
$ git clone https://github.com/data61/Mirza.git
```

This guide assumes that you have successfully got a development enviornment set
up and built all of the other projects. See also readme's for the individual
projects:
* [projects/or_scs/README.md](projects/or_scs/README.md)
* [projects/trails/README.md](projects/trails/README.md)
* [projects/entity-data-api/README.md](projects/entity-data-api/README.md)

For a full development stack you require the following services running:
abberviation | default port | service                       | swagger-api
-------------|--------------|-------------------------------|------------
edapi        | 8020         | Entity Data API               |
scs          | 8000         | Supply Chain Server           | http://localhost:8000/swagger-ui/#/
or           | 8200         | Organisation Registry Service | http://localhost:8200/swagger-ui/#/
trails       | 8300         | Trails Service                | http://localhost:8300/swagger-ui/#/
web          | 8080         | Web front end                 |

Get the latest node (Last verified version 10.16.3):

https://nodejs.org/en/
