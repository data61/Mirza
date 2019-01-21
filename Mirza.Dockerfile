FROM ubuntu:16.04 as PKG-SCS

COPY suppyChainServer /usr/bin/supplyChainServer

RUN apt update && \
	apt install -y libpq-dev libffi-dev ca-certificates && \
	/usr/bin/supplyChainServer --help

FROM ubuntu:16.04 as PKG-BR

COPY businessRegistry /usr/bin/businessRegistry

RUN apt update && \
	apt install -y libpq-dev libffi-dev ca-certificates && \
  /usr/bin/businessRegistry --help
