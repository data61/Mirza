FROM ubuntu:16.04 as PKG-SCS

ADD dist/suppyChainServer /usr/bin/supplyChainServer

RUN apt update && \
	apt install -y libpq-dev libffi-dev ca-certificates && \
	/usr/bin/supplyChainServer --help

FROM ubuntu:16.04 as PKG-BR

ADD dist/businessRegistry /usr/bin/businessRegistry

RUN apt update && \
	apt install -y libpq-dev libffi-dev ca-certificates && \
  /usr/bin/businessRegistry --help
