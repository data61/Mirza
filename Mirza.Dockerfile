FROM ubuntu:16.04 as PKG-SCS

COPY dist/supplyChainServer /opt/Mirza/supplyChainServer
COPY test/Mirza/SupplyChain/TestData/testKeys/goodJWKs /opt/Mirza/test/Mirza/SupplyChain/TestData/testKeys/goodJWKs

RUN apt update && \
	apt install -y libpq-dev libffi-dev ca-certificates && \
	/opt/Mirza/supplyChainServer --help

FROM ubuntu:16.04 as PKG-BR

COPY dist/businessRegistry /opt/Mirza/businessRegistry

RUN apt update && \
	apt install -y libpq-dev libffi-dev ca-certificates && \
  /opt/Mirza/businessRegistry --help
