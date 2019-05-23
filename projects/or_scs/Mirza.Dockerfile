ARG HS_BUILDER_IMAGE=hsbuilder:latest

FROM $HS_BUILDER_IMAGE as BUILD
WORKDIR /src

COPY stack.yaml Mirza.cabal run_tests.sh LICENSE README.md /src/
COPY src/ /src/src/
COPY test/ /src/test/
COPY app/ /src/app/
COPY license_check.py /src/
COPY auth_public_key_2019-04-01.json /src/

RUN /usr/local/bin/stack install --test --dependencies-only --ghc-options='-O2 -j -fPIC' 2>&1

RUN mkdir /src/dist/ && \
	/usr/local/bin/stack install --ghc-options='-O2 -j -fPIC' 2>&1

RUN stack ls dependencies --license | ./license_check.py && \
	/etc/init.d/postgresql start && \
    createdb 'testsupplychainserver' && \
    createdb 'testorgregistry' && \
    stack exec supplyChainServer -- --init-db -c 'dbname=testsupplychainserver' && \
    echo 'YES' | stack exec orgRegistry -- initdb -c 'dbname=testorgregistry' && \
	stack test --ta -j1


FROM ubuntu:18.04 as PKG-SCS

RUN apt update && \
	apt install -y libpq-dev libffi-dev ca-certificates

COPY --from=0 /src/dist/supplyChainServer /opt/Mirza/supplyChainServer
COPY --from=0 /src/test/Mirza/SupplyChain/TestData/testKeys/goodJWKs /opt/Mirza/test/Mirza/SupplyChain/TestData/testKeys/goodJWKs

ENTRYPOINT [ "/opt/Mirza/supplyChainServer" ]

FROM ubuntu:18.04 as PKG-OR

RUN apt update && \
	apt install -y libpq-dev libffi-dev ca-certificates netbase

COPY --from=0 /src/dist/orgRegistry /opt/Mirza/orgRegistry
COPY auth_public_key_2019-04-01.json /opt/Mirza/

ENTRYPOINT [ "/opt/Mirza/orgRegistry" ]
