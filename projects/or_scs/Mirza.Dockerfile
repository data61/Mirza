ARG HS_BUILDER_IMAGE=hsbuilder:latest

FROM $HS_BUILDER_IMAGE as BUILD
WORKDIR /projects/build/or_scs

COPY projects/or_scs/stack.yaml projects/or_scs/Mirza.cabal projects/or_scs/run_tests.sh projects/or_scs/LICENSE projects/or_scs/README.md /projects/build/or_scs/
COPY projects/or_scs/src/  /projects/build/or_scs/src/
COPY projects/or_scs/test/ /projects/build/or_scs/test/
COPY projects/or_scs/app/  /projects/build/or_scs/app/
COPY projects/or_scs/license_check.py /projects/build/or_scs/
COPY projects/or_scs/auth_public_key_2019-04-01.json /projects/build/or_scs/
COPY projects/mirza-common-haskell/ /projects/build/mirza-common-haskell/
COPY projects/mirza-test-utils-haskell/ /projects/build/mirza-test-utils-haskell/

RUN /usr/local/bin/stack install --test --dependencies-only --ghc-options='-O2 -j -fPIC' 2>&1

RUN /usr/local/bin/stack install --ghc-options='-O2 -j -fPIC' 2>&1

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

COPY --from=0 /projects/build/or_scs/dist/supplyChainServer /opt/Mirza/supplyChainServer
COPY --from=0 /projects/build/or_scs/test/Mirza/SupplyChain/TestData/testKeys/goodJWKs /opt/Mirza/test/Mirza/SupplyChain/TestData/testKeys/goodJWKs

ENTRYPOINT [ "/opt/Mirza/supplyChainServer" ]

FROM ubuntu:18.04 as PKG-OR
WORKDIR /opt/Mirza

RUN apt update && \
	apt install -y libpq-dev libffi-dev ca-certificates netbase

COPY --from=0 /projects/build/or_scs/dist/orgRegistry /opt/Mirza/orgRegistry
COPY --from=0 /projects/build/or_scs/auth_public_key_2019-04-01.json /opt/Mirza/

ENTRYPOINT [ "/opt/Mirza/orgRegistry" ]
