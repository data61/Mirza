ARG HS_BUILDER_IMAGE=hsbuilder:latest

FROM $HS_BUILDER_IMAGE as BUILD
RUN mkdir -p /projects/entity-data-api/src/edapi
WORKDIR /projects/entity-data-api/src/edapi

COPY projects/mirza-common-haskell/ /projects/mirza-common-haskell/
COPY projects/entity-data-api/      /projects/entity-data-api/

RUN /usr/local/bin/stack install --test --dependencies-only --ghc-options='-O2 -j -fPIC' 2>&1

RUN mkdir /projects/entity-data-api/src/edapi/dist/ && \
	/usr/local/bin/stack install --ghc-options='-O2 -j -fPIC' 2>&1


FROM ubuntu:18.04 as PKG-EDAPI

RUN apt update && \
	apt install -y libpq-dev libffi-dev ca-certificates netbase

COPY --from=0 /projects/entity-data-api/src/edapi/dist/entity-data-api-proxy /opt/Mirza/entity-data-api

ENTRYPOINT [ "/opt/Mirza/entity-data-api" ]
