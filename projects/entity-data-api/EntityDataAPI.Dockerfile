ARG HS_BUILDER_IMAGE=hsbuilder:latest

FROM $HS_BUILDER_IMAGE as BUILD
RUN mkdir -p /src/edapi 
WORKDIR /src/edapi

COPY stack.yaml entity-data-api.cabal LICENSE README.md /src/edapi/
COPY src/ /src/edapi/src/
COPY app/ /src/edapi/app/

RUN /usr/local/bin/stack install --test --dependencies-only --ghc-options='-O2 -j -fPIC' 2>&1

RUN mkdir /src/edapi/dist/ && \
	/usr/local/bin/stack install --ghc-options='-O2 -j -fPIC' 2>&1


FROM ubuntu:18.04 as PKG-EDAPI

RUN apt update && \
	apt install -y libpq-dev libffi-dev ca-certificates

RUN pwd; ls /; ls /src/edapi; ls /src

COPY /src/edapi/dist/entity-data-api /opt/Mirza/entity-data-api

ENTRYPOINT [ "/opt/Mirza/entity-data-api" ]
