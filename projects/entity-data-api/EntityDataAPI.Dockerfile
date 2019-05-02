ARG HS_BUILDER_IMAGE=hsbuilder:latest

FROM $HS_BUILDER_IMAGE as BUILD
WORKDIR /edapi

COPY stack.yaml entity-data-api.cabal LICENSE README.md /edapi/
COPY src/ /edapi/src/
COPY app/ /edapi/app/

RUN /usr/local/bin/stack install --test --dependencies-only --ghc-options='-O2 -j -fPIC' 2>&1

RUN mkdir /edapi/dist/ && \
	/usr/local/bin/stack install --ghc-options='-O2 -j -fPIC' 2>&1


FROM ubuntu:18.04 as PKG-EDAPI

RUN apt update && \
	apt install -y libpq-dev libffi-dev ca-certificates

RUN pwd; ls; ls /

COPY --from=0 /edapi/dist/entity-data-api /opt/Mirza/entity-data-api

ENTRYPOINT [ "/opt/Mirza/entity-data-api" ]
