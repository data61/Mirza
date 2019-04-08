ARG HS_BUILDER_IMAGE=hsbuilder:latest

FROM $HS_BUILDER_IMAGE as BUILD
WORKDIR /src

COPY stack.yaml entity-data-api.cabal LICENSE README.md /src/
COPY src/ /src/src/
COPY app/ /src/app/

RUN /usr/local/bin/stack install --test --dependencies-only --ghc-options='-O2 -j -fPIC' 2>&1

RUN mkdir /src/dist/ && \
	/usr/local/bin/stack install --ghc-options='-O2 -j -fPIC' 2>&1


FROM ubuntu:18.04 as PKG-EDAPI

# RUN apt update && \
# 	apt install -y libpq-dev libffi-dev ca-certificates

COPY --from=0 /src/dist/entity-data-api /opt/Mirza/entity-data-api

ENTRYPOINT [ "/opt/Mirza/entity-data-api" ]
