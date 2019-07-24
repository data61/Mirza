ARG HS_BUILDER_IMAGE=hsbuilder:latest

FROM $HS_BUILDER_IMAGE as BUILD
RUN mkdir -p /projects/trails/src/trails
WORKDIR /projects/trails/src/trails

COPY projects/mirza-common-haskell/     /projects/mirza-common-haskell/
COPY projects/mirza-test-utils-haskell/ /projects/build/mirza-test-utils-haskell/
COPY projects/trails/                   /projects/trails/

RUN /usr/local/bin/stack install --test --dependencies-only --ghc-options='-O2 -j -fPIC' 2>&1

RUN /usr/local/bin/stack install --ghc-options='-O2 -j -fPIC' 2>&1


FROM ubuntu:18.04 as PKG-TRAILS

RUN apt update && \
	apt install -y libpq-dev libffi-dev ca-certificates netbase

COPY --from=0 /projects/trails/dist/trails /opt/Mirza/trails

ENTRYPOINT [ "/opt/Mirza/trails" ]
