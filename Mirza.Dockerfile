FROM ubuntu:18.04 as BUILD
WORKDIR /src

RUN apt update && apt install -y \
  git \
	libpq-dev \
	liblzma-dev \
  zlib1g-dev \
  curl \
  ca-certificates

RUN curl -sSL https://get.haskellstack.org/ | sh && chmod 755 /usr/local/bin/stack
RUN /usr/local/bin/stack --version

ADD stack.yaml Mirza.cabal run_tests.sh LICENSE README.md /src/
ADD src/ /src/src/
ADD test/ /src/test/
ADD app/ /src/app/
RUN mkdir /src/dist/

RUN /usr/local/bin/stack setup 2>&1
RUN /usr/local/bin/stack install --ghc-options='-O2 -fPIC' 2>&1

FROM ubuntu:18.04 as PKG-SCS
COPY --from=0 /src/dist/supplyChainServer /usr/bin/supplyChainServer

RUN apt update && \
	apt install -y libpq-dev libffi-dev ca-certificates && \
	/usr/bin/supplyChainServer --help

FROM ubuntu:18.04 as PKG-BR
COPY --from=0 /src/dist/businessRegistry /usr/bin/businessRegistry

RUN apt update && \
	apt install -y libpq-dev libffi-dev ca-certificates && \
  /usr/bin/businessRegistry --help
