FROM ubuntu:18.04

WORKDIR /src

ENV TZ=Australia/Canberra
ENV DEBIAN_FRONTEND=noninteractive

RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && \
	echo $TZ > /etc/timezone && \
	apt update && \
	apt install -y git postgresql libpq-dev liblzma-dev zlib1g-dev curl ca-certificates && \
	/etc/init.d/postgresql start && \
	su -c 'createuser --superuser root' postgres && \
	curl -sSL https://get.haskellstack.org/ | sh && chmod 755 /usr/local/bin/stack

ADD stack.yaml Mirza.cabal /src/

RUN /usr/local/bin/stack setup 2>&1
RUN /usr/local/bin/stack install --dependencies-only --ghc-options='-O2 -j -fPIC' 2>&1
RUN /usr/local/bin/stack install --test --dependencies-only --ghc-options='-O2 -j -fPIC' 2>&1
