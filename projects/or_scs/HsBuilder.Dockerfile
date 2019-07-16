FROM ubuntu:18.04

WORKDIR /projects/build/or_scs

ENV TZ=Australia/Canberra
ENV DEBIAN_FRONTEND=noninteractive

RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && \
	echo $TZ > /etc/timezone && \
	apt update && \
	apt install -y git postgresql libpq-dev liblzma-dev zlib1g-dev curl ca-certificates && \
	/etc/init.d/postgresql start && \
	su -c 'createuser --superuser root' postgres && \
	curl -sSL https://get.haskellstack.org/ | sh && chmod 755 /usr/local/bin/stack

ADD /projects/or_scs/stack.yaml /projects/or_scs/Mirza.cabal /projects/build/or_scs/
ADD /projects/mirza-common-haskell/stack.yaml /projects/mirza-common-haskell/mirza-common-haskell.cabal /projects/build/mirza-common-haskell/
ADD /projects/mirza-test-utils-haskell/stack.yaml /projects/mirza-test-utils-haskell/mirza-test-utils-haskell.cabal /projects/build/mirza-test-utils-haskell/

RUN /usr/local/bin/stack setup 2>&1
RUN /usr/local/bin/stack install --dependencies-only --ghc-options='-O2 -j -fPIC' 2>&1
RUN /usr/local/bin/stack install --test --dependencies-only --ghc-options='-O2 -j -fPIC' 2>&1
