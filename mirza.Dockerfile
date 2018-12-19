FROM alpine:3.8
WORKDIR /usr/src
# ADD https://raw.githubusercontent.com/mitchty/alpine-ghc/master/mitch.tishmack%40gmail.com-55881c97.rsa.pub \
    # /etc/apk/keys/mitch.tishmack@gmail.com-55881c97.rsa.pub
# RUN echo "https://s3-us-west-2.amazonaws.com/alpine-ghc/8.0" >> /etc/apk/repositories
RUN apk --no-cache add \
  alpine-sdk \
  git \
  gmp-dev \
  zlib-dev \
  libpq \
  linux-headers \
  curl \
  postgresql-dev \
  ca-certificates \
  ghc \
  xz-dev \
  upx
RUN curl -sSL https://get.haskellstack.org/ | sh && chmod 755 /usr/local/bin/stack
RUN /usr/local/bin/stack --version
RUN /usr/local/bin/stack --system-ghc update 2>&1

ADD stack.yaml supplyChainServer.cabal run_tests.sh LICENSE README.md /usr/src/
ADD src/ /usr/src/src/
ADD test/ /usr/src/test/
ADD app/ /usr/src/app/
RUN mkdir /usr/src/dist/

# RUN /usr/local/bin/stack --system-ghc setup 2>&1
# RUN /usr/local/bin/stack --system-ghc install --split-objs --ghc-options='-fPIC -fllvm' --only-dependencies 2>&1 || /usr/local/bin/stack --system-ghc install --split-objs --ghc-options='-fPIC -fllvm' --only-dependencies 2>&1 || /usr/local/bin/stack --system-ghc install --split-objs --ghc-options='-fPIC -fllvm' --only-dependencies 2>&1
RUN /usr/local/bin/stack --system-ghc setup 2>&1
RUN /usr/local/bin/stack --system-ghc install --split-objs --ghc-options='-fPIC' --only-dependencies 2>&1 || /usr/local/bin/stack --system-ghc install --split-objs --ghc-options='-fPIC' --only-dependencies 2>&1 || /usr/local/bin/stack --system-ghc install --split-objs --ghc-options='-fPIC' --only-dependencies 2>&1
# || /usr/local/bin/stack --system-ghc install --split-objs --ghc-options='-fPIC' --only-dependencies 2>&1 || /usr/local/bin/stack --system-ghc install --split-objs --ghc-options='-fPIC -fllvm' --only-dependencies 2>&1

# WORKDIR /usr/src
# RUN rm -rf ./*
# ADD . /usr/src
RUN /usr/local/bin/stack --system-ghc install --split-objs --ghc-options='-fPIC' 2>&1
RUN ls -lah /usr/src/dist/supplyChainServer && upx /usr/src/dist/supplyChainServer && ls -lah /usr/src/dist/supplyChainServer
# RUN ls -lah /usr/src/dist/supplyChainServer && upx /usr/src/dist/supplyChainServer && ls -lah /usr/src/dist/supplyChainServer

FROM alpine:3.8
COPY --from=0 /usr/src/dist/supplyChainServer /usr/bin/supplyChainServer
RUN apk --no-cache add libpq gmp libffi libstdc++ xz-dev
RUN ls -lh /usr/bin/supplyChainServer && /usr/bin/supplyChainServer --help
# RUN ldd /usr/bin/supplyChainServer
# CMD ["sh","-c","/usr/bin/supplyChainServer --http-port $HTTP_PORT --db-connections $DB_CONNECTIONS --db-conn-string $DB_CONN_STRING +RTS -N"]
