FROM alpine:3.8
WORKDIR /usr/src

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

ADD stack.yaml Mirza.cabal run_tests.sh LICENSE README.md /usr/src/
ADD src/ /usr/src/src/
ADD test/ /usr/src/test/
ADD app/ /usr/src/app/
RUN mkdir /usr/src/dist/

RUN /usr/local/bin/stack --system-ghc setup 2>&1
RUN /usr/local/bin/stack --system-ghc install --split-objs --ghc-options='-fPIC' --only-dependencies 2>&1 || /usr/local/bin/stack --system-ghc install --split-objs --ghc-options='-fPIC' --only-dependencies 2>&1 || /usr/local/bin/stack --system-ghc install --split-objs --ghc-options='-fPIC' --only-dependencies 2>&1

RUN /usr/local/bin/stack --system-ghc install --split-objs --ghc-options='-fPIC' 2>&1
RUN ls -lah /usr/src/dist/businessRegistry && upx /usr/src/dist/businessRegistry && ls -lah /usr/src/dist/businessRegistry

FROM alpine:3.8
COPY --from=0 /usr/src/dist/businessRegistry /usr/bin/businessRegistry
RUN apk --no-cache add libpq gmp libffi libstdc++ xz-dev
RUN ls -lh /usr/bin/businessRegistry && /usr/bin/businessRegistry --help
