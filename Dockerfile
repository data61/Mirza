FROM etd-docker01.it.csiro.au/gs1/haskell-build-image

WORKDIR /app

ADD . /app/supplyChainServer

RUN eval $(ssh-agent) && \
    ssh-add ~/.ssh/GS1_DEPLOY_KEY && \
    cd /app/supplyChainServer && \
    stack build

EXPOSE 8000

WORKDIR /app/supplyChainServer

CMD ["stack", "exec", "supplyChainServer", "--", "run"]
