
# Building
FROM haskell:9.10.1-buster as builder

WORKDIR /usr/src
RUN git clone https://github.com/osklunds/HTTPImageServer.git
WORKDIR /usr/src/HTTPImageServer
RUN git checkout ce4829df11bff4bea44b87017f5692afe9efbae5
RUN cabal update && \
    cabal build --ghc-options="-fobject-code -O2"

# Workaround since project incorrectly assumes cwd, meaning cabal install
# doesn't work
RUN cd dist-newstyle/build/*/ghc-9.10.1/HTTPImageServer-0.1.0.0/x/HTTPImageServer/build/HTTPImageServer && cp HTTPImageServer /usr/src/HTTPImageServer.bin
 
FROM debian:bookworm-slim
COPY --from=builder /usr/src/HTTPImageServer.bin /usr/local/bin/HTTPImageServer
RUN apt-get update -y && apt-get install -y curl
COPY run.sh .
COPY healthcheck.sh .
COPY common.sh .
CMD ["./run.sh"]

HEALTHCHECK CMD "./healthcheck.sh"

EXPOSE 80
