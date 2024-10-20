
### Building
FROM haskell:9.10.1-buster as builder

# Building dependencies
WORKDIR /usr/src
RUN cabal update
COPY HTTPImageServer.cabal .
RUN mkdir src
# The below command will download and compile all dependencies, but then fail
# hence the exit 0 at the end. This is a separate step so that source code
# changes don't lead to recompiling dependencies which takes a long time.
RUN cabal build --ghc-options="-fobject-code -O2" || exit 0

# Building the source
COPY src src
COPY Test Test
COPY html html
RUN ls
RUN cabal build --ghc-options="-fobject-code -O2"

# Run tests inside a docker environment
COPY docker/run_tests.sh .
RUN ./run_tests.sh

# Workaround since project incorrectly assumes cwd, meaning cabal install
# doesn't work
RUN cd dist-newstyle/build/*/ghc-9.10.1/HTTPImageServer-0.1.0.0/x/HTTPImageServer/build/HTTPImageServer && cp HTTPImageServer /usr/src/HTTPImageServer.bin
 
### The actual image
FROM debian:bookworm-slim

ENV LANG C.UTF-8

COPY --from=builder /usr/src/HTTPImageServer.bin /usr/local/bin/HTTPImageServer
RUN apt-get update -y && apt-get install -y curl libpcre3 libpcre3-dev

COPY docker/run.sh .
COPY docker/healthcheck.sh .
COPY docker/common.sh .
CMD ["./run.sh"]

HEALTHCHECK CMD "./healthcheck.sh"

EXPOSE 80
