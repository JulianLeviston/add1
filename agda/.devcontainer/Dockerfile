FROM ubuntu:22.04

ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update && apt-get install -y \
    git build-essential libgmp-dev zlib1g-dev pkg-config libncurses-dev xz-utils cabal-install curl sudo \
    && rm -rf /var/lib/apt/lists/*

# Install Haskell Stack
RUN curl -sSL https://get.haskellstack.org/ | sh

# Build Agda 2.7 from source
# Clone Agda
RUN git clone https://github.com/agda/agda.git /tmp/agda
RUN echo "Git Clone complete"

# Checkout the tag
WORKDIR /tmp/agda
RUN git checkout v2.7.0
RUN echo "Checkout version complete"

# Build with Cabal
RUN cabal update
RUN cabal install --installdir=/usr/local/bin --overwrite-policy=always
RUN echo "Stack build complete"

# Clean up
WORKDIR /workspace
RUN rm -rf /tmp/agda
RUN echo "Cleanup complete"

