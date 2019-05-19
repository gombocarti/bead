FROM debian:stretch

# Download stack dependencies, locales, necessary Haskell tools and libs
RUN apt-get update && \
    apt-get upgrade -y --no-install-recommends && \
    apt-get install -y --no-install-recommends \
      g++ \
      gcc \
      libc6-dev \
      libffi-dev \
      libgmp-dev \
      make \
      netbase \
      xz-utils \
      zlib1g-dev \
      git \
      gnupg && \
    apt-get install -y --no-install-recommends \
      curl \
      ca-certificates \
      patch \
      locales \
      cpphs \
      happy \
      alex \
      libkrb5-dev \
      libpcre3 \
      libpcre3-dev \
      screen \
      netbase \
      pkg-config

# Set locale
RUN echo en_US.UTF-8 UTF-8 >> /etc/locale.gen && \
    locale-gen
ENV LC_ALL en_US.UTF-8

# Install stack
RUN curl -L https://www.stackage.org/stack/linux-x86_64 -o /usr/local/bin/stack.tar.gz && \
    tar -C /usr/local/bin/ --strip-components=1 -xf /usr/local/bin/stack.tar.gz stack-1.9.3-linux-x86_64/stack && \
    chmod +x /usr/local/bin/stack

# Create development dirs
RUN mkdir -p /development/bead && \
    mkdir    /bead-server && \
    chown dev:dev /bead-server

# Create user
RUN adduser --system --uid 1000 --group dev

USER dev

# Copy cabal file and install dependencies
COPY --chown=dev:dev "./Bead.cabal" "/development/init/"
COPY --chown=dev:dev "./stack.yaml" "/development/init/"
RUN cd /development/init && \
    stack setup && \
    stack build --only-dependencies --ghc-options "-dynamic"

USER root

# Expose the default port
EXPOSE 8000
