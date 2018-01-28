FROM debian:stretch-slim

# Download stack dependencies, locales, necessary Haskell tools and libs
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
      g++ \
      gcc \
      libc6-dev \
      libffi-dev \
      libgmp-dev \
      make \
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
      default-libmysqlclient-dev \
      screen \
      netbase \
      pkg-config

# Set locale
RUN echo en_US.UTF-8 UTF-8 >> /etc/locale.gen && \
    locale-gen
ENV LC_ALL en_US.UTF-8

# Install stack
RUN curl -L https://www.stackage.org/stack/linux-x86_64-static -o /usr/local/bin/stack.tar.gz && \
    tar -C /usr/local/bin/ --strip-components=1 -xf /usr/local/bin/stack.tar.gz stack-1.6.3-linux-x86_64-static/stack && \
    chmod +x /usr/local/bin/stack

# Create development dirs
RUN mkdir -p /development/bead && \
    mkdir    /bead-server

# Copy cabal file and install dependencies
COPY "./Bead.cabal" "/development/init/"
COPY "./stack.yaml" "/development/init/"
RUN cd /development/init && \
    stack setup && \
    stack build --only-dependencies

# Convenience scripts for development
RUN ln -s /development/bead/docker/container-script/build \
          /development/bead/docker/container-script/run \
          /development/bead/docker/container-script/dev-env-setup \
          /usr/local/bin/

# Directory for sources
VOLUME "/development/bead"

# Directory for running
VOLUME "/bead-server"

# Expose the default port
EXPOSE 8000

WORKDIR "/bead-server"