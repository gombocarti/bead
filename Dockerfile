FROM debian:stretch-slim

# Do not ask input during package install,
# specifically when installing mysql
ENV DEBIAN_FRONTEND noninteractive

# Download locales, GHC, stack, cabal and necessary GHC tools
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
                       cabal-install locales \
                       ghc cpphs haskell-stack \
                       happy alex \
                       libpcre3 libpcre3-dev mysql-server \
                       default-libmysqlclient-dev screen \
                       netbase pkg-config

# Set locale
RUN echo en_US.UTF-8 UTF-8 >> /etc/locale.gen && \
    locale-gen
ENV LC_ALL en_US.UTF-8

# Set up mysql
RUN service mysql start && \
    mysqladmin -u root password password

# Create development dirs
RUN mkdir /development && \
    mkdir /development/init && \
    mkdir /development/bead && \
    mkdir /bead-server

# Copy cabal file and install dependencies
COPY "./Bead.cabal" "/development/init/Bead.cabal"
COPY "./stack.yaml" "/development/init/stack.yaml"
RUN cd development/init && \
    stack build --only-dependencies

# Convenience scripts for development
COPY "./container-script/build.sh" "/usr/local/bin/build"
COPY "./container-script/dev-env-setup.sh" "/development/init/dev-env-setup.sh"

# Directory for sources
VOLUME "/development/bead"

# Directory for running
VOLUME "/bead-server"

# Expose the default port
EXPOSE 8000
