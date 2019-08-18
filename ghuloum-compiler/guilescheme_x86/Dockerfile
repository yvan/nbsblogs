FROM ubuntu:bionic

WORKDIR /code

# Setup base image deps
RUN apt-get update && apt-get install -y \
    build-essential \
    gcc-multilib \
    libncurses5-dev \
    libx11-dev \
    uuid-dev \
    neovim \
    && rm -rf /var/lib/apt/lists/*

RUN apt-get update && apt-get install -y guile-2.0

ENTRYPOINT ["/bin/bash"]
