FROM mcr.microsoft.com/azure-functions/base:4-appservice 

RUN env

ENV HOME=/root

# avoid interactive mode when installing ghcup
ENV BOOTSTRAP_HASKELL_NONINTERACTIVE=1
# GHC version
ENV BOOTSTRAP_HASKELL_GHC_VERSION=9.6.3
# Cabal version
ENV BOOTSTRAP_HASKELL_CABAL_VERSION=3.10.1.0

RUN apt-get update && \
    apt-get -y install --no-install-recommends lsb-release git zlib1g-dev build-essential curl libffi-dev libffi7 libgmp-dev libgmp10 libncurses-dev libncurses5 libtinfo5 && \
    apt-get clean

RUN bash -c "curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh"

RUN echo "export PATH=$PATH:/root/.local/bin:/root/.ghcup/bin" >> /root/.bashrc

COPY initialize.sh /root/initialize.sh
RUN chmod 755 /root/initialize.sh

ENTRYPOINT ["/bin/bash", "--login", "/root/initialize.sh"]

