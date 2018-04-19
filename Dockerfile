FROM debian:sid as taffyenv

# Stack and things that install go here
RUN mkdir -p "/root/.local/bin"
ENV PATH="/root/.local/bin:${PATH}"

# Work from /taffybar
RUN mkdir /taffybar
WORKDIR /taffybar

# Install pkg-config dependencies
RUN apt-get update
RUN apt-get -y install git libgtk-3-dev libgirepository1.0-dev libasound2-dev libgdk-pixbuf2.0-dev libgmp3-dev libxml++2.6-dev

# This installs stack into $HOME/.local/bin
RUN curl -L https://www.stackage.org/stack/linux-x86_64 | tar xz --wildcards --strip-components=1 -C ~/.local/bin '*/stack'
RUN stack install gtk2hs-buildtools

# The stack file is all that is needed to install ghc, so only copying it avoids
# rebuilding the layer if only other things in the file change.
COPY ./stack.yaml /taffybar/
RUN stack --no-terminal --install-ghc setup

# As with installing GHC, we avoid doing a full copy so that we only rebuild
# this layer when dependencies are actually updated.
COPY ./taffybar.cabal /taffybar/
RUN stack build --no-terminal --only-dependencies

FROM taffyenv as taffybuild
# Actually build taffybar.
COPY . /taffybar/
RUN stack --no-terminal build
