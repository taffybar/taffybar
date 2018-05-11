ARG STACK_YAML=stack-8.2.yaml
FROM taffybar/taffybar-base:latest-${STACK_YAML}
WORKDIR /taffybar
# This second arg declaration is needed because args declared outside of a from
# statement have no effect on the environment
# (see https://docs.docker.com/engine/reference/builder/#from)
ARG STACK_YAML
COPY . /taffybar
# See https://github.com/commercialhaskell/stack/issues/3830#issuecomment-378751470
# for an explanation of why we have to remove this directory.
RUN rm -rf ~/.stack/indices/
RUN stack build --no-terminal
