ARG STACK_YAML=stack-8.2.yaml
FROM taffybar/taffybar-base:latest-${STACK_YAML}
WORKDIR /taffybar
# This second arg declaration is needed because args declared outside of a from
# statement have no effect on the environment
# (see https://docs.docker.com/engine/reference/builder/#from)
ARG STACK_YAML
COPY . /taffybar
RUN stack update
RUN stack build --no-terminal
