FROM ubuntu:focal

RUN apt-get update && apt-get install -y curl libncurses5-dev libncursesw5-dev
RUN curl -sSL https://get.haskellstack.org/ | sh

# Save a little time in case a container is deleted during development
RUN stack setup 8.10.6
