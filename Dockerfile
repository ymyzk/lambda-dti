FROM debian:stretch

ENV OCAML_VERSION 4.05.0

RUN apt-get update && \
        apt-get install -y aspcud build-essential m4 unzip wget

RUN wget https://raw.github.com/ocaml/opam/master/shell/opam_installer.sh -O - | sh -s /usr/local/bin
RUN adduser --disabled-password opam
USER opam

RUN opam init --comp $OCAML_VERSION
RUN opam install jbuilder menhir ounit
