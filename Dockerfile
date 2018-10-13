FROM ocaml/opam2:debian-9-ocaml-4.07

USER root
RUN apt-get update && apt-get install --no-install-recommends -y m4

USER opam
COPY . ./app/
RUN opam pin add lambda-dti ./app

FROM debian:stretch-slim

RUN apt-get update && apt-get install --no-install-recommends -y rlwrap

COPY --from=0 /home/opam/.opam/4.07/bin/ldti /usr/bin/

ENTRYPOINT ["ldti"]
# ENTRYPOINT ["rlwrap", "ldti"]
