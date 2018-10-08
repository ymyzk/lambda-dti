FROM ocaml/opam2:debian-9-ocaml-4.07

USER root
RUN apt-get update && apt-get install --no-install-recommends -y m4

USER opam
COPY . ./app/
RUN opam pin add lambda-rti ./app

FROM debian:stretch-slim

RUN apt-get update && apt-get install --no-install-recommends -y rlwrap

COPY --from=0 /home/opam/.opam/4.07/bin/lrti /usr/bin/

ENTRYPOINT ["lrti"]
# ENTRYPOINT ["rlwrap", "lrti"]
