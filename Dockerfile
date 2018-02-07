FROM ocaml/opam:debian-9_ocaml-4.06.0
COPY . ./app/
RUN opam pin add lambda-rti ./app

FROM debian:stretch-slim
COPY --from=0 /home/opam/.opam/4.06.0/bin/lrti /usr/bin/
ENTRYPOINT ["lrti"]

# RUN apt-get update && apt-get install --no-install-recommends -y rlwrap
# ENTRYPOINT ["rlwrap", "lrti"]
