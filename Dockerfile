FROM ocaml/opam:alpine-3.20-ocaml-5.2

USER root
RUN apk --update add m4

USER opam
COPY . ./app/
RUN opam pin add lambda-dti ./app

FROM alpine:3.20

RUN apk --update add rlwrap

COPY --from=0 /home/opam/.opam/5.2/bin/ldti /usr/bin/

# Workaround: sleep for 1 second to avoid the following error:
# rlwrap: error: My terminal reports width=0
RUN echo $'#!/bin/ash\nsleep 1; rlwrap ldti "$@"' >> /usr/bin/ldti-start.sh
RUN chmod +x /usr/bin/ldti-start.sh

ENTRYPOINT ["ldti-start.sh"]
