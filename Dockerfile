FROM ocaml/opam2:alpine-3.8-ocaml-4.07

USER root
RUN apk --update add m4

USER opam
COPY . ./app/
RUN opam pin add lambda-dti ./app

FROM alpine:3.8

RUN echo "@testing http://dl-cdn.alpinelinux.org/alpine/edge/testing" >> /etc/apk/repositories
RUN apk --update add rlwrap@testing

COPY --from=0 /home/opam/.opam/4.07/bin/ldti /usr/bin/

# Workaround: sleep for 1 second to avoid the following error:
# rlwrap: error: My terminal reports width=0
RUN echo $'#!/bin/ash\nsleep 1; rlwrap ldti "$@"' >> /usr/bin/ldti-start.sh
RUN chmod +x /usr/bin/ldti-start.sh

ENTRYPOINT ["ldti-start.sh"]
