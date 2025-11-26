# Build stage
FROM ocaml/opam:debian-13-ocaml-5.4-afl AS build

USER root
RUN apt-get update && apt-get install -y m4 bmake cpio net-tools fswatch pkg-config \
    && rm -rf /var/lib/apt/lists/*

USER opam
WORKDIR /home/opam

RUN opam update && opam upgrade -y && \
    opam install -y \
    core core_unix yaml ezjsonm mustache dune fpath ocamlfind \
    ounit ounit2 qcheck react ppx_deriving ppx_let ppx_sexp_conv \
    yojson ocp-indent calendar getopt stdio && \
    opam clean -a && \
    rm -rf ~/.opam/download-cache ~/.opam/log ~/.opam/archives


# Runtime stage
FROM debian:13-slim

USER root

RUN apt-get update && apt-get install -y libgmp10 jq rsync && \
    rm -rf /var/lib/apt/lists/*

RUN mkdir -p /tmp/exercism && chmod -R 777 /tmp/exercism

COPY --from=build /home/opam/.opam /home/opam/.opam

ENV PATH="/home/opam/.opam/5.4/bin:${PATH}"