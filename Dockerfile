FROM haskell:latest
RUN stack --version

RUN stack setup
RUN stack --resolver lts build stack
RUN stack exec servant-persistent-template

EXPOSE 3000