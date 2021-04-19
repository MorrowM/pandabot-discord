FROM haskell:8.8.4
WORKDIR /app
COPY . .
RUN cabal update
RUN cabal build
CMD ["cabal", "run"]