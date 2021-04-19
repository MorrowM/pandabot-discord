FROM haskell:8.10.4
WORKDIR /opt/pandabot
RUN cabal update
COPY pandabot-discord.cabal pandabot-discord.cabal
RUN cabal build --only-dependencies -j4
COPY app app
COPY src src
RUN touch ChangeLog.md
RUN touch README.md
RUN touch LICENSE
RUN cabal install -j4
CMD ["pandabot"]