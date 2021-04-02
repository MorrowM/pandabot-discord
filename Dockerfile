FROM fpco/stack-build:lts-17.7
WORKDIR /app
COPY . .
RUN stack build
CMD ["stack", "run"]