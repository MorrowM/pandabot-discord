# Pandabot

This is the moderation bot for the official [Ulraf Discord server](http://battlepandas.com/discord).

## Installation

Install GHC and `cabal`. Compile the project and its dependencies with `cabal build` and run with `cabal run`.

Note that there must be a `bot.json` configuration file in the working directory when you run the bot. It should look something like this:

```json
{
  "botToken": "<token>",
  "welcomeRole":  "<snowflake>",
  "pointAssignEmoji": {
    "name": "👀"
  },
  "reactPositiveEmoji": {
    "name": "PandaBot",
    "id":  "<snowflake>"
  },
  "connectionString": "database.sqlite",
  "commandPrefix": "?",
  "voiceConfig": {
    "roles": [
      {
        "name": "SHECRET voice chat",
        "role":  "<snowflake>",
        "voiceChannels": [
           "<snowflake>"
        ],
        "textChannel":  "<snowflake>"
      },
      {
        "name": "the Bamboo Jungle",
        "role":  "<snowflake>",
        "voiceChannels": [
           "<snowflake>"
        ],
        "textChannel":  "<snowflake>"
      }
    ]
  }
}
```