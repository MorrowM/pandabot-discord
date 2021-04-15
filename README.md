# Pandabot
This is the moderation bot for the official [Ulraf Discord server](http://battlepandas.com/discord).

## Installation

Install GHC and `cabal`. Compile the project and its dependencies with `cabal build` and run with `cabal run`.

Note that there must be a `bot.json` configuration file in the working directory when you run the bot. It should look something like this:

```json
{
  "botToken": "<token>",
  "welcomeRole": "<snowflake>",
  "pointAssignEmoji": "<emoji>",
  "reactPositiveEmoji": "<emoji>",
  "connectionString": "<filename>",
  "commandPrefix": "<string>",
  "voiceConfig": {
    "roles": [
      {
        "name": "<string>",
        "role": <snowflake>,
        "voiceChannels": [
          <snowflake>
        ],
        "textChannel": <snowflake>
      },
      {
        "name": "<string>",
        "role": <snowflake>,
        "voiceChannels": [
          <snowflake>
        ],
        "textChannel": <snowflake>
      }
    ]
  }
}
```