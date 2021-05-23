# Changelog for pandabot-discord

## 0.2.1
- Fix an issue with admin permissions

## 0.2

- Use a json configuration file `bot.json` instead of `bot.cfg`.
- Notifpoints are now called "points" in the code and "bamboo shoots" in the 
  user-facing interface.
- Messages which notify users that they received a point will be removed if
  the point is revoked.
- Role buttons are now cached in memory.
- Guild member info is now cached in memory.