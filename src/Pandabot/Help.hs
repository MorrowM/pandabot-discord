{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns           #-}
module Pandabot.Help
  ( customHelpCommand
  , adminHelpCommand
  ) where

import Calamity
import Calamity qualified as C
import Calamity.Commands qualified as C
import Calamity.Commands.Context ( FullContext )
import CalamityCommands.AliasType
import CalamityCommands.Check
import CalamityCommands.Command
import CalamityCommands.CommandUtils
import CalamityCommands.Context
import CalamityCommands.Dsl
import CalamityCommands.Group
import CalamityCommands.Handler
import CalamityCommands.Internal.LocalWriter
import CalamityCommands.ParameterInfo
import Control.Applicative
import Data.Default
import Data.HashMap.Lazy qualified as LH
import Data.List ( partition )
import Data.List.NonEmpty ( NonEmpty(..) )
import Data.List.NonEmpty qualified as NE
import Data.Maybe ( mapMaybe )
import Data.Text qualified as T
import Optics
import Pandabot.Bot.Util
import Polysemy qualified as P
import Polysemy.Fail qualified as P
import Polysemy.Reader qualified as P

customHelpCommand :: C.BotC r => P.Sem (C.DSLState FullContext r) (C.Command FullContext)
customHelpCommand = mkCommand (helpCommand False "help" )

mkCommand :: _ => _
mkCommand hc = hc $ \a b ->
  let (title, description) = case T.lines b of
        (x:xs) -> (Just x, T.unlines xs)
        []     -> (Nothing, b)
  in tell_ @Embed a $ def
      & #title .~ title
      & #description ?~ description


adminHelpCommand :: C.BotC r => P.Sem (C.DSLState FullContext r) (C.Command FullContext)
adminHelpCommand = mkCommand (helpCommand True "adminhelp")

data CommandOrGroup m c a
  = Command' (Command m c a)
  | Group' (Group m c a) [T.Text]

parameterTypeHelp :: [ParameterInfo] -> T.Text
parameterTypeHelp pinfo =
  let dedup = LH.toList . LH.fromList $ map (\(ParameterInfo _ t d) -> (t, d)) pinfo
      typeDescs = T.unlines ["- " <> T.pack (show t) <> ": " <> d | (t, d) <- dedup]
  in if null dedup
      then ""
      else "Types:\n" <> typeDescs <> "\n"

helpCommandHelp :: c -> T.Text
helpCommandHelp _ = "Show this message. (What else did you expect?)"

helpForCommand :: CommandContext m c a => c -> Command m c a -> T.Text
helpForCommand ctx cmd@Command{names, checks, help, params} =
  "Usage: " <> prefix' <> path' <> " " <> params' <> "\n"
    <> aliasesFmt
    <> checksFmt
    <> parameterTypeHelp params
    <> help ctx
 where
  prefix' = ctxPrefix ctx
  path' = T.unwords $ commandPath cmd
  params' = commandParams cmd
  aliases = NE.tail names
  checks' = map (^. #name) checks
  aliasesFmt =
    if null aliases
      then ""
      else "Aliases: " <> T.unwords aliases <> "\n"
  checksFmt =
    if null checks'
      then ""
      else "Checks: " <> T.unwords checks' <> "\n\n"

fmtCommandWithDescription :: c -> Command m c a -> T.Text
fmtCommandWithDescription ctx Command{names, help} = formatWithAliases names <> ": " <> help ctx

fmtGroupWithDescription :: c -> Group m c a -> T.Text
fmtGroupWithDescription ctx Group{names, help} = formatWithAliases names <> ": " <> help ctx

formatWithAliases :: NonEmpty T.Text -> T.Text
formatWithAliases (name :| aliases) = name <> aliasesFmt
 where
  aliasesFmt = case aliases of
    []       -> ""
    aliases' -> ", " <> T.intercalate "," aliases'

onlyOriginals :: [(a, AliasType)] -> [a]
onlyOriginals = mapMaybe inner
 where
  inner (_, Alias)    = Nothing
  inner (a, Original) = Just a

partitionVisibleC :: Bool -> [Command m c a] -> ([Command m c a], [Command m c a])
partitionVisibleC isAdmin cmds = (\cs -> if isAdmin then cs else []) <$> partition notHiddenC cmds

partitionVisibleG :: Bool -> [Group m c a] -> ([Group m c a], [Group m c a])
partitionVisibleG isAdmin grps = (\gs -> if isAdmin then gs else []) <$> partition notHiddenG grps

helpForGroup :: CommandContext m c a => Bool -> c -> Group m c a -> T.Text
helpForGroup isAdmin ctx grp =
  aliasesFmt
    <> checksFmt
    <> (grp ^. #help) ctx
    <> "\n"
    <> groupsMsg
    <> commandsMsg
    <> aboutHelp ctx
 where
  groups = filter (\g -> isAdmin || notHiddenG g) . onlyOriginals . LH.elems $ grp ^. #children
  commands = filter (\c -> isAdmin || notHiddenC c)  . onlyOriginals . LH.elems $ grp ^. #commands

  groupsFmt = map formatWithAliases (groups ^.. traversed % #names)
  groupsMsg =
    if null groups
      then ""
      else "The following subgroups exist:\n" <> (T.unlines . map ("- " <>) $ groupsFmt)
  commandsMsg =
    if null commands
      then ""
      else "\nThe following subcommands exist:\n" <> (T.unlines . map (("- " <>) . fmtCommandWithDescription ctx) $ commands)
  aliases = NE.tail $ grp ^. #names
  checks' = map (^. #name) $ grp ^. #checks
  aliasesFmt =
    if null aliases
      then ""
      else "Aliases: " <> T.unwords aliases <> "\n"
  checksFmt =
    if null checks'
      then ""
      else "Checks: " <> T.unwords checks' <> "\n\n"

rootHelp :: CommandContext m c a => Bool -> c -> CommandHandler m c a -> T.Text
rootHelp isAdmin ctx handler = "Commands:\n" <> groupsMsg <> commandsMsg <> adminHelp <> aboutHelp ctx
 where
  groups = partitionVisibleG isAdmin . onlyOriginals . LH.elems $ handler ^. #groups
  commands = partitionVisibleC isAdmin . onlyOriginals . LH.elems $ handler ^. #commands
  groupsFmt = mapBoth (map (fmtGroupWithDescription ctx)) groups
  (groupsMsg, hGroupsMsg) = mapBoth (T.unlines . map ("- " <>)) groupsFmt
  (commandsMsg, hcommandsMsg) = mapBoth (T.unlines . map (("- " <>) . fmtCommandWithDescription ctx)) commands
  adminHelp = if null (snd groups) && null (snd commands)
    then ""
    else "\nAdmin Commands:\n" <> hGroupsMsg <> hcommandsMsg

mapBoth :: (a -> b) -> (a, a) -> (b, b)
mapBoth f (a, b) = (f a, f b)

renderHelp :: CommandContext m c a => Bool -> CommandHandler m c a -> c -> [T.Text] -> T.Text
renderHelp isAdmin handler ctx path =
  case findCommandOrGroup isAdmin handler path of
    Just (Command' cmd@Command{names}) ->
      "Help for command `" <> NE.head names <> "`: \n" <> helpForCommand ctx cmd
    Just (Group' grp@Group{names} remainingPath) ->
      let failedMsg =
            if null remainingPath
              then ""
              else "No command or group with the path: `" <> T.unwords remainingPath <> "` exists for the group: `" <> NE.head names <> "`\n"
       in failedMsg <> "Help for command group `" <> NE.head names <> "`: \n" <> helpForGroup isAdmin ctx grp
    Nothing ->
      let failedMsg =
            if null path
              then ""
              else "No command or group with the path: `" <> T.unwords path <> "` was found.\n"
       in failedMsg <> rootHelp isAdmin ctx handler


aboutHelp :: CommandContext m c a => c -> T.Text
aboutHelp ctx = "\nUse `" <> prefix' <> "help <command>` to get more information about a specific command."
  where
    prefix' = ctxPrefix ctx

{- | Given a 'CommandHandler', optionally a parent 'Group', and a list of 'Check's,
 construct a help command that will provide help for all the commands and
 groups in the passed 'CommandHandler'.
-}
helpCommand' ::
  (Monad m, P.Member (P.Final m) r, CommandContext m c a) =>
  Bool ->
  T.Text ->
  CommandHandler m c a ->
  Maybe (Group m c a) ->
  [Check m c] ->
  (c -> T.Text -> P.Sem (P.Fail ': r) a) ->
  P.Sem r (Command m c a)
helpCommand' isAdmin name handler parent checks render =
  buildCommand @'[[T.Text]]
    (name :| [])
    parent
    isAdmin
    checks
    helpCommandHelp
    (\ctx path -> render ctx $ renderHelp isAdmin handler ctx path)

{- | Create and register the default help command for all the commands registered
 in the commands DSL this is used in. The @render@ parameter is used so you can
 determine how the help should be rendered, for example it could be
 @'putStrLn'@, or a pure function such as @'pure' . 'Left'@

 The registered command will have the name \'help\', called with no parameters
 it will render help for the top-level groups and commands, for example:

 @
 The following groups exist:
 - reanimate
 - prefix[prefixes]
 - alias[aliases]
 - remind[reminder|reminders]

 The following commands exist:
 - help :[Text]
 @

 Both commands and groups are listed in the form: @\<name\>[\<alias 0\>|\<alias 1\>]@,
 commands also have their parameter list shown.

 If a path to a group is passed, the help, aliases, and pre-invokation checks
 will be listed, along with the subgroups and commands, For example:

 @
 Help for group remind:
 Group: remind
 Aliases: reminder reminders
 Commands related to making reminders

 The following child commands exist:
 - list
 - remove reminder_id:Text
 - add :KleenePlusConcat Text
 @

 If a command path is passed, the usage, checks and help for the command are
 shown, for example:

 @
 Help for command add:
 Usage: c!prefix add prefix:Text
 Checks: prefixLimit guildOnly

 Add a new prefix
 @
-}
helpCommand ::
  forall c m a r.
  (Monad m, P.Member (P.Final m) r, CommandContext m c a) =>
  Bool ->
  T.Text ->
  (c -> T.Text -> P.Sem (P.Fail ': r) a) ->
  P.Sem (DSLState m c a r) (Command m c a)
helpCommand isAdmin name render = do
  handler <- P.ask @(CommandHandler m c a)
  let grianComm = Command ("grian" :| []) Nothing False [] [] (const "Shows you the one and only timezone.") (const undefined) (const undefined)
      handler' = handler & #commands %~ LH.insert "grian" (grianComm, Original)
  parent <- P.ask @(Maybe (Group m c a))
  checks <- P.ask @[Check m c]
  cmd <- raiseDSL $ helpCommand' isAdmin name handler' parent checks render
  ltell $ LH.singleton name (cmd, Original)
  pure cmd

notHiddenC :: Command m c a -> Bool
notHiddenC Command{hidden} = not hidden

notHiddenG :: Group m c a -> Bool
notHiddenG Group{hidden} = not hidden

findCommandOrGroup :: Bool -> CommandHandler m c a -> [T.Text] -> Maybe (CommandOrGroup m c a)
findCommandOrGroup isAdmin handler = go (handler ^. #commands, handler ^. #groups)
 where
  go ::
    (LH.HashMap T.Text (Command m c a, AliasType), LH.HashMap T.Text (Group m c a, AliasType)) ->
    [T.Text] ->
    Maybe (CommandOrGroup m c a)
  go (commands, groups) (x : xs) =
    case LH.lookup x commands of
      Just (cmd, _) | notHiddenC cmd || isAdmin -> Just (Command' cmd)
      _ -> case LH.lookup x groups of
        Just (group, _) | notHiddenG group || isAdmin -> go (group ^. #commands, group ^. #children) xs <|> Just (Group' group xs)
        _ -> Nothing
  go _ [] = Nothing
