{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Composable combinators and a monadic document builder for generating
-- clean, canonical GitHub-flavored Markdown. Eliminates string concatenation
-- and whitespace-management boilerplate.
module Stronghead.Markdown
  ( -- * Document Monad
    MD
  , renderMD
  , tellBlock
    -- * Document Blocks
  , h1
  , h2
  , h3
  , h4
  , heading
  , para
  , text
  , blank
  , hr
    -- * Inline Formatting
  , bold
  , italic
  , strike
  , code
  , link
  , badge
  , kbd
    -- * Key-Value Fields
  , field
  , subfield
  , fieldCode
  , subfieldCode
  , fields
    -- * Lists
  , bullet
  , bullets
  , subbullet
  , subbullets
  , numbered
  , numberedList
  , task
    -- * Tables
  , Align (..)
  , table
  , tableAligned
    -- * Alerts / Callouts
  , AlertLevel (..)
  , alert
  , note
  , tip
  , important
  , warning
  , caution
    -- * Blockquotes
  , quote
  , quoteLines
    -- * Code Blocks
  , codeBlock
  , codeBlock_
    -- * Raw Blocks
  , raw
    -- * Utilities
  , whenJust
  ) where

import Control.Monad (zipWithM_)
import Control.Monad.State.Strict (State, execState, modify')
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Stronghead.Format (tshow)

-- | Monadic builder for Markdown documents.
newtype MD a = MD (State [Block] a)
  deriving newtype (Functor, Applicative, Monad)

-- | Append a block to the document.
tellBlock :: Block -> MD ()
tellBlock b = MD (modify' (b :))

-- | Column alignment for Markdown tables.
data Align
  = AlignLeft
  | AlignRight
  | AlignCenter
  deriving stock (Eq, Show)

-- | Callout severity levels matching GitHub alerts.
data AlertLevel
  = Note
  | Tip
  | Important
  | Warning
  | Caution
  deriving stock (Eq, Show)

-- | Structured Markdown block.
data Block
  = BHeading !Int !Text
  | BPara !Text
  | BField !Int !Text !Text
  | BBullet !Int !Text
  | BNumbered !Int !Text
  | BTable ![(Align, Text)] ![[Text]]
  | BAlert !AlertLevel !Text ![Text]
  | BQuote ![Text]
  | BCodeBlock !(Maybe Text) !Text
  | BHR
  | BBlank
  | BRaw !Text
  deriving stock (Eq, Show)

-- | Render a monadic Markdown document into canonical GitHub-flavored Markdown text.
renderMD :: MD () -> Text
renderMD (MD m) = renderBlocks (reverse (execState m []))

-- | Render a list of blocks into text with proper Markdown spacing.
renderBlocks :: [Block] -> Text
renderBlocks = T.concat . glue . mapMaybe toRenderItem
  where
    toRenderItem = \case
      BHeading lvl t -> Just (ItemBlock (T.replicate lvl "#" <> " " <> t))
      BPara t
        | T.null (T.strip t) -> Nothing
        | otherwise -> Just (ItemBlock t)
      BField lvl k v ->
        let prefix = T.replicate (lvl * 2) " " <> "- **" <> k <> ":**"
            body = if T.null v then prefix else prefix <> " " <> v
         in Just (ItemList body)
      BBullet lvl t ->
        let prefix = T.replicate (lvl * 2) " " <> "- "
         in Just (ItemList (prefix <> t))
      BNumbered n t ->
        Just (ItemList (tshow n <> ". " <> t))
      BTable cols rows -> Just (ItemBlock (renderTable cols rows))
      BAlert lvl title body -> Just (ItemBlock (renderAlert lvl title body))
      BQuote ls -> Just (ItemBlock (renderQuote ls))
      BCodeBlock mlang c ->
        let lang = fromMaybe "" mlang
         in Just (ItemBlock ("```" <> lang <> "\n" <> c <> "\n```"))
      BHR -> Just (ItemBlock "---")
      BBlank -> Just ItemBlank
      BRaw t -> Just (ItemBlock t)

data RenderItem
  = ItemList !Text
  | ItemBlock !Text
  | ItemBlank

glue :: [RenderItem] -> [Text]
glue [] = []
glue [x] = [itemText x, "\n"]
glue (x : y : xs) = case (x, y) of
  (ItemBlank, _) -> "\n" : glue (y : xs)
  (_, ItemBlank) -> itemText x : "\n" : glue (y : xs)
  (ItemList t, ItemList _) -> t : "\n" : glue (y : xs)
  (ItemList t, ItemBlock _) -> t : "\n\n" : glue (y : xs)
  (ItemBlock t, _) -> t : "\n\n" : glue (y : xs)

itemText :: RenderItem -> Text
itemText = \case
  ItemList t -> t
  ItemBlock t -> t
  ItemBlank -> ""

-- ---------------------------------------------------------------------------
-- Headings

h1 :: Text -> MD ()
h1 = heading 1

h2 :: Text -> MD ()
h2 = heading 2

h3 :: Text -> MD ()
h3 = heading 3

h4 :: Text -> MD ()
h4 = heading 4

heading :: Int -> Text -> MD ()
heading n t = tellBlock (BHeading (max 1 (min 6 n)) t)

-- ---------------------------------------------------------------------------
-- Text & Paragraphs

para :: Text -> MD ()
para t = tellBlock (BPara t)

text :: Text -> MD ()
text = para

blank :: MD ()
blank = tellBlock BBlank

hr :: MD ()
hr = tellBlock BHR

-- ---------------------------------------------------------------------------
-- Inline Formatting

bold :: Text -> Text
bold t = "**" <> t <> "**"

italic :: Text -> Text
italic t = "*" <> t <> "*"

strike :: Text -> Text
strike t = "~~" <> t <> "~~"

code :: Text -> Text
code t = "`" <> t <> "`"

link :: Text -> Text -> Text
link label url = "[" <> label <> "](" <> url <> ")"

badge :: Text -> Text
badge t = "`[" <> t <> "]`"

kbd :: Text -> Text
kbd t = "<kbd>" <> t <> "</kbd>"

-- ---------------------------------------------------------------------------
-- Key-Value Fields

-- | Render a root-level key-value metadata item: @- **key:** val@.
field :: Text -> Text -> MD ()
field k v = tellBlock (BField 0 k v)

-- | Render an indented subfield item: @  - **key:** val@.
subfield :: Text -> Text -> MD ()
subfield k v = tellBlock (BField 1 k v)

fieldCode :: Text -> Text -> MD ()
fieldCode k v = field k (code v)

subfieldCode :: Text -> Text -> MD ()
subfieldCode k v = subfield k (code v)

fields :: [(Text, Text)] -> MD ()
fields = mapM_ (uncurry field)

-- ---------------------------------------------------------------------------
-- Lists

bullet :: Text -> MD ()
bullet t = tellBlock (BBullet 0 t)

bullets :: [Text] -> MD ()
bullets = mapM_ bullet

subbullet :: Text -> MD ()
subbullet t = tellBlock (BBullet 1 t)

subbullets :: [Text] -> MD ()
subbullets = mapM_ subbullet

numbered :: Int -> Text -> MD ()
numbered n t = tellBlock (BNumbered n t)

numberedList :: [Text] -> MD ()
numberedList = zipWithM_ numbered [1 ..]

task :: Bool -> Text -> MD ()
task checked t = bullet ((if checked then "[x] " else "[ ] ") <> t)

-- ---------------------------------------------------------------------------
-- Tables

table :: [Text] -> [[Text]] -> MD ()
table headers rows = tableAligned (map (AlignLeft ,) headers) rows

tableAligned :: [(Align, Text)] -> [[Text]] -> MD ()
tableAligned cols rows = tellBlock (BTable cols rows)

renderTable :: [(Align, Text)] -> [[Text]] -> Text
renderTable cols rows =
  T.intercalate "\n" (headerLine : sepLine : map rowLine rows)
  where
    headerLine = "| " <> T.intercalate " | " (map snd cols) <> " |"
    sepLine = "| " <> T.intercalate " | " (map (alignSep . fst) cols) <> " |"
    rowLine r = "| " <> T.intercalate " | " r <> " |"
    alignSep = \case
      AlignLeft -> ":---"
      AlignRight -> "---:"
      AlignCenter -> ":---:"

-- ---------------------------------------------------------------------------
-- Alerts / Callouts

alert :: AlertLevel -> Text -> [Text] -> MD ()
alert lvl title body = tellBlock (BAlert lvl title body)

note :: Text -> [Text] -> MD ()
note = alert Note

tip :: Text -> [Text] -> MD ()
tip = alert Tip

important :: Text -> [Text] -> MD ()
important = alert Important

warning :: Text -> [Text] -> MD ()
warning = alert Warning

caution :: Text -> [Text] -> MD ()
caution = alert Caution

renderAlert :: AlertLevel -> Text -> [Text] -> Text
renderAlert lvl title bodyLines =
  let tag = alertTag lvl
      headLine = "> [!" <> tag <> "]" <> if T.null title then "" else " " <> title
      lines' = headLine : map (\l -> if T.null l then ">" else "> " <> l) bodyLines
   in T.intercalate "\n" lines'
  where
    alertTag = \case
      Note -> "NOTE"
      Tip -> "TIP"
      Important -> "IMPORTANT"
      Warning -> "WARNING"
      Caution -> "CAUTION"

-- ---------------------------------------------------------------------------
-- Blockquotes

quote :: Text -> MD ()
quote t = quoteLines (T.lines t)

quoteLines :: [Text] -> MD ()
quoteLines ls = tellBlock (BQuote ls)

renderQuote :: [Text] -> Text
renderQuote ls =
  T.intercalate "\n" (map (\l -> if T.null l then ">" else "> " <> l) ls)

-- ---------------------------------------------------------------------------
-- Code Blocks

codeBlock :: Text -> Text -> MD ()
codeBlock lang src = tellBlock (BCodeBlock (Just lang) src)

codeBlock_ :: Text -> MD ()
codeBlock_ src = tellBlock (BCodeBlock Nothing src)

-- ---------------------------------------------------------------------------
-- Raw

raw :: Text -> MD ()
raw t = tellBlock (BRaw t)

-- ---------------------------------------------------------------------------
-- Utilities

whenJust :: (Applicative m) => Maybe a -> (a -> m ()) -> m ()
whenJust (Just x) f = f x
whenJust Nothing _ = pure ()
