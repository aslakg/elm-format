{-# OPTIONS_GHC -Wall #-}
module ElmFormat.Render.Box where

import Elm.Utils ((|>))
import Box
import ElmVersion (ElmVersion(..))

import qualified ASTf.V0_16 as ASTf
import ASTf.V0_16 (UppercaseIdentifier(..), LowercaseIdentifier(..), SymbolIdentifier(..), WithEol)
import ASTf.Declaration (TopLevelStructure, Declaration)
import qualified ASTf.Declaration
import qualified ASTf.Expression
import qualified ASTf.Module
import qualified ASTf.Pattern
import qualified ASTf.Variable
import qualified Cheapskate.Types as Markdown
import qualified Control.Monad as Monad
import qualified Data.Char as Char
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, maybeToList)
import qualified Data.Maybe as Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified ElmFormat.Render.ElmStructure as ElmStructure
import qualified ElmFormat.Render.Markdown
import qualified ElmFormat.Upgrade_0_19 as Upgrade_0_19
import qualified ElmFormat.Version
import qualified ElmVersion
import qualified Parse.Parse as Parse
import qualified Reporting.Annotation as RA
import qualified Reporting.Region as Region
import qualified Reporting.Result as Result
import qualified ReversedList
import ReversedList (Reversed)
import Text.Printf (printf)
import Util.List


pleaseReport'' :: String -> String -> String
pleaseReport'' what details =
    "<elm-format-" ++ ElmFormat.Version.asString ++ ": "++ what ++ ": " ++ details ++ " -- please report this at https://github.com/avh4/elm-format/issues >"


pleaseReport' :: String -> String -> Line
pleaseReport' what details =
    keyword $ pleaseReport'' what details


pleaseReport :: String -> String -> Box
pleaseReport what details =
    line $ pleaseReport' what details


surround :: Char -> Char -> Box -> Box
surround left right b =
  let
    left' = punc (left : [])
    right' = punc (right : [])
  in
    case b of
      SingleLine b' ->
          line $ row [ left', b', right' ]
      _ ->
          stack1
              [ b
                  |> prefix left'
              , line $ right'
              ]


parens :: Box -> Box
parens = surround '(' ')'


formatBinary :: Bool -> Box -> [ ( Bool, ASTf.Comments, Box, Box ) ] -> Box
formatBinary multiline left ops =
    case ops of
        [] ->
            left

        ( isLeftPipe, comments, op, next ) : rest ->
            if isLeftPipe then
                ElmStructure.forceableSpaceSepOrIndented multiline
                    (ElmStructure.spaceSepOrStack left $
                        concat
                            [ Maybe.maybeToList $ formatComments comments
                            , [op]
                            ]
                    )
                    [formatBinary multiline next rest]
            else
                formatBinary
                    multiline
                    (ElmStructure.forceableSpaceSepOrIndented multiline left [formatCommented' comments id $ ElmStructure.spaceSepOrPrefix op next])
                    rest


splitWhere :: (a -> Bool) -> [a] -> [[a]]
splitWhere predicate list =
    let
        merge acc result =
            ReversedList.push (ReversedList.toList acc) result

        step (acc,result) next =
            if predicate next then
                (ReversedList.empty, merge (ReversedList.push next acc) result)
            else
                (ReversedList.push next acc, result)
    in
      list
          |> foldl step (ReversedList.empty, ReversedList.empty)
          |> uncurry merge
          |> ReversedList.toList
          |> dropWhile null


data DeclarationType
  = DComment
  | DStarter
  | DCloser
  | DDefinition (Maybe ASTf.Variable.Ref)
  | DFixity
  | DDocComment
  deriving (Show)


declarationType :: (a -> BodyEntryType) -> TopLevelStructure a -> DeclarationType
declarationType entryType decl =
  case decl of
    ASTf.Declaration.Entry entry ->
        case entryType $ RA.drop entry of
            BodyNamed name -> DDefinition (Just name)
            BodyUnnamed -> DDefinition Nothing
            BodyFixity -> DFixity

    ASTf.Declaration.DocComment _ ->
      DDocComment

    ASTf.Declaration.BodyComment ASTf.CommentTrickOpener ->
      DStarter

    ASTf.Declaration.BodyComment ASTf.CommentTrickCloser ->
      DCloser

    ASTf.Declaration.BodyComment _ ->
      DComment


removeDuplicates :: Ord a => [[a]] -> [[a]]
removeDuplicates input =
    foldl step (ReversedList.empty, Set.empty) input |> fst |> ReversedList.toList
    where
        step :: Ord a => (Reversed [a], Set a) -> [a] -> (Reversed [a], Set a)
        step (acc, seen) next =
            case foldl stepChildren (ReversedList.empty, seen) next |> (\(a,b) -> (ReversedList.toList a, b)) of
                ([], seen') -> (acc, seen')
                (children', seen') -> (ReversedList.push children' acc, seen')

        stepChildren :: Ord a => (Reversed a, Set a) -> a -> (Reversed a, Set a)
        stepChildren (acc, seen) next =
            if Set.member next seen
                then (acc, seen)
                else (ReversedList.push next acc, Set.insert next seen)


sortVars :: Bool -> Set (ASTf.Commented ASTf.Variable.Value) -> [[String]] -> ([[ASTf.Commented ASTf.Variable.Value]], ASTf.Comments)
sortVars forceMultiline fromExposing fromDocs =
    let
        varOrder :: ASTf.Commented ASTf.Variable.Value -> (Int, String)
        varOrder (ASTf.Commented _ (ASTf.Variable.OpValue (SymbolIdentifier name)) _) = (1, name)
        varOrder (ASTf.Commented _ (ASTf.Variable.Union (UppercaseIdentifier name, _) _) _) = (2, name)
        varOrder (ASTf.Commented _ (ASTf.Variable.Value (LowercaseIdentifier name)) _) = (3, name)

        listedInDocs =
            fromDocs
                |> fmap (Maybe.mapMaybe (\v -> Map.lookup v allowedInDocs))
                |> filter (not . List.null)
                |> fmap (fmap (\v -> ASTf.Commented [] v []))
                |> removeDuplicates

        listedInExposing =
            fromExposing
                |> Set.toList
                |> List.sortOn varOrder

        varName (ASTf.Commented _ (ASTf.Variable.Value (LowercaseIdentifier name)) _) = name
        varName (ASTf.Commented _ (ASTf.Variable.OpValue (SymbolIdentifier name)) _) = name
        varName (ASTf.Commented _ (ASTf.Variable.Union (UppercaseIdentifier name, _) _) _) = name

        varSetToMap set =
            Set.toList set
                |> fmap (\(ASTf.Commented pre var post)-> (varName (ASTf.Commented pre var post), var))
                |> Map.fromList

        allowedInDocs =
            varSetToMap fromExposing

        allFromDocs =
            Set.fromList $ fmap varName $ concat listedInDocs

        inDocs x =
            Set.member (varName x) allFromDocs

        remainingFromExposing =
            listedInExposing
                |> filter (not . inDocs)

        commentsFromReorderedVars =
            listedInExposing
                |> filter inDocs
                |> fmap (\(ASTf.Commented pre _ post) -> pre ++ post)
                |> concat
    in
    if List.null listedInDocs && forceMultiline
        then ( fmap (\x -> [x]) remainingFromExposing, commentsFromReorderedVars )
        else ( listedInDocs ++ if List.null remainingFromExposing then [] else [ remainingFromExposing ], commentsFromReorderedVars )


formatModuleHeader :: ElmVersion -> Bool -> ASTf.Module.Module -> [Box]
formatModuleHeader elmVersion addDefaultHeader modu =
  let
      maybeHeader =
        if addDefaultHeader
            then Just (ASTf.Module.header modu |> Maybe.fromMaybe ASTf.Module.defaultHeader)
            else ASTf.Module.header modu

      refName (ASTf.Variable.VarRef _ (LowercaseIdentifier name)) = name
      refName (ASTf.Variable.TagRef _ (UppercaseIdentifier name)) = name
      refName (ASTf.Variable.OpRef (SymbolIdentifier name)) = name

      varName (ASTf.Commented _ (ASTf.Variable.Value (LowercaseIdentifier name)) _) = name
      varName (ASTf.Commented _ (ASTf.Variable.OpValue (SymbolIdentifier name)) _) = name
      varName (ASTf.Commented _ (ASTf.Variable.Union (UppercaseIdentifier name, _) _) _) = name

      documentedVars :: [[String]]
      documentedVars =
          ASTf.Module.docs modu
              |> RA.drop
              |> fmap Foldable.toList
              |> Maybe.fromMaybe []
              |> concatMap extractDocs

      documentedVarsSet :: Set String
      documentedVarsSet = Set.fromList $ concat documentedVars

      extractDocs block =
          case block of
              Markdown.ElmDocs vars ->
                  fmap (fmap (refName . textToRef)) vars
              _ -> []

      textToRef :: Text -> ASTf.Variable.Ref
      textToRef text =
          case Text.unpack text of
              s@(c:_) | Char.isUpper c -> ASTf.Variable.TagRef [] (UppercaseIdentifier s)
              s@(c:_) | Char.isLower c -> ASTf.Variable.VarRef [] (LowercaseIdentifier s)
              '(':a:')':[] -> ASTf.Variable.OpRef (SymbolIdentifier $ a:[])
              '(':a:b:')':[] -> ASTf.Variable.OpRef (SymbolIdentifier $ a:b:[])
              s -> ASTf.Variable.VarRef [] (LowercaseIdentifier s)

      definedVars :: Set (ASTf.Commented ASTf.Variable.Value)
      definedVars =
          ASTf.Module.body modu
              |> concatMap extractVarName
              |> fmap (\x -> ASTf.Commented [] x [])
              |> Set.fromList

      exportsList =
          case
              ASTf.Module.exports (maybeHeader |> Maybe.fromMaybe ASTf.Module.defaultHeader)
          of
              Just (ASTf.KeywordCommented _ _ e) -> e
              Nothing -> ASTf.Variable.ClosedListing

      detailedListingToSet :: ASTf.Variable.Listing ASTf.Module.DetailedListing -> Set (ASTf.Commented ASTf.Variable.Value)
      detailedListingToSet (ASTf.Variable.OpenListing _) = Set.empty
      detailedListingToSet ASTf.Variable.ClosedListing = Set.empty
      detailedListingToSet (ASTf.Variable.ExplicitListing (ASTf.Module.DetailedListing values operators types) _) =
          Set.unions
              [ Map.assocs values |> fmap (\(name, ASTf.Commented pre () post) -> ASTf.Commented pre (ASTf.Variable.Value name) post) |> Set.fromList
              , Map.assocs operators |> fmap (\(name, ASTf.Commented pre () post) -> ASTf.Commented pre (ASTf.Variable.OpValue name) post) |> Set.fromList
              , Map.assocs types |> fmap (\(name, ASTf.Commented pre (preListing, listing) post) -> ASTf.Commented pre (ASTf.Variable.Union (name, preListing) listing) post) |> Set.fromList
              ]

      detailedListingIsMultiline :: ASTf.Variable.Listing a -> Bool
      detailedListingIsMultiline (ASTf.Variable.ExplicitListing _ isMultiline) = isMultiline
      detailedListingIsMultiline _ = False

      varsToExpose =
          case ASTf.Module.exports =<< maybeHeader of
              Nothing ->
                  if null $ concat documentedVars
                      then definedVars
                      else definedVars |> Set.filter (\v -> Set.member (varName v) documentedVarsSet)
              Just (ASTf.KeywordCommented _ _ e) -> detailedListingToSet e

      sortedExports =
          sortVars
              (detailedListingIsMultiline exportsList)
              varsToExpose
              documentedVars

      extractVarName :: TopLevelStructure Declaration -> [ASTf.Variable.Value]
      extractVarName decl =
          case decl of
              ASTf.Declaration.DocComment _ -> []
              ASTf.Declaration.BodyComment _ -> []
              ASTf.Declaration.Entry (RA.A _ (ASTf.Declaration.PortAnnotation (ASTf.Commented _ (LowercaseIdentifier name) _) _ _)) -> [ ASTf.Variable.Value (LowercaseIdentifier name) ]
              ASTf.Declaration.Entry (RA.A _ (ASTf.Declaration.Definition (RA.A _ (ASTf.Pattern.VarPattern (LowercaseIdentifier name))) _ _ _)) -> [ ASTf.Variable.Value (LowercaseIdentifier name) ]
              ASTf.Declaration.Entry (RA.A _ (ASTf.Declaration.Definition (RA.A _ (ASTf.Pattern.Record fields)) _ _ _)) -> fmap (\(ASTf.Commented _ f _) -> ASTf.Variable.Value f) fields
              ASTf.Declaration.Entry (RA.A _ (ASTf.Declaration.Datatype (ASTf.Commented _ (UppercaseIdentifier name, _) _) _)) -> [ ASTf.Variable.Union (UppercaseIdentifier name, []) (ASTf.Variable.OpenListing (ASTf.Commented [] () []))]
              ASTf.Declaration.Entry (RA.A _ (ASTf.Declaration.TypeAlias _ (ASTf.Commented _ (UppercaseIdentifier name, _) _) _)) -> [ ASTf.Variable.Union (UppercaseIdentifier name, []) ASTf.Variable.ClosedListing ]
              ASTf.Declaration.Entry (RA.A _ _) -> []

      formatModuleLine' header@(ASTf.Module.Header srcTag name moduleSettings exports) =
        let
            (preExposing, postExposing) =
                case exports of
                    Nothing -> ([], [])
                    Just (ASTf.KeywordCommented pre post _) -> (pre, post)
        in
        case elmVersion of
          Elm_0_16 ->
            formatModuleLine_0_16 header

          Elm_0_17 ->
            formatModuleLine elmVersion sortedExports srcTag name moduleSettings preExposing postExposing

          Elm_0_18 ->
            formatModuleLine elmVersion sortedExports srcTag name moduleSettings preExposing postExposing

          Elm_0_18_Upgrade ->
              formatModuleLine elmVersion sortedExports srcTag name moduleSettings preExposing postExposing

          Elm_0_19 ->
              formatModuleLine elmVersion sortedExports srcTag name moduleSettings preExposing postExposing

          Elm_0_19_Upgrade ->
              formatModuleLine elmVersion sortedExports srcTag name moduleSettings preExposing postExposing

      docs =
          fmap (formatDocComment elmVersion (makeImportInfo modu)) $ RA.drop $ ASTf.Module.docs modu

      imports =
          formatImports elmVersion modu
  in
  List.intercalate [ blankLine ] $ concat
      [ maybeToList $ fmap (return . formatModuleLine') maybeHeader
      , maybeToList $ fmap return docs
      , if null imports
          then []
          else [ imports ]
      ]


formatImports :: ElmVersion -> ASTf.Module.Module -> [Box]
formatImports elmVersion modu =
    let
        (comments, imports) =
            ASTf.Module.imports modu
    in
    [ formatComments comments
        |> maybeToList
    , imports
        |> Map.assocs
        |> fmap (\(name, (pre, method)) -> formatImport elmVersion ((pre, name), method))
    ]
        |> List.filter (not . List.null)
        |> List.intersperse [blankLine]
        |> concat


formatModuleLine_0_16 :: ASTf.Module.Header -> Box
formatModuleLine_0_16 header =
  let
    elmVersion = Elm_0_16

    exports =
        case ASTf.Module.exports header of
            Just (ASTf.KeywordCommented _ _ value) -> value
            Nothing -> ASTf.Variable.OpenListing (ASTf.Commented [] () [])

    formatExports =
        case formatListing (formatDetailedListing elmVersion) exports of
            Just listing ->
                listing
            _ ->
                pleaseReport "UNEXPECTED MODULE DECLARATION" "empty listing"

    (preWhere, postWhere) =
        case ASTf.Module.exports header of
            Nothing -> ([], [])
            Just (ASTf.KeywordCommented pre post _) -> (pre, post)

    whereClause =
        formatCommented (line . keyword) (ASTf.Commented preWhere "where" postWhere)
  in
    case
      ( formatCommented (line . formatQualifiedUppercaseIdentifier elmVersion) $ ASTf.Module.name header
      , formatExports
      , whereClause
      )
    of
      (SingleLine name', SingleLine exports', SingleLine where') ->
        line $ row
          [ keyword "module"
          , space
          , name'
          , row [ space, exports' ]
          , space
          , where'
          ]
      (name', exports', _) ->
        stack1
          [ line $ keyword "module"
          , indent $ name'
          , indent $ exports'
          , indent $ whereClause
          ]


formatModuleLine ::
    ElmVersion
    -> ([[ASTf.Commented ASTf.Variable.Value]], ASTf.Comments)
    -> ASTf.Module.SourceTag
    -> ASTf.Commented [UppercaseIdentifier]
    -> Maybe (ASTf.KeywordCommented ASTf.Module.SourceSettings)
    -> ASTf.Comments
    -> ASTf.Comments
    -> Box
formatModuleLine elmVersion (varsToExpose, extraComments) srcTag name moduleSettings preExposing postExposing =
  let
    tag =
      case srcTag of
        ASTf.Module.Normal ->
          line $ keyword "module"

        ASTf.Module.Port comments ->
          ElmStructure.spaceSepOrIndented
            (formatTailCommented (line . keyword) ("port", comments))
            [ line $ keyword "module" ]

        ASTf.Module.Effect comments ->
          ElmStructure.spaceSepOrIndented
            (formatTailCommented (line . keyword) ("effect", comments))
            [ line $ keyword "module" ]

    exports =
          case varsToExpose of
              [] -> line $ keyword "(..)"
              [oneGroup] ->
                  oneGroup
                      |> fmap (formatCommented $ formatVarValue elmVersion)
                      |> ElmStructure.group' False "(" "," (maybeToList (formatComments extraComments)) ")" False
              _ ->
                  varsToExpose
                      |> fmap (ElmStructure.group False "" "," "" False . fmap (formatCommented $ formatVarValue elmVersion))
                      |> ElmStructure.group' False "(" "," (maybeToList (formatComments extraComments)) ")" True

    formatSetting (k, v) =
      formatRecordPair elmVersion "=" (line . formatUppercaseIdentifier elmVersion) (k, v, False)

    formatSettings settings =
      map formatSetting settings
        |> ElmStructure.group True "{" "," "}" False

    whereClause =
      moduleSettings
        |> fmap (formatKeywordCommented "where" formatSettings)
        |> fmap (\x -> [x])
        |> Maybe.fromMaybe []

    nameClause =
      case
        ( tag
        , formatCommented (line . formatQualifiedUppercaseIdentifier elmVersion) name
        )
      of
        (SingleLine tag', SingleLine name') ->
          line $ row
            [ tag'
            , space
            , name'
            ]

        (tag', name') ->
          stack1
            [ tag'
            , indent $ name'
            ]
  in
  ElmStructure.spaceSepOrIndented
      (ElmStructure.spaceSepOrIndented
          nameClause
          (whereClause ++ [formatCommented (line . keyword) (ASTf.Commented preExposing "exposing" postExposing)])
      )
      [ exports ]


formatModule :: ElmVersion -> Bool -> Int -> ASTf.Module.Module -> Box
formatModule elmVersion addDefaultHeader spacing modu =
    let
        initialComments' =
          case ASTf.Module.initialComments modu of
            [] ->
              []
            comments ->
              (map formatComment comments)
                ++ [ blankLine, blankLine ]

        spaceBeforeBody =
            case ASTf.Module.body modu of
                [] -> 0
                ASTf.Declaration.BodyComment _ : _ -> spacing + 1
                _ -> spacing
    in
      stack1 $
          concat
              [ initialComments'
              , formatModuleHeader elmVersion addDefaultHeader modu
              , List.replicate spaceBeforeBody blankLine
              , maybeToList $ formatModuleBody spacing elmVersion (makeImportInfo modu) (ASTf.Module.body modu)
              ]


data ImportInfo =
    ImportInfo
        { _exposed :: Map.Map LowercaseIdentifier [UppercaseIdentifier]
        , _aliases :: Map.Map [UppercaseIdentifier] UppercaseIdentifier
        }


makeImportInfo :: ASTf.Module.Module -> ImportInfo
makeImportInfo modu =
    let
        -- these are things we know will get exposed for certain modules when we see "exposing (..)"
        -- only things that are currently useful for Elm 0.19 upgrade are included
        knownModuleContents :: Map.Map [UppercaseIdentifier] [LowercaseIdentifier]
        knownModuleContents =
            Map.fromList $
                fmap (\(a,b) -> (fmap UppercaseIdentifier a, fmap LowercaseIdentifier b))
                [ (["Html", "Attributes"], ["style"])
                ]

        exposed =
            -- currently this only checks for Html.Attributes (needed for Elm 0.19 upgrade)
            let
                importName = (fmap UppercaseIdentifier ["Html", "Attributes"])
            in
            case Map.lookup importName (snd $ ASTf.Module.imports modu) of
                Nothing -> mempty
                Just (_, importMethod) ->
                    case ASTf.Module.exposedVars importMethod of
                        (_, (_, ASTf.Variable.OpenListing _)) ->
                            -- import Html.Attributes [as ...] exposing (..)
                            Map.lookup importName knownModuleContents
                                |> Maybe.fromMaybe []
                                |> fmap (\n -> (n, importName))
                                |> Map.fromList

                        (_, (_, ASTf.Variable.ExplicitListing details _)) ->
                            -- import Html.Attributes [as ...] exposing (some, stuff)
                            ASTf.Module.values details
                                |> Map.keys
                                |> fmap (\n -> (n, importName))
                                |> Map.fromList

                        _ -> mempty

        aliases =
            -- currently this only checks for Html.Attributes (needed for Elm 0.19 upgrade)
            let
                importName = (fmap UppercaseIdentifier ["Html", "Attributes"])
            in
            case Map.lookup importName (snd $ ASTf.Module.imports modu) of
                Nothing -> mempty
                Just (_, importMethod) ->
                    case ASTf.Module.alias importMethod of
                        Just (_, (_, alias)) ->
                            Map.singleton importName alias

                        Nothing -> mempty
    in
    ImportInfo exposed aliases


formatModuleBody :: Int -> ElmVersion -> ImportInfo -> [TopLevelStructure Declaration] -> Maybe Box
formatModuleBody linesBetween elmVersion importInfo body =
    let
        entryType adecl =
            case adecl of
                ASTf.Declaration.Definition pat _ _ _ ->
                    case RA.drop pat of
                        ASTf.Pattern.VarPattern name ->
                            BodyNamed $ ASTf.Variable.VarRef [] name

                        ASTf.Pattern.OpPattern name ->
                            BodyNamed $ ASTf.Variable.OpRef name

                        _ ->
                            BodyUnnamed

                ASTf.Declaration.Datatype (ASTf.Commented _ (name, _) _) _ ->
                    BodyNamed $ ASTf.Variable.TagRef [] name

                ASTf.Declaration.TypeAlias _ (ASTf.Commented _ (name, _) _) _ ->
                    BodyNamed $ ASTf.Variable.TagRef [] name

                ASTf.Declaration.PortDefinition (ASTf.Commented _ name _) _ _ ->
                    BodyNamed $ ASTf.Variable.VarRef [] name

                ASTf.Declaration.TypeAnnotation (name, _) _ ->
                    BodyNamed name

                ASTf.Declaration.PortAnnotation (ASTf.Commented _ name _) _ _ ->
                    BodyNamed $ ASTf.Variable.VarRef [] name

                ASTf.Declaration.Fixity _ _ _ _ _ ->
                    BodyFixity

                ASTf.Declaration.Fixity_0_19 _ _ _ _ ->
                    BodyFixity
    in
    formatTopLevelBody linesBetween elmVersion importInfo entryType (formatDeclaration elmVersion importInfo) body


data BodyEntryType
    = BodyNamed ASTf.Variable.Ref
    | BodyUnnamed
    | BodyFixity


formatTopLevelBody ::
    Int
    -> ElmVersion
    -> ImportInfo
    -> (a -> BodyEntryType)
    -> (a -> Box)
    -> [TopLevelStructure a]
    -> Maybe Box
formatTopLevelBody linesBetween elmVersion importInfo entryType formatEntry body =
    let
        extraLines n =
            List.replicate n blankLine

        spacer first second =
            case (declarationType entryType first, declarationType entryType second) of
                (DStarter, _) -> 0
                (_, DCloser) -> 0
                (DComment, DComment) -> 0
                (_, DComment) -> if linesBetween == 1 then 1 else linesBetween + 1
                (DComment, DDefinition _) -> if linesBetween == 1 then 0 else linesBetween
                (DComment, _) -> linesBetween
                (DDocComment, DDefinition _) -> 0
                (DDefinition Nothing, DDefinition (Just _)) -> linesBetween
                (DDefinition _, DStarter) -> linesBetween
                (DDefinition Nothing, DDefinition Nothing) -> linesBetween
                (DDefinition a, DDefinition b) ->
                    if a == b
                        then 0
                        else linesBetween
                (DCloser, _) -> linesBetween
                (_, DDocComment) -> linesBetween
                (DDocComment, DStarter) -> 0
                (DFixity, DFixity) -> 0
                (DFixity, _) -> linesBetween
                (_, DFixity) -> linesBetween

        boxes =
            intersperseMap (\a b -> extraLines $ spacer a b)
                (formatTopLevelStructure elmVersion importInfo formatEntry)
                body
    in
        case boxes of
            [] -> Nothing
            _ -> Just $ stack1 boxes


data ElmCodeBlock
    = DeclarationsCode [TopLevelStructure Declaration]
    | ExpressionsCode [TopLevelStructure (WithEol ASTf.Expression.Expr)]
    | ModuleCode ASTf.Module.Module
    deriving (Show)


-- TODO: there must be an existing haskell function that does this, right?
firstOf :: [a -> Maybe b] -> a -> Maybe b
firstOf options value =
    case options of
        [] -> Nothing
        (next:rest) ->
            case next value of
                Just result -> Just result
                Nothing -> firstOf rest value


formatDocComment :: ElmVersion -> ImportInfo -> Markdown.Blocks -> Box
formatDocComment elmVersion importInfo blocks =
    let
        parse :: String -> Maybe ElmCodeBlock
        parse source =
            source
                |> firstOf
                    [ fmap DeclarationsCode . Result.toMaybe . Parse.parseDeclarations elmVersion
                    , fmap ExpressionsCode . Result.toMaybe . Parse.parseExpressions elmVersion
                    , fmap ModuleCode . Result.toMaybe . Parse.parseModule elmVersion
                    ]

        format :: ElmCodeBlock -> String
        format result =
            case result of
                ModuleCode modu ->
                    formatModule elmVersion False 1 modu
                        |> (Text.unpack . Box.render)

                DeclarationsCode declarations ->
                    formatModuleBody 1 elmVersion importInfo declarations
                        |> fmap (Text.unpack . Box.render)
                        |> fromMaybe ""

                ExpressionsCode expressions ->
                    let
                        entryType _ = BodyUnnamed
                    in
                    expressions
                        |> formatTopLevelBody 1 elmVersion importInfo entryType (formatEolCommented $ formatExpression elmVersion importInfo SyntaxSeparated)
                        |> fmap (Text.unpack . Box.render)
                        |> fromMaybe ""

        content :: String
        content =
            ElmFormat.Render.Markdown.formatMarkdown (fmap format . parse) $ fmap cleanBlock blocks

        cleanBlock :: Markdown.Block -> Markdown.Block
        cleanBlock block =
            case block of
                Markdown.ElmDocs docs ->
                    Markdown.ElmDocs $
                        (fmap . fmap)
                            (Text.replace (Text.pack "(..)") (Text.pack ""))
                            docs
                _ ->
                    block
    in
    formatDocCommentString content


formatDocCommentString :: String -> Box
formatDocCommentString docs =
    case lines docs of
        [] ->
            line $ row [ punc "{-|", space, punc "-}" ]
        (first:[]) ->
            stack1
                [ line $ row [ punc "{-|", space, literal first ]
                , line $ punc "-}"
                ]
        (first:rest) ->
            (line $ row [ punc "{-|", space, literal first ])
                |> andThen (map (line . literal) rest)
                |> andThen [ line $ punc "-}" ]


formatImport :: ElmVersion -> ASTf.Module.UserImport -> Box
formatImport elmVersion (name, method) =
    let
        as =
          (ASTf.Module.alias method)
            |> fmap (formatImportClause
            (Just . line . formatUppercaseIdentifier elmVersion)
            "as")
            |> Monad.join

        (exposingPreKeyword, exposingPostKeywordAndListing)
          = ASTf.Module.exposedVars method

        exposing =
          formatImportClause
            (formatListing (formatDetailedListing elmVersion))
            "exposing"
            (exposingPreKeyword, exposingPostKeywordAndListing)

        formatImportClause :: (a -> Maybe Box) -> String -> (ASTf.Comments, (ASTf.Comments, a)) -> Maybe Box
        formatImportClause format keyw input =
          case
            fmap (fmap format) $ input
          of
            ([], ([], Nothing)) ->
              Nothing

            (preKeyword, (postKeyword, Just listing')) ->
              case
                ( formatHeadCommented (line . keyword) (preKeyword, keyw)
                , formatHeadCommented id (postKeyword, listing')
                )
              of
                (SingleLine keyword', SingleLine listing'') ->
                  Just $ line $ row
                    [ keyword'
                    , space
                    , listing''
                    ]

                (keyword', listing'') ->
                  Just $ stack1
                    [ keyword'
                    , indent listing''
                    ]

            _ ->
              Just $ pleaseReport "UNEXPECTED IMPORT" "import clause comments with no clause"
    in
    case
        ( formatHeadCommented (line . formatQualifiedUppercaseIdentifier elmVersion) name
        , as
        , exposing
        )
    of
        ( SingleLine name', Just (SingleLine as'), Just (SingleLine exposing') ) ->
          line $ row
            [ keyword "import"
            , space
            , name'
            , space
            , as'
            , space
            , exposing'
            ]

        (SingleLine name', Just (SingleLine as'), Nothing) ->
          line $ row
            [ keyword "import"
            , space
            , name'
            , space
            , as'
            ]

        (SingleLine name', Nothing, Just (SingleLine exposing')) ->
          line $ row
            [ keyword "import"
            , space
            , name'
            , space
            , exposing'
            ]

        (SingleLine name', Nothing, Nothing) ->
          line $ row
            [ keyword "import"
            , space
            , name'
            ]

        ( SingleLine name', Just (SingleLine as'), Just exposing' ) ->
          stack1
            [ line $ row
              [ keyword "import"
              , space
              , name'
              , space
              , as'
              ]
            , indent exposing'
            ]

        ( SingleLine name', Just as', Just exposing' ) ->
          stack1
            [ line $ row
              [ keyword "import"
              , space
              , name'
              ]
            , indent as'
            , indent exposing'
            ]

        ( SingleLine name', Nothing, Just exposing' ) ->
          stack1
            [ line $ row
              [ keyword "import"
              , space
              , name'
              ]
            , indent exposing'
            ]

        ( name', Just as', Just exposing' ) ->
          stack1
            [ line $ keyword "import"
            , indent name'
            , indent $ indent as'
            , indent $ indent exposing'
            ]

        ( name', Nothing, Just exposing' ) ->
          stack1
            [ line $ keyword "import"
            , indent name'
            , indent $ indent exposing'
            ]

        ( name', Just as', Nothing ) ->
          stack1
            [ line $ keyword "import"
            , indent name'
            , indent $ indent as'
            ]

        ( name', Nothing, Nothing ) ->
          stack1
            [ line $ keyword "import"
            , indent name'
            ]


formatListing :: (a -> [Box]) -> ASTf.Variable.Listing a -> Maybe Box
formatListing format listing =
    case listing of
        ASTf.Variable.ClosedListing ->
            Nothing

        ASTf.Variable.OpenListing comments ->
            Just $ parens $ formatCommented (line . keyword) $ fmap (const "..") comments

        ASTf.Variable.ExplicitListing vars multiline ->
            Just $ ElmStructure.group False "(" "," ")" multiline $ format vars


formatDetailedListing :: ElmVersion -> ASTf.Module.DetailedListing -> [Box]
formatDetailedListing elmVersion listing =
    concat
        [ formatCommentedMap
            (\name () -> ASTf.Variable.OpValue name)
            (formatVarValue elmVersion)
            (ASTf.Module.operators listing)
        , formatCommentedMap
            (\name (inner, listing_) -> ASTf.Variable.Union (name, inner) listing_)
            (formatVarValue elmVersion)
            (ASTf.Module.types listing)
        , formatCommentedMap
            (\name () -> ASTf.Variable.Value name)
            (formatVarValue elmVersion)
            (ASTf.Module.values listing)
        ]


formatCommentedMap :: (k -> v -> a) -> (a -> Box) ->  ASTf.Variable.CommentedMap k v -> [Box]
formatCommentedMap construct format values =
    let
        format' (k, ASTf.Commented pre v post)
            = formatCommented format $ ASTf.Commented pre (construct k v) post
    in
    values
        |> Map.assocs
        |> map format'


formatVarValue :: ElmVersion -> ASTf.Variable.Value -> Box
formatVarValue elmVersion aval =
    case aval of
        ASTf.Variable.Value val ->
            line $ formatLowercaseIdentifier elmVersion [] val

        ASTf.Variable.OpValue (SymbolIdentifier name) ->
            line $ identifier $ "(" ++ name ++ ")"

        ASTf.Variable.Union name listing ->
            case
              ( formatListing
                  (formatCommentedMap
                      (\name_ () -> name_)
                      (line . formatUppercaseIdentifier elmVersion)
                  )
                  listing
              , formatTailCommented (line . formatUppercaseIdentifier elmVersion) name
              , snd name
              , elmVersion
              )
            of
                (Just _, _, _, Elm_0_19_Upgrade) ->
                    formatTailCommented
                        (\n -> line $ row [ formatUppercaseIdentifier elmVersion n, keyword "(..)" ])
                        name

                (Just (SingleLine listing'), SingleLine name', [], _) ->
                    line $ row
                        [ name'
                        , listing'
                        ]

                (Just (SingleLine listing'), SingleLine name', _, _) ->
                    line $ row
                        [ name'
                        , space
                        , listing'
                        ]

                (Just listing', name', _, _) ->
                  stack1
                    [ name'
                    , indent $ listing'
                    ]

                (Nothing, name', _, _) ->
                    name'


formatTopLevelStructure :: ElmVersion -> ImportInfo -> (a -> Box) -> TopLevelStructure a -> Box
formatTopLevelStructure elmVersion importInfo formatEntry topLevelStructure =
    case topLevelStructure of
        ASTf.Declaration.DocComment docs ->
            formatDocComment elmVersion importInfo docs

        ASTf.Declaration.BodyComment c ->
            formatComment c

        ASTf.Declaration.Entry entry ->
            formatEntry (RA.drop entry)


formatDeclaration :: ElmVersion -> ImportInfo -> Declaration -> Box
formatDeclaration elmVersion importInfo decl =
    case decl of
        ASTf.Declaration.Definition name args comments expr ->
            formatDefinition elmVersion importInfo name args comments expr

        ASTf.Declaration.TypeAnnotation name typ ->
            formatTypeAnnotation elmVersion name typ

        ASTf.Declaration.Datatype nameWithArgs tags ->
            let
                ctor (tag,args') =
                    case allSingles $ map (formatHeadCommented $ formatType' elmVersion ForCtor) args' of
                        Right args'' ->
                            line $ row $ List.intersperse space $ (formatUppercaseIdentifier elmVersion tag):args''
                        Left [] ->
                            line $ formatUppercaseIdentifier elmVersion tag
                        Left args'' ->
                            stack1
                                [ line $ formatUppercaseIdentifier elmVersion tag
                                , stack1 args''
                                    |> indent
                                ]
            in
                case
                    formatOpenCommentedList ctor tags
                of
                    [] -> error "List can't be empty"
                    first:rest ->
                        case formatCommented (formatNameWithArgs elmVersion) nameWithArgs of
                        SingleLine nameWithArgs' ->
                            stack1
                            [ line $ row
                                [ keyword "type"
                                , space
                                , nameWithArgs'
                                ]
                            , first
                                |> prefix (row [punc "=", space])
                                |> andThen (map (prefix (row [punc "|", space])) rest)
                                |> indent
                            ]
                        nameWithArgs' ->
                            stack1
                            [ line $ keyword "type"
                            , indent $ nameWithArgs'
                            , first
                                |> prefix (row [punc "=", space])
                                |> andThen (map (prefix (row [punc "|", space])) rest)
                                |> indent
                            ]

        ASTf.Declaration.TypeAlias preAlias nameWithArgs typ ->
            ElmStructure.definition "=" True
            (line $ keyword "type")
            [ formatHeadCommented (line . keyword) (preAlias, "alias")
            , formatCommented (formatNameWithArgs elmVersion) nameWithArgs
            ]
            (formatHeadCommentedStack (formatType elmVersion) typ)

        ASTf.Declaration.PortAnnotation name typeComments typ ->
            ElmStructure.definition ":" False
            (line $ keyword "port")
            [ formatCommented (line . formatLowercaseIdentifier elmVersion []) name ]
            (formatCommented' typeComments (formatType elmVersion) typ)

        ASTf.Declaration.PortDefinition name bodyComments expr ->
            ElmStructure.definition "=" True
            (line $ keyword "port")
            [formatCommented (line . formatLowercaseIdentifier elmVersion []) name]
            (formatCommented' bodyComments (formatExpression elmVersion importInfo SyntaxSeparated) expr)

        ASTf.Declaration.Fixity assoc precedenceComments precedence nameComments name ->
            case
                ( formatCommented' nameComments (line . formatInfixVar elmVersion) name
                , formatCommented' precedenceComments (line . literal . show) precedence
                )
            of
                (SingleLine name', SingleLine precedence') ->
                    line $ row
                        [ case assoc of
                                ASTf.Declaration.L -> keyword "infixl"
                                ASTf.Declaration.R -> keyword "infixr"
                                ASTf.Declaration.N -> keyword "infix"
                        , space
                        , precedence'
                        , space
                        , name'
                        ]
                _ ->
                    pleaseReport "TODO" "multiline fixity declaration"

        ASTf.Declaration.Fixity_0_19 assoc precedence name value ->
            let
                formatAssoc a =
                    case a of
                        ASTf.Declaration.L -> keyword "left "
                        ASTf.Declaration.R -> keyword "right"
                        ASTf.Declaration.N -> keyword "non  "
            in
            ElmStructure.spaceSepOrIndented
                (line $ keyword "infix")
                [ formatHeadCommented (line . formatAssoc) assoc
                , formatHeadCommented (line . literal . show) precedence
                , formatCommented (line . formatSymbolIdentifierInParens) name
                , line $ keyword "="
                , formatHeadCommented (line . identifier . formatVarName elmVersion) value
                ]


formatNameWithArgs :: ElmVersion -> (ASTf.UppercaseIdentifier, [(ASTf.Comments, ASTf.LowercaseIdentifier)]) -> Box
formatNameWithArgs elmVersion (name, args) =
  case allSingles $ map (formatHeadCommented (line . formatLowercaseIdentifier elmVersion [])) args of
    Right args' ->
      line $ row $ List.intersperse space $ ((formatUppercaseIdentifier elmVersion name):args')
    Left args' ->
      stack1 $
        [ line $ formatUppercaseIdentifier elmVersion name ]
        ++ (map indent args')


formatDefinition :: ElmVersion
    -> ImportInfo
    -> ASTf.Pattern.Pattern
    -> [(ASTf.Comments, ASTf.Pattern.Pattern)]
    -> [ASTf.Comment]
    -> ASTf.Expression.Expr
    -> Box
formatDefinition elmVersion importInfo name args comments expr =
  let
    body =
      stack1 $ concat
        [ map formatComment comments
        , [ formatExpression elmVersion importInfo SyntaxSeparated expr ]
        ]
  in
    ElmStructure.definition "=" True
      (formatPattern elmVersion True name)
      (map (\(x,y) -> formatCommented' x (formatPattern elmVersion True) y) args)
      body


formatTypeAnnotation :: ElmVersion -> (ASTf.Variable.Ref, ASTf.Comments) -> (ASTf.Comments, ASTf.Type) -> Box
formatTypeAnnotation elmVersion name typ =
  ElmStructure.definition ":" False
    (formatTailCommented (line . formatVar elmVersion) name)
    []
    (formatHeadCommented (formatType elmVersion) typ)


formatPattern :: ElmVersion -> Bool -> ASTf.Pattern.Pattern -> Box
formatPattern elmVersion parensRequired apattern =
    case RA.drop apattern of
        ASTf.Pattern.Anything ->
            line $ keyword "_"

        ASTf.Pattern.UnitPattern comments ->
            formatUnit '(' ')' comments

        ASTf.Pattern.Literal lit ->
            formatLiteral elmVersion lit

        ASTf.Pattern.VarPattern var ->
            line $ formatLowercaseIdentifier elmVersion [] var

        ASTf.Pattern.OpPattern (SymbolIdentifier name) ->
            line $ identifier $ "(" ++ name ++ ")"

        ASTf.Pattern.ConsPattern first rest ->
            let
                formatRight (preOp, postOp, term, eol) =
                    ( False
                    , preOp
                    , line $ punc "::"
                    , formatCommented
                        (formatEolCommented $ formatPattern elmVersion True)
                        (ASTf.Commented postOp (ASTf.WithEol term eol) [])
                    )
            in
                formatBinary False
                    (formatEolCommented (formatPattern elmVersion True) first)
                    (map formatRight rest)
                |> if parensRequired then parens else id

        ASTf.Pattern.Data ctor [] ->
            line (formatQualifiedUppercaseIdentifier elmVersion ctor)
                |>
                    case (elmVersion, ctor) of
                        (Elm_0_16, [_]) ->
                            id
                        (Elm_0_16, _) ->
                            if parensRequired then parens else id
                        _ ->
                            id

        ASTf.Pattern.Data ctor patterns ->
            ElmStructure.application
                (ASTf.FAJoinFirst ASTf.JoinAll)
                (line $ formatQualifiedUppercaseIdentifier elmVersion ctor)
                (map (formatHeadCommented $ formatPattern elmVersion True) patterns)
            |> if parensRequired then parens else id

        ASTf.Pattern.PatternParens pattern ->
            formatCommented (formatPattern elmVersion False) pattern
              |> parens

        ASTf.Pattern.Tuple patterns ->
            ElmStructure.group True "(" "," ")" False $ map (formatCommented $ formatPattern elmVersion False) patterns

        ASTf.Pattern.EmptyListPattern comments ->
            formatUnit '[' ']' comments

        ASTf.Pattern.List patterns ->
            ElmStructure.group True "[" "," "]" False $ map (formatCommented $ formatPattern elmVersion False) patterns

        ASTf.Pattern.EmptyRecordPattern comments ->
            formatUnit '{' '}' comments

        ASTf.Pattern.Record fields ->
            ElmStructure.group True "{" "," "}" False $ map (formatCommented $ line . formatLowercaseIdentifier elmVersion []) fields

        ASTf.Pattern.Alias pattern name ->
          case
            ( formatTailCommented (formatPattern elmVersion True) pattern
            , formatHeadCommented (line . formatLowercaseIdentifier elmVersion []) name
            )
          of
            (SingleLine pattern', SingleLine name') ->
              line $ row
                [ pattern'
                , space
                , keyword "as"
                , space
                , name'
                ]

            (pattern', name') ->
              stack1
                [ pattern'
                , line $ keyword "as"
                , indent name'
                ]

          |> (if parensRequired then parens else id)


formatRecordPair :: ElmVersion -> String -> (v -> Box) -> (ASTf.Commented ASTf.LowercaseIdentifier, ASTf.Commented v, Bool) -> Box
formatRecordPair elmVersion delim formatValue (ASTf.Commented pre k postK, v, forceMultiline) =
    ElmStructure.equalsPair delim forceMultiline
      (formatCommented (line . formatLowercaseIdentifier elmVersion []) $ ASTf.Commented [] k postK)
      (formatCommented formatValue v)
    |> (\x -> ASTf.Commented pre x []) |> formatCommented id


formatPair :: (a -> Line) -> String -> (b -> Box) -> ASTf.Pair a b -> Box
formatPair formatA delim formatB (ASTf.Pair a b (ASTf.ForceMultiline forceMultiline)) =
    ElmStructure.equalsPair delim forceMultiline
        (formatTailCommented (line . formatA) a)
        (formatHeadCommented formatB b)


negativeCasePatternWorkaround :: ASTf.Commented ASTf.Pattern.Pattern -> Box -> Box
negativeCasePatternWorkaround (ASTf.Commented _ (RA.A _ pattern) _) =
    case pattern of
        ASTf.Pattern.Literal (ASTf.IntNum i _) | i < 0 -> parens
        ASTf.Pattern.Literal (ASTf.FloatNum f _) | f < 0 -> parens
        _ -> id


data ExpressionContext
    = SyntaxSeparated
    | InfixSeparated
    | SpaceSeparated
    | AmbiguousEnd


expressionParens :: ExpressionContext -> ExpressionContext -> Box -> Box
expressionParens inner outer =
    case (inner, outer) of
        (SpaceSeparated, SpaceSeparated) -> parens
        (InfixSeparated, SpaceSeparated) -> parens
        (InfixSeparated, InfixSeparated) -> parens
        (AmbiguousEnd, SpaceSeparated) -> parens
        (AmbiguousEnd, InfixSeparated) -> parens
        (InfixSeparated, AmbiguousEnd) -> parens
        _ -> id


formatExpression :: ElmVersion -> ImportInfo -> ExpressionContext -> ASTf.Expression.Expr -> Box
formatExpression elmVersion importInfo context aexpr =
    case elmVersion of
        Elm_0_19_Upgrade -> formatExpression' elmVersion importInfo context (Upgrade_0_19.transform (_exposed importInfo) (_aliases importInfo) aexpr)
        _ -> formatExpression' elmVersion importInfo context aexpr


formatExpression' :: ElmVersion -> ImportInfo -> ExpressionContext -> ASTf.Expression.Expr -> Box
formatExpression' elmVersion importInfo context aexpr =
    case RA.drop aexpr of
        ASTf.Expression.Literal lit ->
            formatLiteral elmVersion lit

        ASTf.Expression.VarExpr v ->
            line $ formatVar elmVersion v

        ASTf.Expression.Range left right multiline ->
            case elmVersion of
                Elm_0_16 -> formatRange_0_17 elmVersion importInfo left right multiline
                Elm_0_17 -> formatRange_0_17 elmVersion importInfo left right multiline
                Elm_0_18 -> formatRange_0_17 elmVersion importInfo left right multiline
                Elm_0_19 -> formatRange_0_18 elmVersion importInfo context left right
                Elm_0_18_Upgrade -> formatRange_0_18 elmVersion importInfo context left right
                Elm_0_19_Upgrade -> formatRange_0_18 elmVersion importInfo context left right

        ASTf.Expression.ExplicitList exprs trailing multiline ->
            formatSequence '[' ',' (Just ']')
                (formatExpression elmVersion importInfo SyntaxSeparated)
                multiline
                trailing
                exprs

        ASTf.Expression.Binops left ops multiline ->
            case elmVersion of
                Elm_0_16 -> formatBinops_0_17 elmVersion importInfo left ops multiline
                Elm_0_17 -> formatBinops_0_17 elmVersion importInfo left ops multiline
                Elm_0_18 -> formatBinops_0_17 elmVersion importInfo left ops multiline
                Elm_0_19 -> formatBinops_0_17 elmVersion importInfo left ops multiline
                Elm_0_18_Upgrade -> formatBinops_0_18 elmVersion importInfo left ops multiline
                Elm_0_19_Upgrade -> formatBinops_0_19_upgrade elmVersion importInfo left ops multiline
            |> expressionParens InfixSeparated context

        ASTf.Expression.Lambda patterns bodyComments expr multiline ->
            case
                ( multiline
                , allSingles $ map (formatCommented (formatPattern elmVersion True) . (\(c,p) -> ASTf.Commented c p [])) patterns
                , bodyComments == []
                , formatExpression elmVersion importInfo SyntaxSeparated expr
                )
            of
                (False, Right patterns', True, SingleLine expr') ->
                    line $ row
                        [ punc "\\"
                        , row $ List.intersperse space $ patterns'
                        , space
                        , punc "->"
                        , space
                        , expr'
                        ]
                (_, Right patterns', _, expr') ->
                    stack1
                        [ line $ row
                            [ punc "\\"
                            , row $ List.intersperse space $ patterns'
                            , space
                            , punc "->"
                            ]
                        , indent $ stack1 $
                            (map formatComment bodyComments)
                            ++ [ expr' ]
                        ]
                (_, Left [], _, _) ->
                    pleaseReport "UNEXPECTED LAMBDA" "no patterns"
                (_, Left patterns', _, expr') ->
                    stack1
                        [ prefix (punc "\\") $ stack1 patterns'
                        , line $ punc "->"
                        , indent $ stack1 $
                            (map formatComment bodyComments)
                            ++ [ expr' ]
                        ]
            |> expressionParens AmbiguousEnd context

        ASTf.Expression.Unary ASTf.Expression.Negative e ->
            prefix (punc "-") $ formatExpression elmVersion importInfo SpaceSeparated e -- TODO: This might need something stronger than SpaceSeparated?

        ASTf.Expression.App left [] _ ->
            formatExpression elmVersion importInfo context left

        ASTf.Expression.App left args multiline ->
            ElmStructure.application
                multiline
                (formatExpression elmVersion importInfo InfixSeparated left)
                (fmap (formatPreCommentedExpression elmVersion importInfo SpaceSeparated) args)
                |> expressionParens SpaceSeparated context

        ASTf.Expression.If if' elseifs (elsComments, els) ->
            let
                opening key cond =
                    case (key, cond) of
                        (SingleLine key', SingleLine cond') ->
                            line $ row
                                [ key'
                                , space
                                , cond'
                                , space
                                , keyword "then"
                                ]
                        _ ->
                            stack1
                                [ key
                                , cond |> indent
                                , line $ keyword "then"
                                ]

                formatIf (cond, body) =
                    stack1
                        [ opening (line $ keyword "if") $ formatCommentedExpression elmVersion importInfo SyntaxSeparated cond
                        , indent $ formatCommented_ True (formatExpression elmVersion importInfo SyntaxSeparated) body
                        ]

                formatElseIf (ifComments, (cond, body)) =
                  let
                    key =
                      case (formatHeadCommented id (ifComments, line $ keyword "if")) of
                        SingleLine key' ->
                          line $ row [ keyword "else", space, key' ]
                        key' ->
                          stack1
                            [ line $ keyword "else"
                            , key'
                            ]
                  in
                    stack1
                      [ blankLine
                      , opening key $ formatCommentedExpression elmVersion importInfo SyntaxSeparated cond
                      , indent $ formatCommented_ True (formatExpression elmVersion importInfo SyntaxSeparated) body
                      ]
            in
                formatIf if'
                    |> andThen (map formatElseIf elseifs)
                    |> andThen
                        [ blankLine
                        , line $ keyword "else"
                        , indent $ formatCommented_ True (formatExpression elmVersion importInfo SyntaxSeparated) (ASTf.Commented elsComments els [])
                        ]
                    |> expressionParens AmbiguousEnd context

        ASTf.Expression.Let defs bodyComments expr ->
            let
                spacer first _ =
                    case first of
                        ASTf.Expression.LetDefinition _ _ _ _ ->
                            [ blankLine ]
                        _ ->
                            []

                formatDefinition' def =
                  case def of
                    ASTf.Expression.LetDefinition name args comments expr' ->
                      formatDefinition elmVersion importInfo name args comments expr'

                    ASTf.Expression.LetAnnotation name typ ->
                      formatTypeAnnotation elmVersion name typ

                    ASTf.Expression.LetComment comment ->
                        formatComment comment
            in
                (line $ keyword "let")
                    |> andThen
                        (defs
                            |> intersperseMap spacer formatDefinition'
                            |> map indent
                        )
                    |> andThen
                        [ line $ keyword "in"
                        , stack1 $
                            (map formatComment bodyComments)
                            ++ [formatExpression elmVersion importInfo SyntaxSeparated expr]
                        ]
                    |> expressionParens AmbiguousEnd context -- TODO: not tested

        ASTf.Expression.Case (subject,multiline) clauses ->
            let
                opening =
                  case
                    ( multiline
                    , formatCommentedExpression elmVersion importInfo SyntaxSeparated subject
                    )
                  of
                      (False, SingleLine subject') ->
                          line $ row
                              [ keyword "case"
                              , space
                              , subject'
                              , space
                              , keyword "of"
                              ]
                      (_, subject') ->
                          stack1
                              [ line $ keyword "case"
                              , indent subject'
                              , line $ keyword "of"
                              ]

                clause (pat, expr) =
                    case
                      ( pat
                      , (formatPattern elmVersion False $ (\(ASTf.Commented _ x _) -> x) pat)
                          |> negativeCasePatternWorkaround pat
                      , formatCommentedStack (formatPattern elmVersion False) pat
                          |> negativeCasePatternWorkaround pat
                      , formatHeadCommentedStack (formatExpression elmVersion importInfo SyntaxSeparated) expr
                      )
                    of
                        (_, _, SingleLine pat', body') ->
                            stack1
                                [ line $ row [ pat', space, keyword "->"]
                                , indent body'
                                ]
                        (ASTf.Commented pre _ [], SingleLine pat', _, body') ->
                            stack1 $
                                (map formatComment pre)
                                ++ [ line $ row [ pat', space, keyword "->"]
                                   , indent body'
                                   ]
                        (_, _, pat', body') ->
                            stack1 $
                              [ pat'
                              , line $ keyword "->"
                              , indent body'
                              ]
            in
                opening
                    |> andThen
                        (clauses
                            |> map clause
                            |> List.intersperse blankLine
                            |> map indent
                        )
                    |> expressionParens AmbiguousEnd context -- TODO: not tested

        ASTf.Expression.Tuple exprs multiline ->
            ElmStructure.group True "(" "," ")" multiline $ map (formatCommentedExpression elmVersion importInfo SyntaxSeparated) exprs

        ASTf.Expression.TupleFunction n ->
            line $ keyword $ "(" ++ (List.replicate (n-1) ',') ++ ")"

        ASTf.Expression.Access expr field ->
            formatExpression elmVersion importInfo SpaceSeparated expr -- TODO: does this need a different context than SpaceSeparated?
                |> addSuffix (row $ [punc ".", formatLowercaseIdentifier elmVersion [] field])

        ASTf.Expression.AccessFunction (ASTf.LowercaseIdentifier field) ->
            line $ identifier $ "." ++ (formatVarName' elmVersion field)

        ASTf.Expression.Record base fields trailing multiline ->
            formatRecordLike
                (line . formatLowercaseIdentifier elmVersion [])
                (formatLowercaseIdentifier elmVersion [])
                "="
                (formatExpression elmVersion importInfo SyntaxSeparated)
                base fields trailing multiline

        ASTf.Expression.Parens expr ->
            case expr of
                ASTf.Commented [] expr' [] ->
                    formatExpression elmVersion importInfo context expr'

                _ ->
                    formatCommentedExpression elmVersion importInfo SyntaxSeparated expr
                        |> parens


        ASTf.Expression.Unit comments ->
            formatUnit '(' ')' comments

        ASTf.Expression.GLShader src ->
          line $ row
            [ punc "[glsl|"
            , literal $ src
            , punc "|]"
            ]


formatCommentedExpression :: ElmVersion -> ImportInfo -> ExpressionContext -> ASTf.Commented ASTf.Expression.Expr -> Box
formatCommentedExpression elmVersion importInfo context (ASTf.Commented pre e post) =
    let
        commented' =
            case RA.drop e of
                ASTf.Expression.Parens (ASTf.Commented pre'' e'' post'') ->
                    ASTf.Commented (pre ++ pre'') e'' (post'' ++ post)
                _ -> ASTf.Commented pre e post
    in
    formatCommented (formatExpression elmVersion importInfo context) commented'


formatPreCommentedExpression :: ElmVersion -> ImportInfo -> ExpressionContext -> ASTf.PreCommented ASTf.Expression.Expr -> Box
formatPreCommentedExpression elmVersion importInfo context (pre, e) =
    let
        (pre', e') =
            case RA.drop e of
                ASTf.Expression.Parens (ASTf.Commented pre'' e'' []) ->
                    (pre ++ pre'', e'')
                _ -> (pre, e)
    in
    formatCommented' pre' (formatExpression elmVersion importInfo context) e'


formatRecordLike ::
    (base -> Box) -> (key -> Line) -> String -> (value -> Box)
    -> Maybe (ASTf.Commented base) -> ASTf.Sequence (ASTf.Pair key value)-> ASTf.Comments -> ASTf.ForceMultiline
    -> Box
formatRecordLike formatBase formatKey fieldSep formatValue base' fields trailing multiline =
    case (base', fields) of
      ( Just base, pairs' ) ->
          ElmStructure.extensionGroup'
              ((\(ASTf.ForceMultiline b) -> b) multiline)
              (formatCommented formatBase base)
              (formatSequence '|' ',' Nothing
                  (formatPair formatKey fieldSep formatValue)
                  multiline
                  trailing
                  pairs')

      ( Nothing, pairs' ) ->
          formatSequence '{' ',' (Just '}')
              (formatPair formatKey fieldSep formatValue)
              multiline
              trailing
              pairs'


formatSequence :: Char -> Char -> Maybe Char -> (a -> Box) -> ASTf.ForceMultiline -> ASTf.Comments -> ASTf.Sequence a -> Box
formatSequence left delim right formatA (ASTf.ForceMultiline multiline) trailing (first:rest) =
    let
        formatItem delim_ (pre, item) =
            maybe id (stack' . stack' blankLine) (formatComments pre) $
            prefix (row [ punc [delim_], space ]) $
            formatHeadCommented (formatEolCommented formatA) item
    in
        ElmStructure.forceableSpaceSepOrStack multiline
            (ElmStructure.forceableRowOrStack multiline
                (formatItem left first)
                (map (formatItem delim) rest)
            )
            (maybe [] (flip (:) [] . stack' blankLine) (formatComments trailing) ++ (Maybe.maybeToList $ fmap (line . punc . flip (:) []) right))
formatSequence left _ (Just right) _ _ trailing [] =
    formatUnit left right trailing
formatSequence left _ Nothing _ _ trailing [] =
    formatUnit left ' ' trailing


mapIsLast :: (Bool -> a -> b) -> [a] -> [b]
mapIsLast _ [] = []
mapIsLast f (last_:[]) = f True last_ : []
mapIsLast f (next:rest) = f False next : mapIsLast f rest


formatBinops_0_17 ::
    ElmVersion
    -> ImportInfo
    -> ASTf.Expression.Expr
    -> [(ASTf.Comments, ASTf.Variable.Ref, ASTf.Comments, ASTf.Expression.Expr)]
    -> Bool
    -> Box
formatBinops_0_17 elmVersion importInfo left ops multiline =
    formatBinops_common (,) elmVersion importInfo left ops multiline


formatBinops_0_18 ::
    ElmVersion
    -> ImportInfo
    -> ASTf.Expression.Expr
    -> [(ASTf.Comments, ASTf.Variable.Ref, ASTf.Comments, ASTf.Expression.Expr)]
    -> Bool
    -> Box
formatBinops_0_18 elmVersion importInfo left ops multiline =
    formatBinops_common removeBackticks elmVersion importInfo left ops multiline


formatBinops_0_19_upgrade ::
    ElmVersion
    -> ImportInfo
    -> ASTf.Expression.Expr
    -> [(ASTf.Comments, ASTf.Variable.Ref, ASTf.Comments, ASTf.Expression.Expr)]
    -> Bool
    -> Box
formatBinops_0_19_upgrade elmVersion importInfo left ops multiline =
    let
        transform = uncurry removeBangs . uncurry removeMod
    in
    formatBinops_common (curry transform) elmVersion importInfo left ops multiline


formatBinops_common ::
    (ASTf.Expression.Expr
        -> [(ASTf.Comments, ASTf.Variable.Ref, ASTf.Comments, ASTf.Expression.Expr)]
        -> ( ASTf.Expression.Expr
           , [(ASTf.Comments, ASTf.Variable.Ref, ASTf.Comments, ASTf.Expression.Expr)]
           )
    )
    -> ElmVersion
    -> ImportInfo
    -> ASTf.Expression.Expr
    -> [(ASTf.Comments, ASTf.Variable.Ref, ASTf.Comments, ASTf.Expression.Expr)]
    -> Bool
    -> Box
formatBinops_common transform elmVersion importInfo left ops multiline =
    let
        (left', ops') = transform left ops

        formatPair_ isLast ( po, o, pe, e ) =
            let
                isLeftPipe =
                    o == ASTf.Variable.OpRef (SymbolIdentifier "<|")

                formatContext =
                    if isLeftPipe && isLast
                        then AmbiguousEnd
                        else InfixSeparated
            in
            ( isLeftPipe
            , po
            , (line . formatInfixVar elmVersion) o
            , formatCommented' pe (formatExpression elmVersion importInfo formatContext) e
            )
    in
        formatBinary
            multiline
            (formatExpression elmVersion importInfo InfixSeparated left')
            (mapIsLast formatPair_ ops')


removeBackticks ::
    ASTf.Expression.Expr
    -> [(ASTf.Comments, ASTf.Variable.Ref, ASTf.Comments, ASTf.Expression.Expr)]
    -> (ASTf.Expression.Expr, [(ASTf.Comments, ASTf.Variable.Ref, ASTf.Comments, ASTf.Expression.Expr)])
removeBackticks left ops =
    case ops of
        [] -> (left, ops)
        (pre, ASTf.Variable.VarRef v' v, post, e):rest
          | v == ASTf.LowercaseIdentifier "andThen" || v == ASTf.LowercaseIdentifier "onError"
          ->
            -- Convert `andThen` to |> andThen
            let
                e' = noRegion $ ASTf.Expression.App
                    (noRegion $ ASTf.Expression.VarExpr $ ASTf.Variable.VarRef v' v)
                    [ (post, e)
                    ]
                    (ASTf.FAJoinFirst ASTf.JoinAll)

                (e'', rest') = removeBackticks e' rest
            in
                (left, (pre, ASTf.Variable.OpRef $ SymbolIdentifier "|>", [], e''):rest')

        (pre, ASTf.Variable.VarRef v' v, post, e):rest ->
            -- Convert other backtick operators to normal function application
            removeBackticks
                (noRegion $ ASTf.Expression.App
                    (noRegion $ ASTf.Expression.VarExpr $ ASTf.Variable.VarRef v' v)
                    [ (pre, left)
                    , (post, e)
                    ]
                    (ASTf.FAJoinFirst ASTf.JoinAll)
                )
                rest

        (pre, op, post, e):rest ->
            -- Preserve symbolic infix operators
            let
                (e', rest') = removeBackticks e rest
            in
                (left, (pre, op, post, e'):rest')


removeBangs ::
    ASTf.Expression.Expr
    -> [(ASTf.Comments, ASTf.Variable.Ref, ASTf.Comments, ASTf.Expression.Expr)]
    -> (ASTf.Expression.Expr, [(ASTf.Comments, ASTf.Variable.Ref, ASTf.Comments, ASTf.Expression.Expr)])
removeBangs left ops =
    let
        cmds' post cmds =
            case RA.drop cmds of
                ASTf.Expression.ExplicitList [] innerComments _ ->
                    ASTf.Commented
                        post
                        (noRegion $ ASTf.Expression.VarExpr (ASTf.Variable.VarRef [ASTf.UppercaseIdentifier "Cmd"] (ASTf.LowercaseIdentifier "none")))
                        innerComments

                ASTf.Expression.ExplicitList [(extraPre, (pre', ASTf.WithEol cmd eol))] trailing _ ->
                    let
                        eolComment =
                            case eol of
                                Nothing -> []
                                Just c -> [ASTf.LineComment c]
                    in
                    ASTf.Commented (post ++ extraPre ++ pre') cmd (eolComment ++ trailing)

                _ ->
                    ASTf.Commented [] (noRegion $ ASTf.Expression.App
                        (noRegion $ ASTf.Expression.VarExpr (ASTf.Variable.VarRef [ASTf.UppercaseIdentifier "Cmd"] (ASTf.LowercaseIdentifier "batch")))
                        [(post, cmds)]
                        (ASTf.FAJoinFirst ASTf.JoinAll)
                      )
                      []

        tuple left' pre post cmds =
            noRegion $ ASTf.Expression.Tuple
                [ ASTf.Commented [] left' pre
                , cmds' post cmds
                ]
                True

        shouldFold opi =
            case opi of
                ">>" -> FoldIfLeft
                _ -> NeverFold
    in
    binopToFunction (SymbolIdentifier "!") shouldFold tuple left ReversedList.empty ops


removeMod ::
    ASTf.Expression.Expr
    -> [(ASTf.Comments, ASTf.Variable.Ref, ASTf.Comments, ASTf.Expression.Expr)]
    -> (ASTf.Expression.Expr, [(ASTf.Comments, ASTf.Variable.Ref, ASTf.Comments, ASTf.Expression.Expr)])
removeMod left ops =
    let
        tuple left' pre post right' =
            noRegion $ ASTf.Expression.App
                (noRegion $ ASTf.Expression.VarExpr $ ASTf.Variable.OpRef (SymbolIdentifier "%"))
                [ (pre, left')
                , (post, right')
                ]
                (ASTf.FAJoinFirst ASTf.JoinAll)


        shouldFold opi =
            case opi of
                ">>" -> AlwaysFold
                "<<" -> AlwaysFold
                "^" -> AlwaysFold
                _ -> NeverFold
    in
    binopToFunction (SymbolIdentifier "%") shouldFold tuple left ReversedList.empty ops


data ShouldFold
    = AlwaysFold
    | FoldIfLeft
    | NeverFold


collectRightFold :: ShouldFold -> Bool
collectRightFold AlwaysFold = True
collectRightFold FoldIfLeft = False
collectRightFold NeverFold = False


collectLeftFold :: ShouldFold -> Bool
collectLeftFold AlwaysFold = True
collectLeftFold FoldIfLeft = True
collectLeftFold NeverFold = False


binopToFunction ::
    SymbolIdentifier
    -> (String -> ShouldFold)
    -> (ASTf.Expression.Expr -> ASTf.Comments -> ASTf.Comments -> ASTf.Expression.Expr -> ASTf.Expression.Expr)
    -> ASTf.Expression.Expr
    -> Reversed (ASTf.Comments, ASTf.Variable.Ref, ASTf.Comments, ASTf.Expression.Expr)
    -> [(ASTf.Comments, ASTf.Variable.Ref, ASTf.Comments, ASTf.Expression.Expr)]
    -> (ASTf.Expression.Expr, [(ASTf.Comments, ASTf.Variable.Ref, ASTf.Comments, ASTf.Expression.Expr)])
binopToFunction target shouldFold applyFn left preBang remaining =
    case remaining of
        [] ->
            (left, ReversedList.toList preBang)

        (pre, ASTf.Variable.OpRef sym, post, cmds):rest | sym == target ->
            let
                left' =
                    case ReversedList.isEmpty preBang of
                        True -> left
                        False -> ( noRegion $ ASTf.Expression.Binops left (ReversedList.toList preBang) False)

                (right', rest') =
                    collectRight (collectRightFold . shouldFold) cmds ReversedList.empty rest

                tuple =
                    applyFn left' pre post right'
            in
            binopToFunction target shouldFold applyFn tuple ReversedList.empty rest'

        (pre, op@(ASTf.Variable.OpRef (SymbolIdentifier opi)), post, e):rest | collectLeftFold $ shouldFold opi ->
            binopToFunction target shouldFold applyFn left (ReversedList.push (pre, op, post, e) preBang) rest

        (pre, op, post, e):rest ->
            let
                (e', rest') = binopToFunction target shouldFold applyFn e ReversedList.empty rest
            in
            (left, ReversedList.toList preBang ++ (pre, op, post, e'):rest')

collectRight ::
    (String -> Bool)
    -> ASTf.Expression.Expr
    -> Reversed (ASTf.Comments, ASTf.Variable.Ref, ASTf.Comments, ASTf.Expression.Expr)
    -> [(ASTf.Comments, ASTf.Variable.Ref, ASTf.Comments, ASTf.Expression.Expr)]
    -> (ASTf.Expression.Expr, [(ASTf.Comments, ASTf.Variable.Ref, ASTf.Comments, ASTf.Expression.Expr)])
collectRight shouldFold leftMost collectedLeft rest =
    case rest of
        (pre, op@(ASTf.Variable.OpRef (SymbolIdentifier opi)), post, e):rest' | shouldFold opi ->
            collectRight shouldFold leftMost (ReversedList.push (pre, op, post, e) collectedLeft) rest'

        _ ->
            -- terminate if either rest is empty, or the next op fails the shouldFold test
            ( case ReversedList.isEmpty collectedLeft of
                True -> leftMost
                False -> noRegion $ ASTf.Expression.Binops leftMost (ReversedList.toList collectedLeft) False
            , rest
            )

formatRange_0_17 :: ElmVersion -> ImportInfo -> ASTf.Commented ASTf.Expression.Expr -> ASTf.Commented ASTf.Expression.Expr -> Bool -> Box
formatRange_0_17 elmVersion importInfo left right multiline =
    case
        ( multiline
        , formatCommentedExpression elmVersion importInfo SyntaxSeparated left
        , formatCommentedExpression elmVersion importInfo SyntaxSeparated right
        )
    of
        (False, SingleLine left', SingleLine right') ->
            line $ row
                [ punc "["
                , left'
                , punc ".."
                , right'
                , punc "]"
                ]
        (_, left', right') ->
            stack1
                [ line $ punc "["
                , indent left'
                , line $ punc ".."
                , indent right'
                , line $ punc "]"
                ]

nowhere :: Region.Position
nowhere =
    Region.Position 0 0


noRegion :: a -> RA.Located a
noRegion =
    RA.at nowhere nowhere

formatRange_0_18 :: ElmVersion -> ImportInfo -> ExpressionContext -> ASTf.Commented ASTf.Expression.Expr -> ASTf.Commented ASTf.Expression.Expr -> Box
formatRange_0_18 elmVersion importInfo context left right =
    case (left, right) of
        (ASTf.Commented preLeft left' [], ASTf.Commented preRight right' []) ->
            ASTf.Expression.App
                (noRegion $ ASTf.Expression.VarExpr $ ASTf.Variable.VarRef [ASTf.UppercaseIdentifier "List"] $ ASTf.LowercaseIdentifier "range")
                [ (preLeft, left')
                , (preRight, right')
                ]
                (ASTf.FAJoinFirst ASTf.JoinAll)
                |> noRegion
                |> formatExpression elmVersion importInfo context

        _ ->
            ASTf.Expression.App
                (noRegion $ ASTf.Expression.VarExpr $ ASTf.Variable.VarRef [ASTf.UppercaseIdentifier "List"] $ ASTf.LowercaseIdentifier "range")
                [ ([], noRegion $ ASTf.Expression.Parens left)
                , ([], noRegion $ ASTf.Expression.Parens right)
                ]
                (ASTf.FAJoinFirst ASTf.JoinAll)
                |> noRegion
                |> formatExpression elmVersion importInfo context


formatUnit :: Char -> Char -> ASTf.Comments -> Box
formatUnit left right comments =
  case (left, comments) of
    (_, []) ->
      line $ punc (left : right : [])

    ('{', (ASTf.LineComment _):_) ->
      surround left right $ prefix space $ stack1 $ map formatComment comments

    _ ->
      surround left right $
        case allSingles $ map formatComment comments of
          Right comments' ->
            line $ row $ List.intersperse space comments'

          Left comments' ->
            stack1 comments'


formatComments :: ASTf.Comments -> Maybe Box
formatComments comments =
    case fmap formatComment comments of
        [] ->
            Nothing

        (first:rest) ->
            Just $ ElmStructure.spaceSepOrStack first rest


formatCommented_ :: Bool -> (a -> Box) -> ASTf.Commented a -> Box
formatCommented_ forceMultiline format (ASTf.Commented pre inner post) =
    ElmStructure.forceableSpaceSepOrStack1 forceMultiline $
        concat
            [ Maybe.maybeToList $ formatComments pre
            , [format inner]
            , Maybe.maybeToList $ formatComments post
            ]


formatCommented :: (a -> Box) -> ASTf.Commented a -> Box
formatCommented =
  formatCommented_ False


-- TODO: rename to formatPreCommented
formatHeadCommented :: (a -> Box) -> (ASTf.Comments, a) -> Box
formatHeadCommented format (pre, inner) =
    formatCommented' pre format inner


formatCommented' :: ASTf.Comments -> (a -> Box) -> a -> Box
formatCommented' pre format inner =
    formatCommented format (ASTf.Commented pre inner [])


formatTailCommented :: (a -> Box) -> (a, ASTf.Comments) -> Box
formatTailCommented format (inner, post) =
  formatCommented format (ASTf.Commented [] inner post)


formatEolCommented :: (a -> Box) -> ASTf.WithEol a -> Box
formatEolCommented format (ASTf.WithEol inner post) =
  case (post, format inner) of
    (Nothing, box) -> box
    (Just eol, SingleLine result) ->
      mustBreak $ row [ result, space, punc "--", literal eol ]
    (Just eol, box) ->
      stack1 [ box, formatComment $ ASTf.LineComment eol ]


formatCommentedStack :: (a -> Box) -> ASTf.Commented a -> Box
formatCommentedStack format (ASTf.Commented pre inner post) =
  stack1 $
    (map formatComment pre)
      ++ [ format inner ]
      ++ (map formatComment post)


formatHeadCommentedStack :: (a -> Box) -> (ASTf.Comments, a) -> Box
formatHeadCommentedStack format (pre, inner) =
  formatCommentedStack format (ASTf.Commented pre inner [])


formatKeywordCommented :: String -> (a -> Box) -> ASTf.KeywordCommented a -> Box
formatKeywordCommented word format (ASTf.KeywordCommented pre post value) =
  ElmStructure.spaceSepOrIndented
    (formatCommented (line . keyword) (ASTf.Commented pre word post))
    [ format value ]


formatOpenCommentedList :: (a -> Box) -> ASTf.OpenCommentedList a -> [Box]
formatOpenCommentedList format (ASTf.OpenCommentedList rest (preLst, lst)) =
    (fmap (formatCommented $ formatEolCommented format) rest)
        ++ [formatCommented (formatEolCommented format) $ ASTf.Commented preLst lst []]


formatComment :: ASTf.Comment -> Box
formatComment comment =
    case comment of
        ASTf.BlockComment c ->
            case c of
                [] ->
                    line $ punc "{- -}"
                [l] ->
                    line $ row
                        [ punc "{-"
                        , space
                        , literal l
                        , space
                        , punc "-}"
                        ]
                ls ->
                    stack1
                        [ prefix
                            (row [ punc "{-", space ])
                            (stack1 $ map (line . literal) ls)
                        , line $ punc "-}"
                        ]

        ASTf.LineComment c ->
            mustBreak $ row [ punc "--", literal c ]

        ASTf.CommentTrickOpener ->
            mustBreak $ punc "{--}"

        ASTf.CommentTrickCloser ->
            mustBreak $ punc "--}"

        ASTf.CommentTrickBlock c ->
            mustBreak $ row [ punc "{--", literal c, punc "-}" ]


formatLiteral :: ElmVersion -> ASTf.Literal -> Box
formatLiteral elmVersion lit =
    case lit of
        ASTf.IntNum i ASTf.DecimalInt ->
            line $ literal $ show i
        ASTf.IntNum i ASTf.HexadecimalInt ->
            line $ literal $
              if i <= 0xFF then
                printf "0x%02X" i
              else if i <= 0xFFFF then
                printf "0x%04X" i
              else if i <= 0xFFFFFFFF then
                printf "0x%08X" i
              else
                printf "0x%016X" i
        ASTf.FloatNum f ASTf.DecimalFloat ->
            line $ literal $ printf "%f" f
        ASTf.FloatNum f ASTf.ExponentFloat ->
            line $ literal $ printf "%e" f
        ASTf.Chr c ->
            formatString elmVersion SChar [c]
        ASTf.Str s multi ->
            formatString elmVersion (if multi then SMulti else SString) s
        ASTf.Boolean b ->
            line $ literal $ show b


data StringStyle
    = SChar
    | SString
    | SMulti
    deriving (Eq)


formatString :: ElmVersion -> StringStyle -> String -> Box
formatString elmVersion style s =
  case style of
      SChar ->
        stringBox "\'" id
      SString ->
        stringBox "\"" id
      SMulti ->
        stringBox "\"\"\"" escapeMultiQuote
  where
    stringBox quotes escaper =
      line $ row
          [ punc quotes
          , literal $ escaper $ concatMap fix s
          , punc quotes
          ]

    fix c =
        if (style == SMulti) && c == '\n' then
            [c]
        else if c == '\n' then
            "\\n"
        else if c == '\t' then
            "\\t"
        else if c == '\\' then
            "\\\\"
        else if (style == SString) && c == '\"' then
            "\\\""
        else if (style == SChar) && c == '\'' then
            "\\\'"
        else if not $ Char.isPrint c then
            hex c
        else if c == ' ' then
            [c]
        else if ElmVersion.style_0_19_stringEscape elmVersion == False && c == '\xA0' then
            [c] -- Workaround for https://github.com/elm-lang/elm-compiler/issues/1279
        else if Char.isSpace c then
            hex c
        else
            [c]

    hex char =
      case ElmVersion.style_0_19_stringEscape elmVersion of
          True ->
              "\\u{" ++ (printf "%04X" $ Char.ord char) ++ "}"
          False ->
              "\\x" ++ (printf fmt $ Char.ord char)
      where
        fmt =
          if Char.ord char <= 0xFF then
            "%02X"
          else
            "%04X"

    escapeMultiQuote =
        let
            step okay quotes remaining =
                case remaining of
                    [] ->
                        reverse $ (concat $ replicate quotes "\"\\") ++ okay

                    next : rest ->
                        if next == '"' then
                            step okay (quotes + 1) rest
                        else if quotes >= 3 then
                            step (next : (concat $ replicate quotes "\"\\") ++ okay) 0 rest
                        else if quotes > 0 then
                            step (next : (replicate quotes '"') ++ okay) 0 rest
                        else
                            step (next : okay) 0 rest
        in
            step "" 0



data TypeParensRequired
    = ForLambda
    | ForCtor
    | NotRequired
    deriving (Eq)


formatType :: ElmVersion -> ASTf.Type -> Box
formatType elmVersion =
    formatType' elmVersion NotRequired


commaSpace :: Line
commaSpace =
    row
        [ punc ","
        , space
        ]


formatTypeConstructor :: ElmVersion -> ASTf.TypeConstructor -> Box
formatTypeConstructor elmVersion ctor =
    case ctor of
        ASTf.NamedConstructor namespace name ->
            line $ formatQualifiedUppercaseIdentifier elmVersion (namespace ++ [name])

        ASTf.TupleConstructor n ->
            line $ keyword $ "(" ++ (List.replicate (n-1) ',') ++ ")"


formatType' :: ElmVersion -> TypeParensRequired -> ASTf.Type -> Box
formatType' elmVersion requireParens atype =
    case RA.drop atype of
        ASTf.UnitType comments ->
          formatUnit '(' ')' comments

        ASTf.FunctionType first rest (ASTf.ForceMultiline forceMultiline) ->
            let
                formatRight (preOp, postOp, term, eol) =
                    ElmStructure.forceableSpaceSepOrStack1
                        False
                        $ concat
                            [ Maybe.maybeToList $ formatComments preOp
                            , [ ElmStructure.prefixOrIndented
                                  (line $ punc "->")
                                  (formatCommented
                                      (formatEolCommented $ formatType' elmVersion ForLambda)
                                      (ASTf.Commented postOp (ASTf.WithEol term eol) [])
                                  )
                              ]
                            ]
            in
                ElmStructure.forceableSpaceSepOrStack
                    forceMultiline
                    (formatEolCommented (formatType' elmVersion ForLambda) first)
                    (map formatRight rest)
                |> if requireParens /= NotRequired then parens else id

        ASTf.TypeVariable var ->
            line $ identifier $ formatVarName elmVersion var

        ASTf.TypeConstruction ctor args ->
            ElmStructure.application
                (ASTf.FAJoinFirst ASTf.JoinAll)
                (formatTypeConstructor elmVersion ctor)
                (map (formatHeadCommented $ formatType' elmVersion ForCtor) args)
                |> (if args /= [] && requireParens == ForCtor then parens else id)

        ASTf.TypeParens type' ->
          parens $ formatCommented (formatType elmVersion) type'

        ASTf.TupleType types ->
          ElmStructure.group True "(" "," ")" False (map (formatCommented (formatEolCommented $ formatType elmVersion)) types)

        ASTf.RecordType base fields trailing multiline ->
            formatRecordLike
                (line . formatLowercaseIdentifier elmVersion [])
                (formatLowercaseIdentifier elmVersion [])
                ":"
                (formatType elmVersion)
                base fields trailing multiline


formatVar :: ElmVersion -> ASTf.Variable.Ref -> Line
formatVar elmVersion var =
    case var of
        ASTf.Variable.VarRef namespace name ->
            formatLowercaseIdentifier elmVersion namespace name

        ASTf.Variable.TagRef namespace name ->
            case namespace of
                [] -> identifier $ formatVarName'' elmVersion name
                _ ->
                    row
                        [ formatQualifiedUppercaseIdentifier elmVersion namespace
                        , punc "."
                        , identifier $ formatVarName'' elmVersion name
                        ]

        ASTf.Variable.OpRef name ->
            formatSymbolIdentifierInParens name


formatSymbolIdentifierInParens :: SymbolIdentifier -> Line
formatSymbolIdentifierInParens (SymbolIdentifier name) =
    identifier $ "(" ++ name ++ ")"


formatInfixVar :: ElmVersion -> ASTf.Variable.Ref -> Line
formatInfixVar elmVersion var =
    case var of
        ASTf.Variable.VarRef _ _ ->
            row [ punc "`"
                , formatVar elmVersion var
                , punc "`"
                ]
        ASTf.Variable.TagRef _ _ ->
            row [ punc "`"
                , formatVar elmVersion var
                , punc "`"
                ]
        ASTf.Variable.OpRef (SymbolIdentifier name) ->
            identifier name


formatLowercaseIdentifier :: ElmVersion -> [ASTf.UppercaseIdentifier] -> ASTf.LowercaseIdentifier -> Line
formatLowercaseIdentifier elmVersion namespace (ASTf.LowercaseIdentifier name) =
    case (elmVersion, namespace, name) of
        (Elm_0_18_Upgrade, [], "fst") -> identifier "Tuple.first"
        (Elm_0_18_Upgrade, [ASTf.UppercaseIdentifier "Basics"], "fst") -> identifier "Tuple.first"
        (Elm_0_18_Upgrade, [], "snd") -> identifier "Tuple.second"
        (Elm_0_18_Upgrade, [ASTf.UppercaseIdentifier "Basics"], "snd") -> identifier "Tuple.second"
        (_, [], _) -> identifier $ formatVarName' elmVersion name
        _ ->
            row
                [ formatQualifiedUppercaseIdentifier elmVersion namespace
                , punc "."
                , identifier $ formatVarName' elmVersion name
                ]


formatUppercaseIdentifier :: ElmVersion -> ASTf.UppercaseIdentifier -> Line
formatUppercaseIdentifier elmVersion (ASTf.UppercaseIdentifier name) =
    identifier $ formatVarName' elmVersion name


formatQualifiedUppercaseIdentifier :: ElmVersion -> [ASTf.UppercaseIdentifier] -> Line
formatQualifiedUppercaseIdentifier elmVersion names =
  identifier $ List.intercalate "." $
      map (\(ASTf.UppercaseIdentifier name) -> formatVarName' elmVersion name) names


formatVarName :: ElmVersion -> ASTf.LowercaseIdentifier -> String
formatVarName elmVersion (ASTf.LowercaseIdentifier name) =
    formatVarName' elmVersion name


formatVarName' :: ElmVersion -> String -> String
formatVarName' elmVersion name =
    case elmVersion of
        Elm_0_18_Upgrade -> map (\x -> if x == '\'' then '_' else x) name
        _ -> name


formatVarName'' :: ElmVersion -> ASTf.UppercaseIdentifier -> String
formatVarName'' elmVersion (ASTf.UppercaseIdentifier name) =
    formatVarName' elmVersion name
