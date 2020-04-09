{-# OPTIONS_GHC -Wall #-}
module ElmFormat.Render.Box where

import Elm.Utils ((|>))
import Box
import ElmVersion (ElmVersion(..))

import qualified ElmFormat.AST.V0_16 as ElmFormat.AST
import ElmFormat.AST.V0_16 (UppercaseIdentifier(..), LowercaseIdentifier(..), SymbolIdentifier(..), WithEol)
import ElmFormat.AST.Declaration (TopLevelStructure, Declaration)
import qualified ElmFormat.AST.Declaration
import qualified ElmFormat.AST.Expression
import qualified ElmFormat.AST.Module
import qualified ElmFormat.AST.Pattern
import qualified ElmFormat.AST.Variable
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
import qualified ElmFormat.Parse.Parse as Parse
import qualified ElmFormat.Reporting.Annotation as RA
import qualified ElmFormat.Reporting.Region as Region
import qualified ElmFormat.Reporting.Result as Result
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


formatBinary :: Bool -> Box -> [ ( Bool, ElmFormat.AST.Comments, Box, Box ) ] -> Box
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
  | DDefinition (Maybe ElmFormat.AST.Variable.Ref)
  | DFixity
  | DDocComment
  deriving (Show)


declarationType :: (a -> BodyEntryType) -> TopLevelStructure a -> DeclarationType
declarationType entryType decl =
  case decl of
    ElmFormat.AST.Declaration.Entry entry ->
        case entryType $ RA.drop entry of
            BodyNamed name -> DDefinition (Just name)
            BodyUnnamed -> DDefinition Nothing
            BodyFixity -> DFixity

    ElmFormat.AST.Declaration.DocComment _ ->
      DDocComment

    ElmFormat.AST.Declaration.BodyComment ElmFormat.AST.CommentTrickOpener ->
      DStarter

    ElmFormat.AST.Declaration.BodyComment ElmFormat.AST.CommentTrickCloser ->
      DCloser

    ElmFormat.AST.Declaration.BodyComment _ ->
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


sortVars :: Bool -> Set (ElmFormat.AST.Commented ElmFormat.AST.Variable.Value) -> [[String]] -> ([[ElmFormat.AST.Commented ElmFormat.AST.Variable.Value]], ElmFormat.AST.Comments)
sortVars forceMultiline fromExposing fromDocs =
    let
        varOrder :: ElmFormat.AST.Commented ElmFormat.AST.Variable.Value -> (Int, String)
        varOrder (ElmFormat.AST.Commented _ (ElmFormat.AST.Variable.OpValue (SymbolIdentifier name)) _) = (1, name)
        varOrder (ElmFormat.AST.Commented _ (ElmFormat.AST.Variable.Union (UppercaseIdentifier name, _) _) _) = (2, name)
        varOrder (ElmFormat.AST.Commented _ (ElmFormat.AST.Variable.Value (LowercaseIdentifier name)) _) = (3, name)

        listedInDocs =
            fromDocs
                |> fmap (Maybe.mapMaybe (\v -> Map.lookup v allowedInDocs))
                |> filter (not . List.null)
                |> fmap (fmap (\v -> ElmFormat.AST.Commented [] v []))
                |> removeDuplicates

        listedInExposing =
            fromExposing
                |> Set.toList
                |> List.sortOn varOrder

        varName (ElmFormat.AST.Commented _ (ElmFormat.AST.Variable.Value (LowercaseIdentifier name)) _) = name
        varName (ElmFormat.AST.Commented _ (ElmFormat.AST.Variable.OpValue (SymbolIdentifier name)) _) = name
        varName (ElmFormat.AST.Commented _ (ElmFormat.AST.Variable.Union (UppercaseIdentifier name, _) _) _) = name

        varSetToMap set =
            Set.toList set
                |> fmap (\(ElmFormat.AST.Commented pre var post)-> (varName (ElmFormat.AST.Commented pre var post), var))
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
                |> fmap (\(ElmFormat.AST.Commented pre _ post) -> pre ++ post)
                |> concat
    in
    if List.null listedInDocs && forceMultiline
        then ( fmap (\x -> [x]) remainingFromExposing, commentsFromReorderedVars )
        else ( listedInDocs ++ if List.null remainingFromExposing then [] else [ remainingFromExposing ], commentsFromReorderedVars )


formatModuleHeader :: ElmVersion -> Bool -> ElmFormat.AST.Module.Module -> [Box]
formatModuleHeader elmVersion addDefaultHeader modu =
  let
      maybeHeader =
        if addDefaultHeader
            then Just (ElmFormat.AST.Module.header modu |> Maybe.fromMaybe ElmFormat.AST.Module.defaultHeader)
            else ElmFormat.AST.Module.header modu

      refName (ElmFormat.AST.Variable.VarRef _ (LowercaseIdentifier name)) = name
      refName (ElmFormat.AST.Variable.TagRef _ (UppercaseIdentifier name)) = name
      refName (ElmFormat.AST.Variable.OpRef (SymbolIdentifier name)) = name

      varName (ElmFormat.AST.Commented _ (ElmFormat.AST.Variable.Value (LowercaseIdentifier name)) _) = name
      varName (ElmFormat.AST.Commented _ (ElmFormat.AST.Variable.OpValue (SymbolIdentifier name)) _) = name
      varName (ElmFormat.AST.Commented _ (ElmFormat.AST.Variable.Union (UppercaseIdentifier name, _) _) _) = name

      documentedVars :: [[String]]
      documentedVars =
          ElmFormat.AST.Module.docs modu
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

      textToRef :: Text -> ElmFormat.AST.Variable.Ref
      textToRef text =
          case Text.unpack text of
              s@(c:_) | Char.isUpper c -> ElmFormat.AST.Variable.TagRef [] (UppercaseIdentifier s)
              s@(c:_) | Char.isLower c -> ElmFormat.AST.Variable.VarRef [] (LowercaseIdentifier s)
              '(':a:')':[] -> ElmFormat.AST.Variable.OpRef (SymbolIdentifier $ a:[])
              '(':a:b:')':[] -> ElmFormat.AST.Variable.OpRef (SymbolIdentifier $ a:b:[])
              s -> ElmFormat.AST.Variable.VarRef [] (LowercaseIdentifier s)

      definedVars :: Set (ElmFormat.AST.Commented ElmFormat.AST.Variable.Value)
      definedVars =
          ElmFormat.AST.Module.body modu
              |> concatMap extractVarName
              |> fmap (\x -> ElmFormat.AST.Commented [] x [])
              |> Set.fromList

      exportsList =
          case
              ElmFormat.AST.Module.exports (maybeHeader |> Maybe.fromMaybe ElmFormat.AST.Module.defaultHeader)
          of
              Just (ElmFormat.AST.KeywordCommented _ _ e) -> e
              Nothing -> ElmFormat.AST.Variable.ClosedListing

      detailedListingToSet :: ElmFormat.AST.Variable.Listing ElmFormat.AST.Module.DetailedListing -> Set (ElmFormat.AST.Commented ElmFormat.AST.Variable.Value)
      detailedListingToSet (ElmFormat.AST.Variable.OpenListing _) = Set.empty
      detailedListingToSet ElmFormat.AST.Variable.ClosedListing = Set.empty
      detailedListingToSet (ElmFormat.AST.Variable.ExplicitListing (ElmFormat.AST.Module.DetailedListing values operators types) _) =
          Set.unions
              [ Map.assocs values |> fmap (\(name, ElmFormat.AST.Commented pre () post) -> ElmFormat.AST.Commented pre (ElmFormat.AST.Variable.Value name) post) |> Set.fromList
              , Map.assocs operators |> fmap (\(name, ElmFormat.AST.Commented pre () post) -> ElmFormat.AST.Commented pre (ElmFormat.AST.Variable.OpValue name) post) |> Set.fromList
              , Map.assocs types |> fmap (\(name, ElmFormat.AST.Commented pre (preListing, listing) post) -> ElmFormat.AST.Commented pre (ElmFormat.AST.Variable.Union (name, preListing) listing) post) |> Set.fromList
              ]

      detailedListingIsMultiline :: ElmFormat.AST.Variable.Listing a -> Bool
      detailedListingIsMultiline (ElmFormat.AST.Variable.ExplicitListing _ isMultiline) = isMultiline
      detailedListingIsMultiline _ = False

      varsToExpose =
          case ElmFormat.AST.Module.exports =<< maybeHeader of
              Nothing ->
                  if null $ concat documentedVars
                      then definedVars
                      else definedVars |> Set.filter (\v -> Set.member (varName v) documentedVarsSet)
              Just (ElmFormat.AST.KeywordCommented _ _ e) -> detailedListingToSet e

      sortedExports =
          sortVars
              (detailedListingIsMultiline exportsList)
              varsToExpose
              documentedVars

      extractVarName :: TopLevelStructure Declaration -> [ElmFormat.AST.Variable.Value]
      extractVarName decl =
          case decl of
              ElmFormat.AST.Declaration.DocComment _ -> []
              ElmFormat.AST.Declaration.BodyComment _ -> []
              ElmFormat.AST.Declaration.Entry (RA.A _ (ElmFormat.AST.Declaration.PortAnnotation (ElmFormat.AST.Commented _ (LowercaseIdentifier name) _) _ _)) -> [ ElmFormat.AST.Variable.Value (LowercaseIdentifier name) ]
              ElmFormat.AST.Declaration.Entry (RA.A _ (ElmFormat.AST.Declaration.Definition (RA.A _ (ElmFormat.AST.Pattern.VarPattern (LowercaseIdentifier name))) _ _ _)) -> [ ElmFormat.AST.Variable.Value (LowercaseIdentifier name) ]
              ElmFormat.AST.Declaration.Entry (RA.A _ (ElmFormat.AST.Declaration.Definition (RA.A _ (ElmFormat.AST.Pattern.Record fields)) _ _ _)) -> fmap (\(ElmFormat.AST.Commented _ f _) -> ElmFormat.AST.Variable.Value f) fields
              ElmFormat.AST.Declaration.Entry (RA.A _ (ElmFormat.AST.Declaration.Datatype (ElmFormat.AST.Commented _ (UppercaseIdentifier name, _) _) _)) -> [ ElmFormat.AST.Variable.Union (UppercaseIdentifier name, []) (ElmFormat.AST.Variable.OpenListing (ElmFormat.AST.Commented [] () []))]
              ElmFormat.AST.Declaration.Entry (RA.A _ (ElmFormat.AST.Declaration.TypeAlias _ (ElmFormat.AST.Commented _ (UppercaseIdentifier name, _) _) _)) -> [ ElmFormat.AST.Variable.Union (UppercaseIdentifier name, []) ElmFormat.AST.Variable.ClosedListing ]
              ElmFormat.AST.Declaration.Entry (RA.A _ _) -> []

      formatModuleLine' header@(ElmFormat.AST.Module.Header srcTag name moduleSettings exports) =
        let
            (preExposing, postExposing) =
                case exports of
                    Nothing -> ([], [])
                    Just (ElmFormat.AST.KeywordCommented pre post _) -> (pre, post)
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
          fmap (formatDocComment elmVersion (makeImportInfo modu)) $ RA.drop $ ElmFormat.AST.Module.docs modu

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


formatImports :: ElmVersion -> ElmFormat.AST.Module.Module -> [Box]
formatImports elmVersion modu =
    let
        (comments, imports) =
            ElmFormat.AST.Module.imports modu
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


formatModuleLine_0_16 :: ElmFormat.AST.Module.Header -> Box
formatModuleLine_0_16 header =
  let
    elmVersion = Elm_0_16

    exports =
        case ElmFormat.AST.Module.exports header of
            Just (ElmFormat.AST.KeywordCommented _ _ value) -> value
            Nothing -> ElmFormat.AST.Variable.OpenListing (ElmFormat.AST.Commented [] () [])

    formatExports =
        case formatListing (formatDetailedListing elmVersion) exports of
            Just listing ->
                listing
            _ ->
                pleaseReport "UNEXPECTED MODULE DECLARATION" "empty listing"

    (preWhere, postWhere) =
        case ElmFormat.AST.Module.exports header of
            Nothing -> ([], [])
            Just (ElmFormat.AST.KeywordCommented pre post _) -> (pre, post)

    whereClause =
        formatCommented (line . keyword) (ElmFormat.AST.Commented preWhere "where" postWhere)
  in
    case
      ( formatCommented (line . formatQualifiedUppercaseIdentifier elmVersion) $ ElmFormat.AST.Module.name header
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
    -> ([[ElmFormat.AST.Commented ElmFormat.AST.Variable.Value]], ElmFormat.AST.Comments)
    -> ElmFormat.AST.Module.SourceTag
    -> ElmFormat.AST.Commented [UppercaseIdentifier]
    -> Maybe (ElmFormat.AST.KeywordCommented ElmFormat.AST.Module.SourceSettings)
    -> ElmFormat.AST.Comments
    -> ElmFormat.AST.Comments
    -> Box
formatModuleLine elmVersion (varsToExpose, extraComments) srcTag name moduleSettings preExposing postExposing =
  let
    tag =
      case srcTag of
        ElmFormat.AST.Module.Normal ->
          line $ keyword "module"

        ElmFormat.AST.Module.Port comments ->
          ElmStructure.spaceSepOrIndented
            (formatTailCommented (line . keyword) ("port", comments))
            [ line $ keyword "module" ]

        ElmFormat.AST.Module.Effect comments ->
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
          (whereClause ++ [formatCommented (line . keyword) (ElmFormat.AST.Commented preExposing "exposing" postExposing)])
      )
      [ exports ]


formatModule :: ElmVersion -> Bool -> Int -> ElmFormat.AST.Module.Module -> Box
formatModule elmVersion addDefaultHeader spacing modu =
    let
        initialComments' =
          case ElmFormat.AST.Module.initialComments modu of
            [] ->
              []
            comments ->
              (map formatComment comments)
                ++ [ blankLine, blankLine ]

        spaceBeforeBody =
            case ElmFormat.AST.Module.body modu of
                [] -> 0
                ElmFormat.AST.Declaration.BodyComment _ : _ -> spacing + 1
                _ -> spacing
    in
      stack1 $
          concat
              [ initialComments'
              , formatModuleHeader elmVersion addDefaultHeader modu
              , List.replicate spaceBeforeBody blankLine
              , maybeToList $ formatModuleBody spacing elmVersion (makeImportInfo modu) (ElmFormat.AST.Module.body modu)
              ]


data ImportInfo =
    ImportInfo
        { _exposed :: Map.Map LowercaseIdentifier [UppercaseIdentifier]
        , _aliases :: Map.Map [UppercaseIdentifier] UppercaseIdentifier
        }


makeImportInfo :: ElmFormat.AST.Module.Module -> ImportInfo
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
            case Map.lookup importName (snd $ ElmFormat.AST.Module.imports modu) of
                Nothing -> mempty
                Just (_, importMethod) ->
                    case ElmFormat.AST.Module.exposedVars importMethod of
                        (_, (_, ElmFormat.AST.Variable.OpenListing _)) ->
                            -- import Html.Attributes [as ...] exposing (..)
                            Map.lookup importName knownModuleContents
                                |> Maybe.fromMaybe []
                                |> fmap (\n -> (n, importName))
                                |> Map.fromList

                        (_, (_, ElmFormat.AST.Variable.ExplicitListing details _)) ->
                            -- import Html.Attributes [as ...] exposing (some, stuff)
                            ElmFormat.AST.Module.values details
                                |> Map.keys
                                |> fmap (\n -> (n, importName))
                                |> Map.fromList

                        _ -> mempty

        aliases =
            -- currently this only checks for Html.Attributes (needed for Elm 0.19 upgrade)
            let
                importName = (fmap UppercaseIdentifier ["Html", "Attributes"])
            in
            case Map.lookup importName (snd $ ElmFormat.AST.Module.imports modu) of
                Nothing -> mempty
                Just (_, importMethod) ->
                    case ElmFormat.AST.Module.alias importMethod of
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
                ElmFormat.AST.Declaration.Definition pat _ _ _ ->
                    case RA.drop pat of
                        ElmFormat.AST.Pattern.VarPattern name ->
                            BodyNamed $ ElmFormat.AST.Variable.VarRef [] name

                        ElmFormat.AST.Pattern.OpPattern name ->
                            BodyNamed $ ElmFormat.AST.Variable.OpRef name

                        _ ->
                            BodyUnnamed

                ElmFormat.AST.Declaration.Datatype (ElmFormat.AST.Commented _ (name, _) _) _ ->
                    BodyNamed $ ElmFormat.AST.Variable.TagRef [] name

                ElmFormat.AST.Declaration.TypeAlias _ (ElmFormat.AST.Commented _ (name, _) _) _ ->
                    BodyNamed $ ElmFormat.AST.Variable.TagRef [] name

                ElmFormat.AST.Declaration.PortDefinition (ElmFormat.AST.Commented _ name _) _ _ ->
                    BodyNamed $ ElmFormat.AST.Variable.VarRef [] name

                ElmFormat.AST.Declaration.TypeAnnotation (name, _) _ ->
                    BodyNamed name

                ElmFormat.AST.Declaration.PortAnnotation (ElmFormat.AST.Commented _ name _) _ _ ->
                    BodyNamed $ ElmFormat.AST.Variable.VarRef [] name

                ElmFormat.AST.Declaration.Fixity _ _ _ _ _ ->
                    BodyFixity

                ElmFormat.AST.Declaration.Fixity_0_19 _ _ _ _ ->
                    BodyFixity
    in
    formatTopLevelBody linesBetween elmVersion importInfo entryType (formatDeclaration elmVersion importInfo) body


data BodyEntryType
    = BodyNamed ElmFormat.AST.Variable.Ref
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
    | ExpressionsCode [TopLevelStructure (WithEol ElmFormat.AST.Expression.Expr)]
    | ModuleCode ElmFormat.AST.Module.Module
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


formatImport :: ElmVersion -> ElmFormat.AST.Module.UserImport -> Box
formatImport elmVersion (name, method) =
    let
        as =
          (ElmFormat.AST.Module.alias method)
            |> fmap (formatImportClause
            (Just . line . formatUppercaseIdentifier elmVersion)
            "as")
            |> Monad.join

        (exposingPreKeyword, exposingPostKeywordAndListing)
          = ElmFormat.AST.Module.exposedVars method

        exposing =
          formatImportClause
            (formatListing (formatDetailedListing elmVersion))
            "exposing"
            (exposingPreKeyword, exposingPostKeywordAndListing)

        formatImportClause :: (a -> Maybe Box) -> String -> (ElmFormat.AST.Comments, (ElmFormat.AST.Comments, a)) -> Maybe Box
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


formatListing :: (a -> [Box]) -> ElmFormat.AST.Variable.Listing a -> Maybe Box
formatListing format listing =
    case listing of
        ElmFormat.AST.Variable.ClosedListing ->
            Nothing

        ElmFormat.AST.Variable.OpenListing comments ->
            Just $ parens $ formatCommented (line . keyword) $ fmap (const "..") comments

        ElmFormat.AST.Variable.ExplicitListing vars multiline ->
            Just $ ElmStructure.group False "(" "," ")" multiline $ format vars


formatDetailedListing :: ElmVersion -> ElmFormat.AST.Module.DetailedListing -> [Box]
formatDetailedListing elmVersion listing =
    concat
        [ formatCommentedMap
            (\name () -> ElmFormat.AST.Variable.OpValue name)
            (formatVarValue elmVersion)
            (ElmFormat.AST.Module.operators listing)
        , formatCommentedMap
            (\name (inner, listing_) -> ElmFormat.AST.Variable.Union (name, inner) listing_)
            (formatVarValue elmVersion)
            (ElmFormat.AST.Module.types listing)
        , formatCommentedMap
            (\name () -> ElmFormat.AST.Variable.Value name)
            (formatVarValue elmVersion)
            (ElmFormat.AST.Module.values listing)
        ]


formatCommentedMap :: (k -> v -> a) -> (a -> Box) ->  ElmFormat.AST.Variable.CommentedMap k v -> [Box]
formatCommentedMap construct format values =
    let
        format' (k, ElmFormat.AST.Commented pre v post)
            = formatCommented format $ ElmFormat.AST.Commented pre (construct k v) post
    in
    values
        |> Map.assocs
        |> map format'


formatVarValue :: ElmVersion -> ElmFormat.AST.Variable.Value -> Box
formatVarValue elmVersion aval =
    case aval of
        ElmFormat.AST.Variable.Value val ->
            line $ formatLowercaseIdentifier elmVersion [] val

        ElmFormat.AST.Variable.OpValue (SymbolIdentifier name) ->
            line $ identifier $ "(" ++ name ++ ")"

        ElmFormat.AST.Variable.Union name listing ->
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
        ElmFormat.AST.Declaration.DocComment docs ->
            formatDocComment elmVersion importInfo docs

        ElmFormat.AST.Declaration.BodyComment c ->
            formatComment c

        ElmFormat.AST.Declaration.Entry entry ->
            formatEntry (RA.drop entry)


formatDeclaration :: ElmVersion -> ImportInfo -> Declaration -> Box
formatDeclaration elmVersion importInfo decl =
    case decl of
        ElmFormat.AST.Declaration.Definition name args comments expr ->
            formatDefinition elmVersion importInfo name args comments expr

        ElmFormat.AST.Declaration.TypeAnnotation name typ ->
            formatTypeAnnotation elmVersion name typ

        ElmFormat.AST.Declaration.Datatype nameWithArgs tags ->
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

        ElmFormat.AST.Declaration.TypeAlias preAlias nameWithArgs typ ->
            ElmStructure.definition "=" True
            (line $ keyword "type")
            [ formatHeadCommented (line . keyword) (preAlias, "alias")
            , formatCommented (formatNameWithArgs elmVersion) nameWithArgs
            ]
            (formatHeadCommentedStack (formatType elmVersion) typ)

        ElmFormat.AST.Declaration.PortAnnotation name typeComments typ ->
            ElmStructure.definition ":" False
            (line $ keyword "port")
            [ formatCommented (line . formatLowercaseIdentifier elmVersion []) name ]
            (formatCommented' typeComments (formatType elmVersion) typ)

        ElmFormat.AST.Declaration.PortDefinition name bodyComments expr ->
            ElmStructure.definition "=" True
            (line $ keyword "port")
            [formatCommented (line . formatLowercaseIdentifier elmVersion []) name]
            (formatCommented' bodyComments (formatExpression elmVersion importInfo SyntaxSeparated) expr)

        ElmFormat.AST.Declaration.Fixity assoc precedenceComments precedence nameComments name ->
            case
                ( formatCommented' nameComments (line . formatInfixVar elmVersion) name
                , formatCommented' precedenceComments (line . literal . show) precedence
                )
            of
                (SingleLine name', SingleLine precedence') ->
                    line $ row
                        [ case assoc of
                                ElmFormat.AST.Declaration.L -> keyword "infixl"
                                ElmFormat.AST.Declaration.R -> keyword "infixr"
                                ElmFormat.AST.Declaration.N -> keyword "infix"
                        , space
                        , precedence'
                        , space
                        , name'
                        ]
                _ ->
                    pleaseReport "TODO" "multiline fixity declaration"

        ElmFormat.AST.Declaration.Fixity_0_19 assoc precedence name value ->
            let
                formatAssoc a =
                    case a of
                        ElmFormat.AST.Declaration.L -> keyword "left "
                        ElmFormat.AST.Declaration.R -> keyword "right"
                        ElmFormat.AST.Declaration.N -> keyword "non  "
            in
            ElmStructure.spaceSepOrIndented
                (line $ keyword "infix")
                [ formatHeadCommented (line . formatAssoc) assoc
                , formatHeadCommented (line . literal . show) precedence
                , formatCommented (line . formatSymbolIdentifierInParens) name
                , line $ keyword "="
                , formatHeadCommented (line . identifier . formatVarName elmVersion) value
                ]


formatNameWithArgs :: ElmVersion -> (ElmFormat.AST.UppercaseIdentifier, [(ElmFormat.AST.Comments, ElmFormat.AST.LowercaseIdentifier)]) -> Box
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
    -> ElmFormat.AST.Pattern.Pattern
    -> [(ElmFormat.AST.Comments, ElmFormat.AST.Pattern.Pattern)]
    -> [ElmFormat.AST.Comment]
    -> ElmFormat.AST.Expression.Expr
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


formatTypeAnnotation :: ElmVersion -> (ElmFormat.AST.Variable.Ref, ElmFormat.AST.Comments) -> (ElmFormat.AST.Comments, ElmFormat.AST.Type) -> Box
formatTypeAnnotation elmVersion name typ =
  ElmStructure.definition ":" False
    (formatTailCommented (line . formatVar elmVersion) name)
    []
    (formatHeadCommented (formatType elmVersion) typ)


formatPattern :: ElmVersion -> Bool -> ElmFormat.AST.Pattern.Pattern -> Box
formatPattern elmVersion parensRequired apattern =
    case RA.drop apattern of
        ElmFormat.AST.Pattern.Anything ->
            line $ keyword "_"

        ElmFormat.AST.Pattern.UnitPattern comments ->
            formatUnit '(' ')' comments

        ElmFormat.AST.Pattern.Literal lit ->
            formatLiteral elmVersion lit

        ElmFormat.AST.Pattern.VarPattern var ->
            line $ formatLowercaseIdentifier elmVersion [] var

        ElmFormat.AST.Pattern.OpPattern (SymbolIdentifier name) ->
            line $ identifier $ "(" ++ name ++ ")"

        ElmFormat.AST.Pattern.ConsPattern first rest ->
            let
                formatRight (preOp, postOp, term, eol) =
                    ( False
                    , preOp
                    , line $ punc "::"
                    , formatCommented
                        (formatEolCommented $ formatPattern elmVersion True)
                        (ElmFormat.AST.Commented postOp (ElmFormat.AST.WithEol term eol) [])
                    )
            in
                formatBinary False
                    (formatEolCommented (formatPattern elmVersion True) first)
                    (map formatRight rest)
                |> if parensRequired then parens else id

        ElmFormat.AST.Pattern.Data ctor [] ->
            line (formatQualifiedUppercaseIdentifier elmVersion ctor)
                |>
                    case (elmVersion, ctor) of
                        (Elm_0_16, [_]) ->
                            id
                        (Elm_0_16, _) ->
                            if parensRequired then parens else id
                        _ ->
                            id

        ElmFormat.AST.Pattern.Data ctor patterns ->
            ElmStructure.application
                (ElmFormat.AST.FAJoinFirst ElmFormat.AST.JoinAll)
                (line $ formatQualifiedUppercaseIdentifier elmVersion ctor)
                (map (formatHeadCommented $ formatPattern elmVersion True) patterns)
            |> if parensRequired then parens else id

        ElmFormat.AST.Pattern.PatternParens pattern ->
            formatCommented (formatPattern elmVersion False) pattern
              |> parens

        ElmFormat.AST.Pattern.Tuple patterns ->
            ElmStructure.group True "(" "," ")" False $ map (formatCommented $ formatPattern elmVersion False) patterns

        ElmFormat.AST.Pattern.EmptyListPattern comments ->
            formatUnit '[' ']' comments

        ElmFormat.AST.Pattern.List patterns ->
            ElmStructure.group True "[" "," "]" False $ map (formatCommented $ formatPattern elmVersion False) patterns

        ElmFormat.AST.Pattern.EmptyRecordPattern comments ->
            formatUnit '{' '}' comments

        ElmFormat.AST.Pattern.Record fields ->
            ElmStructure.group True "{" "," "}" False $ map (formatCommented $ line . formatLowercaseIdentifier elmVersion []) fields

        ElmFormat.AST.Pattern.Alias pattern name ->
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


formatRecordPair :: ElmVersion -> String -> (v -> Box) -> (ElmFormat.AST.Commented ElmFormat.AST.LowercaseIdentifier, ElmFormat.AST.Commented v, Bool) -> Box
formatRecordPair elmVersion delim formatValue (ElmFormat.AST.Commented pre k postK, v, forceMultiline) =
    ElmStructure.equalsPair delim forceMultiline
      (formatCommented (line . formatLowercaseIdentifier elmVersion []) $ ElmFormat.AST.Commented [] k postK)
      (formatCommented formatValue v)
    |> (\x -> ElmFormat.AST.Commented pre x []) |> formatCommented id


formatPair :: (a -> Line) -> String -> (b -> Box) -> ElmFormat.AST.Pair a b -> Box
formatPair formatA delim formatB (ElmFormat.AST.Pair a b (ElmFormat.AST.ForceMultiline forceMultiline)) =
    ElmStructure.equalsPair delim forceMultiline
        (formatTailCommented (line . formatA) a)
        (formatHeadCommented formatB b)


negativeCasePatternWorkaround :: ElmFormat.AST.Commented ElmFormat.AST.Pattern.Pattern -> Box -> Box
negativeCasePatternWorkaround (ElmFormat.AST.Commented _ (RA.A _ pattern) _) =
    case pattern of
        ElmFormat.AST.Pattern.Literal (ElmFormat.AST.IntNum i _) | i < 0 -> parens
        ElmFormat.AST.Pattern.Literal (ElmFormat.AST.FloatNum f _) | f < 0 -> parens
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


formatExpression :: ElmVersion -> ImportInfo -> ExpressionContext -> ElmFormat.AST.Expression.Expr -> Box
formatExpression elmVersion importInfo context aexpr =
    case elmVersion of
        Elm_0_19_Upgrade -> formatExpression' elmVersion importInfo context (Upgrade_0_19.transform (_exposed importInfo) (_aliases importInfo) aexpr)
        _ -> formatExpression' elmVersion importInfo context aexpr


formatExpression' :: ElmVersion -> ImportInfo -> ExpressionContext -> ElmFormat.AST.Expression.Expr -> Box
formatExpression' elmVersion importInfo context aexpr =
    case RA.drop aexpr of
        ElmFormat.AST.Expression.Literal lit ->
            formatLiteral elmVersion lit

        ElmFormat.AST.Expression.VarExpr v ->
            line $ formatVar elmVersion v

        ElmFormat.AST.Expression.Range left right multiline ->
            case elmVersion of
                Elm_0_16 -> formatRange_0_17 elmVersion importInfo left right multiline
                Elm_0_17 -> formatRange_0_17 elmVersion importInfo left right multiline
                Elm_0_18 -> formatRange_0_17 elmVersion importInfo left right multiline
                Elm_0_19 -> formatRange_0_18 elmVersion importInfo context left right
                Elm_0_18_Upgrade -> formatRange_0_18 elmVersion importInfo context left right
                Elm_0_19_Upgrade -> formatRange_0_18 elmVersion importInfo context left right

        ElmFormat.AST.Expression.ExplicitList exprs trailing multiline ->
            formatSequence '[' ',' (Just ']')
                (formatExpression elmVersion importInfo SyntaxSeparated)
                multiline
                trailing
                exprs

        ElmFormat.AST.Expression.Binops left ops multiline ->
            case elmVersion of
                Elm_0_16 -> formatBinops_0_17 elmVersion importInfo left ops multiline
                Elm_0_17 -> formatBinops_0_17 elmVersion importInfo left ops multiline
                Elm_0_18 -> formatBinops_0_17 elmVersion importInfo left ops multiline
                Elm_0_19 -> formatBinops_0_17 elmVersion importInfo left ops multiline
                Elm_0_18_Upgrade -> formatBinops_0_18 elmVersion importInfo left ops multiline
                Elm_0_19_Upgrade -> formatBinops_0_19_upgrade elmVersion importInfo left ops multiline
            |> expressionParens InfixSeparated context

        ElmFormat.AST.Expression.Lambda patterns bodyComments expr multiline ->
            case
                ( multiline
                , allSingles $ map (formatCommented (formatPattern elmVersion True) . (\(c,p) -> ElmFormat.AST.Commented c p [])) patterns
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

        ElmFormat.AST.Expression.Unary ElmFormat.AST.Expression.Negative e ->
            prefix (punc "-") $ formatExpression elmVersion importInfo SpaceSeparated e -- TODO: This might need something stronger than SpaceSeparated?

        ElmFormat.AST.Expression.App left [] _ ->
            formatExpression elmVersion importInfo context left

        ElmFormat.AST.Expression.App left args multiline ->
            ElmStructure.application
                multiline
                (formatExpression elmVersion importInfo InfixSeparated left)
                (fmap (formatPreCommentedExpression elmVersion importInfo SpaceSeparated) args)
                |> expressionParens SpaceSeparated context

        ElmFormat.AST.Expression.If if' elseifs (elsComments, els) ->
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
                        , indent $ formatCommented_ True (formatExpression elmVersion importInfo SyntaxSeparated) (ElmFormat.AST.Commented elsComments els [])
                        ]
                    |> expressionParens AmbiguousEnd context

        ElmFormat.AST.Expression.Let defs bodyComments expr ->
            let
                spacer first _ =
                    case first of
                        ElmFormat.AST.Expression.LetDefinition _ _ _ _ ->
                            [ blankLine ]
                        _ ->
                            []

                formatDefinition' def =
                  case def of
                    ElmFormat.AST.Expression.LetDefinition name args comments expr' ->
                      formatDefinition elmVersion importInfo name args comments expr'

                    ElmFormat.AST.Expression.LetAnnotation name typ ->
                      formatTypeAnnotation elmVersion name typ

                    ElmFormat.AST.Expression.LetComment comment ->
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

        ElmFormat.AST.Expression.Case (subject,multiline) clauses ->
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
                      , (formatPattern elmVersion False $ (\(ElmFormat.AST.Commented _ x _) -> x) pat)
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
                        (ElmFormat.AST.Commented pre _ [], SingleLine pat', _, body') ->
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

        ElmFormat.AST.Expression.Tuple exprs multiline ->
            ElmStructure.group True "(" "," ")" multiline $ map (formatCommentedExpression elmVersion importInfo SyntaxSeparated) exprs

        ElmFormat.AST.Expression.TupleFunction n ->
            line $ keyword $ "(" ++ (List.replicate (n-1) ',') ++ ")"

        ElmFormat.AST.Expression.Access expr field ->
            formatExpression elmVersion importInfo SpaceSeparated expr -- TODO: does this need a different context than SpaceSeparated?
                |> addSuffix (row $ [punc ".", formatLowercaseIdentifier elmVersion [] field])

        ElmFormat.AST.Expression.AccessFunction (ElmFormat.AST.LowercaseIdentifier field) ->
            line $ identifier $ "." ++ (formatVarName' elmVersion field)

        ElmFormat.AST.Expression.Record base fields trailing multiline ->
            formatRecordLike
                (line . formatLowercaseIdentifier elmVersion [])
                (formatLowercaseIdentifier elmVersion [])
                "="
                (formatExpression elmVersion importInfo SyntaxSeparated)
                base fields trailing multiline

        ElmFormat.AST.Expression.Parens expr ->
            case expr of
                ElmFormat.AST.Commented [] expr' [] ->
                    formatExpression elmVersion importInfo context expr'

                _ ->
                    formatCommentedExpression elmVersion importInfo SyntaxSeparated expr
                        |> parens


        ElmFormat.AST.Expression.Unit comments ->
            formatUnit '(' ')' comments

        ElmFormat.AST.Expression.GLShader src ->
          line $ row
            [ punc "[glsl|"
            , literal $ src
            , punc "|]"
            ]


formatCommentedExpression :: ElmVersion -> ImportInfo -> ExpressionContext -> ElmFormat.AST.Commented ElmFormat.AST.Expression.Expr -> Box
formatCommentedExpression elmVersion importInfo context (ElmFormat.AST.Commented pre e post) =
    let
        commented' =
            case RA.drop e of
                ElmFormat.AST.Expression.Parens (ElmFormat.AST.Commented pre'' e'' post'') ->
                    ElmFormat.AST.Commented (pre ++ pre'') e'' (post'' ++ post)
                _ -> ElmFormat.AST.Commented pre e post
    in
    formatCommented (formatExpression elmVersion importInfo context) commented'


formatPreCommentedExpression :: ElmVersion -> ImportInfo -> ExpressionContext -> ElmFormat.AST.PreCommented ElmFormat.AST.Expression.Expr -> Box
formatPreCommentedExpression elmVersion importInfo context (pre, e) =
    let
        (pre', e') =
            case RA.drop e of
                ElmFormat.AST.Expression.Parens (ElmFormat.AST.Commented pre'' e'' []) ->
                    (pre ++ pre'', e'')
                _ -> (pre, e)
    in
    formatCommented' pre' (formatExpression elmVersion importInfo context) e'


formatRecordLike ::
    (base -> Box) -> (key -> Line) -> String -> (value -> Box)
    -> Maybe (ElmFormat.AST.Commented base) -> ElmFormat.AST.Sequence (ElmFormat.AST.Pair key value)-> ElmFormat.AST.Comments -> ElmFormat.AST.ForceMultiline
    -> Box
formatRecordLike formatBase formatKey fieldSep formatValue base' fields trailing multiline =
    case (base', fields) of
      ( Just base, pairs' ) ->
          ElmStructure.extensionGroup'
              ((\(ElmFormat.AST.ForceMultiline b) -> b) multiline)
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


formatSequence :: Char -> Char -> Maybe Char -> (a -> Box) -> ElmFormat.AST.ForceMultiline -> ElmFormat.AST.Comments -> ElmFormat.AST.Sequence a -> Box
formatSequence left delim right formatA (ElmFormat.AST.ForceMultiline multiline) trailing (first:rest) =
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
    -> ElmFormat.AST.Expression.Expr
    -> [(ElmFormat.AST.Comments, ElmFormat.AST.Variable.Ref, ElmFormat.AST.Comments, ElmFormat.AST.Expression.Expr)]
    -> Bool
    -> Box
formatBinops_0_17 elmVersion importInfo left ops multiline =
    formatBinops_common (,) elmVersion importInfo left ops multiline


formatBinops_0_18 ::
    ElmVersion
    -> ImportInfo
    -> ElmFormat.AST.Expression.Expr
    -> [(ElmFormat.AST.Comments, ElmFormat.AST.Variable.Ref, ElmFormat.AST.Comments, ElmFormat.AST.Expression.Expr)]
    -> Bool
    -> Box
formatBinops_0_18 elmVersion importInfo left ops multiline =
    formatBinops_common removeBackticks elmVersion importInfo left ops multiline


formatBinops_0_19_upgrade ::
    ElmVersion
    -> ImportInfo
    -> ElmFormat.AST.Expression.Expr
    -> [(ElmFormat.AST.Comments, ElmFormat.AST.Variable.Ref, ElmFormat.AST.Comments, ElmFormat.AST.Expression.Expr)]
    -> Bool
    -> Box
formatBinops_0_19_upgrade elmVersion importInfo left ops multiline =
    let
        transform = uncurry removeBangs . uncurry removeMod
    in
    formatBinops_common (curry transform) elmVersion importInfo left ops multiline


formatBinops_common ::
    (ElmFormat.AST.Expression.Expr
        -> [(ElmFormat.AST.Comments, ElmFormat.AST.Variable.Ref, ElmFormat.AST.Comments, ElmFormat.AST.Expression.Expr)]
        -> ( ElmFormat.AST.Expression.Expr
           , [(ElmFormat.AST.Comments, ElmFormat.AST.Variable.Ref, ElmFormat.AST.Comments, ElmFormat.AST.Expression.Expr)]
           )
    )
    -> ElmVersion
    -> ImportInfo
    -> ElmFormat.AST.Expression.Expr
    -> [(ElmFormat.AST.Comments, ElmFormat.AST.Variable.Ref, ElmFormat.AST.Comments, ElmFormat.AST.Expression.Expr)]
    -> Bool
    -> Box
formatBinops_common transform elmVersion importInfo left ops multiline =
    let
        (left', ops') = transform left ops

        formatPair_ isLast ( po, o, pe, e ) =
            let
                isLeftPipe =
                    o == ElmFormat.AST.Variable.OpRef (SymbolIdentifier "<|")

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
    ElmFormat.AST.Expression.Expr
    -> [(ElmFormat.AST.Comments, ElmFormat.AST.Variable.Ref, ElmFormat.AST.Comments, ElmFormat.AST.Expression.Expr)]
    -> (ElmFormat.AST.Expression.Expr, [(ElmFormat.AST.Comments, ElmFormat.AST.Variable.Ref, ElmFormat.AST.Comments, ElmFormat.AST.Expression.Expr)])
removeBackticks left ops =
    case ops of
        [] -> (left, ops)
        (pre, ElmFormat.AST.Variable.VarRef v' v, post, e):rest
          | v == ElmFormat.AST.LowercaseIdentifier "andThen" || v == ElmFormat.AST.LowercaseIdentifier "onError"
          ->
            -- Convert `andThen` to |> andThen
            let
                e' = noRegion $ ElmFormat.AST.Expression.App
                    (noRegion $ ElmFormat.AST.Expression.VarExpr $ ElmFormat.AST.Variable.VarRef v' v)
                    [ (post, e)
                    ]
                    (ElmFormat.AST.FAJoinFirst ElmFormat.AST.JoinAll)

                (e'', rest') = removeBackticks e' rest
            in
                (left, (pre, ElmFormat.AST.Variable.OpRef $ SymbolIdentifier "|>", [], e''):rest')

        (pre, ElmFormat.AST.Variable.VarRef v' v, post, e):rest ->
            -- Convert other backtick operators to normal function application
            removeBackticks
                (noRegion $ ElmFormat.AST.Expression.App
                    (noRegion $ ElmFormat.AST.Expression.VarExpr $ ElmFormat.AST.Variable.VarRef v' v)
                    [ (pre, left)
                    , (post, e)
                    ]
                    (ElmFormat.AST.FAJoinFirst ElmFormat.AST.JoinAll)
                )
                rest

        (pre, op, post, e):rest ->
            -- Preserve symbolic infix operators
            let
                (e', rest') = removeBackticks e rest
            in
                (left, (pre, op, post, e'):rest')


removeBangs ::
    ElmFormat.AST.Expression.Expr
    -> [(ElmFormat.AST.Comments, ElmFormat.AST.Variable.Ref, ElmFormat.AST.Comments, ElmFormat.AST.Expression.Expr)]
    -> (ElmFormat.AST.Expression.Expr, [(ElmFormat.AST.Comments, ElmFormat.AST.Variable.Ref, ElmFormat.AST.Comments, ElmFormat.AST.Expression.Expr)])
removeBangs left ops =
    let
        cmds' post cmds =
            case RA.drop cmds of
                ElmFormat.AST.Expression.ExplicitList [] innerComments _ ->
                    ElmFormat.AST.Commented
                        post
                        (noRegion $ ElmFormat.AST.Expression.VarExpr (ElmFormat.AST.Variable.VarRef [ElmFormat.AST.UppercaseIdentifier "Cmd"] (ElmFormat.AST.LowercaseIdentifier "none")))
                        innerComments

                ElmFormat.AST.Expression.ExplicitList [(extraPre, (pre', ElmFormat.AST.WithEol cmd eol))] trailing _ ->
                    let
                        eolComment =
                            case eol of
                                Nothing -> []
                                Just c -> [ElmFormat.AST.LineComment c]
                    in
                    ElmFormat.AST.Commented (post ++ extraPre ++ pre') cmd (eolComment ++ trailing)

                _ ->
                    ElmFormat.AST.Commented [] (noRegion $ ElmFormat.AST.Expression.App
                        (noRegion $ ElmFormat.AST.Expression.VarExpr (ElmFormat.AST.Variable.VarRef [ElmFormat.AST.UppercaseIdentifier "Cmd"] (ElmFormat.AST.LowercaseIdentifier "batch")))
                        [(post, cmds)]
                        (ElmFormat.AST.FAJoinFirst ElmFormat.AST.JoinAll)
                      )
                      []

        tuple left' pre post cmds =
            noRegion $ ElmFormat.AST.Expression.Tuple
                [ ElmFormat.AST.Commented [] left' pre
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
    ElmFormat.AST.Expression.Expr
    -> [(ElmFormat.AST.Comments, ElmFormat.AST.Variable.Ref, ElmFormat.AST.Comments, ElmFormat.AST.Expression.Expr)]
    -> (ElmFormat.AST.Expression.Expr, [(ElmFormat.AST.Comments, ElmFormat.AST.Variable.Ref, ElmFormat.AST.Comments, ElmFormat.AST.Expression.Expr)])
removeMod left ops =
    let
        tuple left' pre post right' =
            noRegion $ ElmFormat.AST.Expression.App
                (noRegion $ ElmFormat.AST.Expression.VarExpr $ ElmFormat.AST.Variable.OpRef (SymbolIdentifier "%"))
                [ (pre, left')
                , (post, right')
                ]
                (ElmFormat.AST.FAJoinFirst ElmFormat.AST.JoinAll)


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
    -> (ElmFormat.AST.Expression.Expr -> ElmFormat.AST.Comments -> ElmFormat.AST.Comments -> ElmFormat.AST.Expression.Expr -> ElmFormat.AST.Expression.Expr)
    -> ElmFormat.AST.Expression.Expr
    -> Reversed (ElmFormat.AST.Comments, ElmFormat.AST.Variable.Ref, ElmFormat.AST.Comments, ElmFormat.AST.Expression.Expr)
    -> [(ElmFormat.AST.Comments, ElmFormat.AST.Variable.Ref, ElmFormat.AST.Comments, ElmFormat.AST.Expression.Expr)]
    -> (ElmFormat.AST.Expression.Expr, [(ElmFormat.AST.Comments, ElmFormat.AST.Variable.Ref, ElmFormat.AST.Comments, ElmFormat.AST.Expression.Expr)])
binopToFunction target shouldFold applyFn left preBang remaining =
    case remaining of
        [] ->
            (left, ReversedList.toList preBang)

        (pre, ElmFormat.AST.Variable.OpRef sym, post, cmds):rest | sym == target ->
            let
                left' =
                    case ReversedList.isEmpty preBang of
                        True -> left
                        False -> ( noRegion $ ElmFormat.AST.Expression.Binops left (ReversedList.toList preBang) False)

                (right', rest') =
                    collectRight (collectRightFold . shouldFold) cmds ReversedList.empty rest

                tuple =
                    applyFn left' pre post right'
            in
            binopToFunction target shouldFold applyFn tuple ReversedList.empty rest'

        (pre, op@(ElmFormat.AST.Variable.OpRef (SymbolIdentifier opi)), post, e):rest | collectLeftFold $ shouldFold opi ->
            binopToFunction target shouldFold applyFn left (ReversedList.push (pre, op, post, e) preBang) rest

        (pre, op, post, e):rest ->
            let
                (e', rest') = binopToFunction target shouldFold applyFn e ReversedList.empty rest
            in
            (left, ReversedList.toList preBang ++ (pre, op, post, e'):rest')

collectRight ::
    (String -> Bool)
    -> ElmFormat.AST.Expression.Expr
    -> Reversed (ElmFormat.AST.Comments, ElmFormat.AST.Variable.Ref, ElmFormat.AST.Comments, ElmFormat.AST.Expression.Expr)
    -> [(ElmFormat.AST.Comments, ElmFormat.AST.Variable.Ref, ElmFormat.AST.Comments, ElmFormat.AST.Expression.Expr)]
    -> (ElmFormat.AST.Expression.Expr, [(ElmFormat.AST.Comments, ElmFormat.AST.Variable.Ref, ElmFormat.AST.Comments, ElmFormat.AST.Expression.Expr)])
collectRight shouldFold leftMost collectedLeft rest =
    case rest of
        (pre, op@(ElmFormat.AST.Variable.OpRef (SymbolIdentifier opi)), post, e):rest' | shouldFold opi ->
            collectRight shouldFold leftMost (ReversedList.push (pre, op, post, e) collectedLeft) rest'

        _ ->
            -- terminate if either rest is empty, or the next op fails the shouldFold test
            ( case ReversedList.isEmpty collectedLeft of
                True -> leftMost
                False -> noRegion $ ElmFormat.AST.Expression.Binops leftMost (ReversedList.toList collectedLeft) False
            , rest
            )

formatRange_0_17 :: ElmVersion -> ImportInfo -> ElmFormat.AST.Commented ElmFormat.AST.Expression.Expr -> ElmFormat.AST.Commented ElmFormat.AST.Expression.Expr -> Bool -> Box
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

formatRange_0_18 :: ElmVersion -> ImportInfo -> ExpressionContext -> ElmFormat.AST.Commented ElmFormat.AST.Expression.Expr -> ElmFormat.AST.Commented ElmFormat.AST.Expression.Expr -> Box
formatRange_0_18 elmVersion importInfo context left right =
    case (left, right) of
        (ElmFormat.AST.Commented preLeft left' [], ElmFormat.AST.Commented preRight right' []) ->
            ElmFormat.AST.Expression.App
                (noRegion $ ElmFormat.AST.Expression.VarExpr $ ElmFormat.AST.Variable.VarRef [ElmFormat.AST.UppercaseIdentifier "List"] $ ElmFormat.AST.LowercaseIdentifier "range")
                [ (preLeft, left')
                , (preRight, right')
                ]
                (ElmFormat.AST.FAJoinFirst ElmFormat.AST.JoinAll)
                |> noRegion
                |> formatExpression elmVersion importInfo context

        _ ->
            ElmFormat.AST.Expression.App
                (noRegion $ ElmFormat.AST.Expression.VarExpr $ ElmFormat.AST.Variable.VarRef [ElmFormat.AST.UppercaseIdentifier "List"] $ ElmFormat.AST.LowercaseIdentifier "range")
                [ ([], noRegion $ ElmFormat.AST.Expression.Parens left)
                , ([], noRegion $ ElmFormat.AST.Expression.Parens right)
                ]
                (ElmFormat.AST.FAJoinFirst ElmFormat.AST.JoinAll)
                |> noRegion
                |> formatExpression elmVersion importInfo context


formatUnit :: Char -> Char -> ElmFormat.AST.Comments -> Box
formatUnit left right comments =
  case (left, comments) of
    (_, []) ->
      line $ punc (left : right : [])

    ('{', (ElmFormat.AST.LineComment _):_) ->
      surround left right $ prefix space $ stack1 $ map formatComment comments

    _ ->
      surround left right $
        case allSingles $ map formatComment comments of
          Right comments' ->
            line $ row $ List.intersperse space comments'

          Left comments' ->
            stack1 comments'


formatComments :: ElmFormat.AST.Comments -> Maybe Box
formatComments comments =
    case fmap formatComment comments of
        [] ->
            Nothing

        (first:rest) ->
            Just $ ElmStructure.spaceSepOrStack first rest


formatCommented_ :: Bool -> (a -> Box) -> ElmFormat.AST.Commented a -> Box
formatCommented_ forceMultiline format (ElmFormat.AST.Commented pre inner post) =
    ElmStructure.forceableSpaceSepOrStack1 forceMultiline $
        concat
            [ Maybe.maybeToList $ formatComments pre
            , [format inner]
            , Maybe.maybeToList $ formatComments post
            ]


formatCommented :: (a -> Box) -> ElmFormat.AST.Commented a -> Box
formatCommented =
  formatCommented_ False


-- TODO: rename to formatPreCommented
formatHeadCommented :: (a -> Box) -> (ElmFormat.AST.Comments, a) -> Box
formatHeadCommented format (pre, inner) =
    formatCommented' pre format inner


formatCommented' :: ElmFormat.AST.Comments -> (a -> Box) -> a -> Box
formatCommented' pre format inner =
    formatCommented format (ElmFormat.AST.Commented pre inner [])


formatTailCommented :: (a -> Box) -> (a, ElmFormat.AST.Comments) -> Box
formatTailCommented format (inner, post) =
  formatCommented format (ElmFormat.AST.Commented [] inner post)


formatEolCommented :: (a -> Box) -> ElmFormat.AST.WithEol a -> Box
formatEolCommented format (ElmFormat.AST.WithEol inner post) =
  case (post, format inner) of
    (Nothing, box) -> box
    (Just eol, SingleLine result) ->
      mustBreak $ row [ result, space, punc "--", literal eol ]
    (Just eol, box) ->
      stack1 [ box, formatComment $ ElmFormat.AST.LineComment eol ]


formatCommentedStack :: (a -> Box) -> ElmFormat.AST.Commented a -> Box
formatCommentedStack format (ElmFormat.AST.Commented pre inner post) =
  stack1 $
    (map formatComment pre)
      ++ [ format inner ]
      ++ (map formatComment post)


formatHeadCommentedStack :: (a -> Box) -> (ElmFormat.AST.Comments, a) -> Box
formatHeadCommentedStack format (pre, inner) =
  formatCommentedStack format (ElmFormat.AST.Commented pre inner [])


formatKeywordCommented :: String -> (a -> Box) -> ElmFormat.AST.KeywordCommented a -> Box
formatKeywordCommented word format (ElmFormat.AST.KeywordCommented pre post value) =
  ElmStructure.spaceSepOrIndented
    (formatCommented (line . keyword) (ElmFormat.AST.Commented pre word post))
    [ format value ]


formatOpenCommentedList :: (a -> Box) -> ElmFormat.AST.OpenCommentedList a -> [Box]
formatOpenCommentedList format (ElmFormat.AST.OpenCommentedList rest (preLst, lst)) =
    (fmap (formatCommented $ formatEolCommented format) rest)
        ++ [formatCommented (formatEolCommented format) $ ElmFormat.AST.Commented preLst lst []]


formatComment :: ElmFormat.AST.Comment -> Box
formatComment comment =
    case comment of
        ElmFormat.AST.BlockComment c ->
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

        ElmFormat.AST.LineComment c ->
            mustBreak $ row [ punc "--", literal c ]

        ElmFormat.AST.CommentTrickOpener ->
            mustBreak $ punc "{--}"

        ElmFormat.AST.CommentTrickCloser ->
            mustBreak $ punc "--}"

        ElmFormat.AST.CommentTrickBlock c ->
            mustBreak $ row [ punc "{--", literal c, punc "-}" ]


formatLiteral :: ElmVersion -> ElmFormat.AST.Literal -> Box
formatLiteral elmVersion lit =
    case lit of
        ElmFormat.AST.IntNum i ElmFormat.AST.DecimalInt ->
            line $ literal $ show i
        ElmFormat.AST.IntNum i ElmFormat.AST.HexadecimalInt ->
            line $ literal $
              if i <= 0xFF then
                printf "0x%02X" i
              else if i <= 0xFFFF then
                printf "0x%04X" i
              else if i <= 0xFFFFFFFF then
                printf "0x%08X" i
              else
                printf "0x%016X" i
        ElmFormat.AST.FloatNum f ElmFormat.AST.DecimalFloat ->
            line $ literal $ printf "%f" f
        ElmFormat.AST.FloatNum f ElmFormat.AST.ExponentFloat ->
            line $ literal $ printf "%e" f
        ElmFormat.AST.Chr c ->
            formatString elmVersion SChar [c]
        ElmFormat.AST.Str s multi ->
            formatString elmVersion (if multi then SMulti else SString) s
        ElmFormat.AST.Boolean b ->
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


formatType :: ElmVersion -> ElmFormat.AST.Type -> Box
formatType elmVersion =
    formatType' elmVersion NotRequired


commaSpace :: Line
commaSpace =
    row
        [ punc ","
        , space
        ]


formatTypeConstructor :: ElmVersion -> ElmFormat.AST.TypeConstructor -> Box
formatTypeConstructor elmVersion ctor =
    case ctor of
        ElmFormat.AST.NamedConstructor namespace name ->
            line $ formatQualifiedUppercaseIdentifier elmVersion (namespace ++ [name])

        ElmFormat.AST.TupleConstructor n ->
            line $ keyword $ "(" ++ (List.replicate (n-1) ',') ++ ")"


formatType' :: ElmVersion -> TypeParensRequired -> ElmFormat.AST.Type -> Box
formatType' elmVersion requireParens atype =
    case RA.drop atype of
        ElmFormat.AST.UnitType comments ->
          formatUnit '(' ')' comments

        ElmFormat.AST.FunctionType first rest (ElmFormat.AST.ForceMultiline forceMultiline) ->
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
                                      (ElmFormat.AST.Commented postOp (ElmFormat.AST.WithEol term eol) [])
                                  )
                              ]
                            ]
            in
                ElmStructure.forceableSpaceSepOrStack
                    forceMultiline
                    (formatEolCommented (formatType' elmVersion ForLambda) first)
                    (map formatRight rest)
                |> if requireParens /= NotRequired then parens else id

        ElmFormat.AST.TypeVariable var ->
            line $ identifier $ formatVarName elmVersion var

        ElmFormat.AST.TypeConstruction ctor args ->
            ElmStructure.application
                (ElmFormat.AST.FAJoinFirst ElmFormat.AST.JoinAll)
                (formatTypeConstructor elmVersion ctor)
                (map (formatHeadCommented $ formatType' elmVersion ForCtor) args)
                |> (if args /= [] && requireParens == ForCtor then parens else id)

        ElmFormat.AST.TypeParens type' ->
          parens $ formatCommented (formatType elmVersion) type'

        ElmFormat.AST.TupleType types ->
          ElmStructure.group True "(" "," ")" False (map (formatCommented (formatEolCommented $ formatType elmVersion)) types)

        ElmFormat.AST.RecordType base fields trailing multiline ->
            formatRecordLike
                (line . formatLowercaseIdentifier elmVersion [])
                (formatLowercaseIdentifier elmVersion [])
                ":"
                (formatType elmVersion)
                base fields trailing multiline


formatVar :: ElmVersion -> ElmFormat.AST.Variable.Ref -> Line
formatVar elmVersion var =
    case var of
        ElmFormat.AST.Variable.VarRef namespace name ->
            formatLowercaseIdentifier elmVersion namespace name

        ElmFormat.AST.Variable.TagRef namespace name ->
            case namespace of
                [] -> identifier $ formatVarName'' elmVersion name
                _ ->
                    row
                        [ formatQualifiedUppercaseIdentifier elmVersion namespace
                        , punc "."
                        , identifier $ formatVarName'' elmVersion name
                        ]

        ElmFormat.AST.Variable.OpRef name ->
            formatSymbolIdentifierInParens name


formatSymbolIdentifierInParens :: SymbolIdentifier -> Line
formatSymbolIdentifierInParens (SymbolIdentifier name) =
    identifier $ "(" ++ name ++ ")"


formatInfixVar :: ElmVersion -> ElmFormat.AST.Variable.Ref -> Line
formatInfixVar elmVersion var =
    case var of
        ElmFormat.AST.Variable.VarRef _ _ ->
            row [ punc "`"
                , formatVar elmVersion var
                , punc "`"
                ]
        ElmFormat.AST.Variable.TagRef _ _ ->
            row [ punc "`"
                , formatVar elmVersion var
                , punc "`"
                ]
        ElmFormat.AST.Variable.OpRef (SymbolIdentifier name) ->
            identifier name


formatLowercaseIdentifier :: ElmVersion -> [ElmFormat.AST.UppercaseIdentifier] -> ElmFormat.AST.LowercaseIdentifier -> Line
formatLowercaseIdentifier elmVersion namespace (ElmFormat.AST.LowercaseIdentifier name) =
    case (elmVersion, namespace, name) of
        (Elm_0_18_Upgrade, [], "fst") -> identifier "Tuple.first"
        (Elm_0_18_Upgrade, [ElmFormat.AST.UppercaseIdentifier "Basics"], "fst") -> identifier "Tuple.first"
        (Elm_0_18_Upgrade, [], "snd") -> identifier "Tuple.second"
        (Elm_0_18_Upgrade, [ElmFormat.AST.UppercaseIdentifier "Basics"], "snd") -> identifier "Tuple.second"
        (_, [], _) -> identifier $ formatVarName' elmVersion name
        _ ->
            row
                [ formatQualifiedUppercaseIdentifier elmVersion namespace
                , punc "."
                , identifier $ formatVarName' elmVersion name
                ]


formatUppercaseIdentifier :: ElmVersion -> ElmFormat.AST.UppercaseIdentifier -> Line
formatUppercaseIdentifier elmVersion (ElmFormat.AST.UppercaseIdentifier name) =
    identifier $ formatVarName' elmVersion name


formatQualifiedUppercaseIdentifier :: ElmVersion -> [ElmFormat.AST.UppercaseIdentifier] -> Line
formatQualifiedUppercaseIdentifier elmVersion names =
  identifier $ List.intercalate "." $
      map (\(ElmFormat.AST.UppercaseIdentifier name) -> formatVarName' elmVersion name) names


formatVarName :: ElmVersion -> ElmFormat.AST.LowercaseIdentifier -> String
formatVarName elmVersion (ElmFormat.AST.LowercaseIdentifier name) =
    formatVarName' elmVersion name


formatVarName' :: ElmVersion -> String -> String
formatVarName' elmVersion name =
    case elmVersion of
        Elm_0_18_Upgrade -> map (\x -> if x == '\'' then '_' else x) name
        _ -> name


formatVarName'' :: ElmVersion -> ElmFormat.AST.UppercaseIdentifier -> String
formatVarName'' elmVersion (ElmFormat.AST.UppercaseIdentifier name) =
    formatVarName' elmVersion name
