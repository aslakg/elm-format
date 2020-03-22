{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
module ElmFormat.Parse.Declaration where

import Text.Parsec ( (<|>), (<?>), choice, digit, optionMaybe, string, try )

import ElmFormat.AST.Declaration
import ElmVersion
import ElmFormat.Parse.Comments
import qualified ElmFormat.Parse.Expression as Expr
import ElmFormat.Parse.Helpers as Help
import qualified ElmFormat.Parse.Type as Type
import ElmFormat.AST.V0_16
import ElmFormat.Parse.IParser
import ElmFormat.Parse.Whitespace


declaration :: ElmVersion -> IParser Declaration
declaration elmVersion =
    typeDecl elmVersion <|> infixDecl elmVersion <|> port elmVersion <|> definition elmVersion


topLevelStructure :: IParser a -> IParser (TopLevelStructure a)
topLevelStructure entry =
    choice
        [ DocComment <$> docCommentAsMarkdown
        , Entry <$> addLocation entry
        ]



-- TYPE ANNOTATIONS and DEFINITIONS

definition :: ElmVersion -> IParser ElmFormat.AST.Declaration.Declaration
definition elmVersion =
    (Expr.typeAnnotation elmVersion TypeAnnotation <|> Expr.definition elmVersion Definition)
    <?> "a value definition"


-- TYPE ALIAS and UNION TYPES

typeDecl :: ElmVersion -> IParser ElmFormat.AST.Declaration.Declaration
typeDecl elmVersion =
  do  try (reserved elmVersion "type") <?> "a type declaration"
      postType <- forcedWS
      isAlias <- optionMaybe (string "alias" >> forcedWS)

      name <- capVar elmVersion
      args <- spacePrefix (lowVar elmVersion)
      (preEquals, _, postEquals) <- padded equals

      case isAlias of
        Just postAlias ->
            do  tipe <- Type.expr elmVersion <?> "a type"
                return $
                  ElmFormat.AST.Declaration.TypeAlias
                    postType
                    (Commented postAlias (name, args) preEquals)
                    (postEquals, tipe)

        Nothing ->
            do
                tags_ <- pipeSep1 (Type.tag elmVersion) <?> "a constructor for a union type"
                return
                    ElmFormat.AST.Declaration.Datatype
                        { nameWithArgs = Commented postType (name, args) preEquals
                        , tags = exposedToOpen postEquals tags_
                        }


-- INFIX


infixDecl :: ElmVersion -> IParser ElmFormat.AST.Declaration.Declaration
infixDecl elmVersion =
    expecting "an infix declaration" $
    choice
        [ try $ infixDecl_0_16 elmVersion
        , infixDecl_0_19 elmVersion
        ]


infixDecl_0_19 :: ElmVersion -> IParser ElmFormat.AST.Declaration.Declaration
infixDecl_0_19 elmVersion =
    let
        assoc =
            choice
                [ string "right" >> return ElmFormat.AST.Declaration.R
                , string "non" >> return ElmFormat.AST.Declaration.N
                , string "left" >> return ElmFormat.AST.Declaration.L
                ]
    in
    ElmFormat.AST.Declaration.Fixity_0_19
        <$> (try (reserved elmVersion "infix") *> preCommented assoc)
        <*> (preCommented $ (\n -> read [n]) <$> digit)
        <*> (commented symOpInParens)
        <*> (equals *> preCommented (lowVar elmVersion))


infixDecl_0_16 :: ElmVersion -> IParser ElmFormat.AST.Declaration.Declaration
infixDecl_0_16 elmVersion =
  do  assoc <-
          choice
            [ try (reserved elmVersion "infixl") >> return ElmFormat.AST.Declaration.L
            , try (reserved elmVersion "infixr") >> return ElmFormat.AST.Declaration.R
            , try (reserved elmVersion "infix")  >> return ElmFormat.AST.Declaration.N
            ]
      digitComments <- forcedWS
      n <- digit
      opComments <- forcedWS
      ElmFormat.AST.Declaration.Fixity assoc digitComments (read [n]) opComments <$> anyOp elmVersion


-- PORT

port :: ElmVersion -> IParser ElmFormat.AST.Declaration.Declaration
port elmVersion =
  expecting "a port declaration" $
  do  try (reserved elmVersion "port")
      preNameComments <- whitespace
      name <- lowVar elmVersion
      postNameComments <- whitespace
      let name' = Commented preNameComments name postNameComments
      choice [ portAnnotation name', portDefinition name' ]
  where
    portAnnotation name =
      do  try hasType
          typeComments <- whitespace
          tipe <- Type.expr elmVersion <?> "a type"
          return (ElmFormat.AST.Declaration.PortAnnotation name typeComments tipe)

    portDefinition name =
      do  try equals
          bodyComments <- whitespace
          expr <- Expr.expr elmVersion
          return (ElmFormat.AST.Declaration.PortDefinition name bodyComments expr)
