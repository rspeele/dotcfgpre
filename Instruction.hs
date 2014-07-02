{-# LANGUAGE OverloadedStrings #-}
module Instruction
    ( DynamicAlias(..)
    , StaticAlias(..)
    , Instruction(..)
    , assemble
    , assembleMany
    ) where
import CoreTypes
import Data.Char (isSpace)
import Data.List (intersperse)
import qualified Data.ByteString.Char8 as B

newtype DynamicAlias = DynamicAlias Int deriving (Show, Read, Eq, Ord)

nameDynamic :: DynamicAlias -> ByteString
nameDynamic (DynamicAlias id) = B.concat ["_d", B.pack $ show id ]

newtype StaticAlias = StaticAlias Int deriving (Show, Read, Eq, Ord)

nameStatic :: StaticAlias -> ByteString
nameStatic (StaticAlias id) = B.concat ["_s", B.pack $ show id ]

data Instruction
    = InvokeDynamic DynamicAlias
    | InvokeStatic StaticAlias
    | AssignDynamic DynamicAlias Instruction
    | AssignStatic StaticAlias [Instruction]
    | AssignName AliasName [Instruction]
    | BindKey KeyName [Instruction]
    | InvokeRaw ByteString [ByteString]
      deriving (Show, Read, Eq, Ord)

assembleAssignment :: ByteString -> ByteString -> [Instruction] -> ByteString
assembleAssignment command name code =
    B.concat [command, " ", name, " ", args ]
        where
          args =
              case code of
                [single] -> assemble single
                many -> B.concat ["\"", B.concat $ intersperse ";" $ map assemble many, "\""]

alias = assembleAssignment "alias"
bind = assembleAssignment "bind"

raw :: ByteString -> [ByteString] -> ByteString
raw cmd args =
    B.concat $ intersperse " " $ cmd : map argStr args
        where
          argStr a
              | B.any isSpace a = B.concat ["\"", a, "\""]
              | otherwise = a

assemble :: Instruction -> ByteString
assemble ins =
    case ins of
      InvokeDynamic d -> nameDynamic d
      InvokeStatic s -> nameStatic s
      AssignDynamic d i -> alias (nameDynamic d) [i]
      AssignStatic s i -> alias (nameStatic s) i
      AssignName n i -> alias n i
      BindKey k i -> bind k i
      InvokeRaw cmd args -> raw cmd args

assembleMany :: [Instruction] -> ByteString
assembleMany = B.concat . intersperse "\n" . map assemble