module Pfsm.Nim

import Data.List
import Data.Strings
import Pfsm
import Pfsm.Data

export
nimBuiltinTypes : List String
nimBuiltinTypes = [ "int"
                  , "int8"
                  , "int16"
                  , "int32"
                  , "int64"
                  , "uint"
                  , "uint8"
                  , "uint16"
                  , "uint32"
                  , "uint64"
                  , "float"
                  , "float32"
                  , "float64"
                  , "true"
                  , "false"
                  , "char"
                  , "string"
                  , "cstring"
                  ]

export
nimKeywords : List String
nimKeywords = [ "addr"
              , "and"
              , "as"
              , "asm"
              , "bind"
              , "block"
              , "break"
              , "case"
              , "cast"
              , "concept"
              , "const"
              , "continue"
              , "converter"
              , "defer"
              , "discard"
              , "distinct"
              , "div"
              , "do"
              , "elif"
              , "else"
              , "end"
              , "enum"
              , "except"
              , "export"
              , "finally"
              , "for"
              , "from"
              , "func"
              , "if"
              , "import"
              , "in"
              , "include"
              , "interface"
              , "is"
              , "isnot"
              , "iterator"
              , "let"
              , "macro"
              , "method"
              , "mixin"
              , "mod"
              , "nil"
              , "not"
              , "notin"
              , "object"
              , "of"
              , "off"
              , "or"
              , "out"
              , "proc"
              , "ptr"
              , "raise"
              , "ref"
              , "return"
              , "shl"
              , "shr"
              , "static"
              , "template"
              , "try"
              , "tuple"
              , "type"
              , "using"
              , "var"
              , "when"
              , "while"
              , "xor"
              , "yield"
              ]

export
primToNimType : PrimType -> String
primToNimType PTBool   = "bool"
primToNimType PTByte   = "uint8"
primToNimType PTChar   = "char"
primToNimType PTShort  = "int16"
primToNimType PTUShort = "uint16"
primToNimType PTInt    = "int"
primToNimType PTUInt   = "uint"
primToNimType PTLong   = "int64"
primToNimType PTULong  = "uint64"
primToNimType PTReal   = "float64"
primToNimType PTString = "string"

export
toNimName : Name -> String
toNimName n
  = let n' = normalize n in
        if elemBy (==) n' nimKeywords
           then "my_" ++ n'
           else n'
  where
    mappings : List (String, String)
    mappings = [ (" ", "_")
               , ("-", "_")
               , ("+", "plus")
               ]
    normalize : Name -> String
    normalize n = foldl (\acc, x => replaceAll (fst x) (snd x) acc) n mappings

export
toNimFuncName : Name -> String
toNimFuncName "+" = "`+`"
toNimFuncName "-" = "`-`"
toNimFuncName "*" = "`*`"
toNimFuncName "/" = "`/`"
toNimFuncName n   = toNimName n

export
toNimType : Tipe -> String
toNimType TUnit                                 = "void"
toNimType (TPrimType t)                         = primToNimType t
toNimType (TList t)                             = "seq[" ++ (toNimType t) ++ "]"
toNimType (TDict PTString (TPrimType PTString)) = "StringTableRef"
toNimType (TDict k v)                           = "TableRef[" ++ (primToNimType k) ++ ", " ++ (toNimType v) ++ "]"
toNimType (TRecord n _)                         = camelize n
toNimType t@(TArrow a b)                        = case liftArrowParams t [] of
                                                       []      => toNimFuncType []           TUnit
                                                       x :: xs => toNimFuncType (reverse xs) x
                                                where
                                                  liftArrowParams : Tipe -> List Tipe -> List Tipe
                                                  liftArrowParams (TArrow a b@(TArrow _ _)) acc = liftArrowParams b (a :: acc)
                                                  liftArrowParams (TArrow a b)              acc = b :: (a :: acc)
                                                  liftArrowParams _                         acc = acc

                                                  toNimFuncType : List Tipe -> Tipe -> String
                                                  toNimFuncType as r
                                                    = let args = join ", " (map (\(i, x) => "a" ++ (show i) ++ ": " ++ toNimType(x)) (enumerate as))
                                                          ret  = toNimType r in
                                                          "proc (" ++ args ++ "): " ++ ret

export
toNimModelAttribute : String -> String
toNimModelAttribute "@" = "model"
toNimModelAttribute a   = if isPrefixOf "@" a
                             then "model." ++ toNimName (substr 1 (minus (length a) 1) a)
                             else toNimName a

export
toNimExpression : String -> Expression -> String
toNimExpression "fsm.guard_delegate" (ApplicationExpression n es) = "fsm.guard_delegate" ++ "." ++ (toNimFuncName n) ++ "(" ++ (join ", " (map (toNimExpression "fsm.guard_delegate") ((IdentifyExpression "model") :: es))) ++ ")"
toNimExpression caller               (ApplicationExpression n es) = caller ++ "." ++ (toNimFuncName n) ++ "(" ++ (join ", " (map (toNimExpression caller) es)) ++ ")"
toNimExpression _                    (BooleanExpression True)     = "true"
toNimExpression _                    (BooleanExpression False)    = "false"
toNimExpression _                    (IdentifyExpression i)       = toNimModelAttribute i
toNimExpression _                    (IntegerLiteralExpression i) = show i
toNimExpression _                    (RealLiteralExpression r)    = show r
toNimExpression _                    (StringLiteralExpression s)  = "\"" ++ s ++ "\""

export
toNimCompareOperation : CompareOperation -> String
toNimCompareOperation NotEqualsToOperation         = "!="
toNimCompareOperation EqualsToOperation            = "=="
toNimCompareOperation LessThanOperation            = "<"
toNimCompareOperation LessThanOrEqualsToOperation  = "<="
toNimCompareOperation GreatThanOperation           = ">"
toNimCompareOperation GreatThanOrEqualsToOperation = ">="

export
toNimTestExpression : String -> TestExpression -> String
toNimTestExpression caller (PrimitiveTestExpression e)     = toNimExpression caller e
toNimTestExpression caller (BinaryTestExpression op e1 e2) = (toNimTestExpression caller e1) ++ " " ++ (show op) ++ " " ++ (toNimTestExpression caller e2)
toNimTestExpression caller (UnaryTestExpression op e)      = (show op) ++ " " ++ (toNimTestExpression caller e)
toNimTestExpression caller (CompareExpression op e1 e2)    = (toNimExpression caller e1) ++ " " ++ (toNimCompareOperation op) ++ " " ++ (toNimExpression caller e2)

export
toNimFromJson : String -> Tipe -> String
toNimFromJson s (TPrimType PTBool)                                     = s ++ ".getBool"
toNimFromJson s (TPrimType PTByte)                                     = "cast[uint8](" ++ s ++ ".getInt)"
toNimFromJson s (TPrimType PTShort)                                    = "cast[int16](" ++ s ++ ".getInt)"
toNimFromJson s (TPrimType PTUShort)                                   = "cast[uint16](" ++ s ++ ".getInt)"
toNimFromJson s (TPrimType PTInt)                                      = s ++ ".getInt"
toNimFromJson s (TPrimType PTUInt)                                     = "cast[uint](" ++ s ++ ".getInt)"
toNimFromJson s (TPrimType PTLong)                                     = s ++ ".getStr(\"0\").parseBiggestInt"
toNimFromJson s (TPrimType PTULong)                                    = s ++ ".getStr(\"0\").parseBiggestUInt"
toNimFromJson s (TPrimType PTReal)                                     = s ++ ".getFloat"
toNimFromJson s (TPrimType PTChar)                                     = "if len(" ++ s ++ ".getStr) > 0: " ++ s ++ ".getStr()[0] else: '\\0'"
toNimFromJson s (TPrimType PTString)                                   = s ++ ".getStr"
toNimFromJson s (TList t)                                              = s ++ ".getElems.mapIt(" ++ (toNimFromJson "it" t) ++ ")"
toNimFromJson s (TDict PTString (TPrimType PTString))                  = s ++ ".jsonToStringTable"
toNimFromJson s (TDict PTString t@(TPrimType _))                       = s ++ ".jsonToPrimitiveTable[" ++ (toNimType t) ++ "]()"
toNimFromJson s (TDict PTString (TList t))                             = s ++ ".jsonToSeqTable[" ++ (toNimType t) ++ "]()"
toNimFromJson s (TDict PTString (TDict PTString (TPrimType PTString))) = s ++ ".jsonToDictStringTable()"
toNimFromJson s (TDict PTString (TDict PTString t@(TPrimType _)))      = s ++ ".jsonToDictTable[" ++ (toNimType t) ++ "]()"
toNimFromJson s (TRecord n _)                                          = s ++ ".jsonTo" ++ (camelize n)
toNimFromJson s _                                                      = s

export
toNimToJson : String -> Tipe -> String
toNimToJson s (TPrimType PTLong)          = "% ($" ++ s ++ ")"
toNimToJson s (TPrimType PTULong)         = "% ($" ++ s ++ ")"
toNimToJson s (TList (TPrimType PTLong))  = "% (" ++ s ++ ".mapIt($it))"
toNimToJson s (TList (TPrimType PTULong)) = "% (" ++ s ++ ".mapIt($it))"
toNimToJson s (TList (TRecord n ps))      = "% (" ++ s ++ ".mapIt(" ++ (camelize n) ++ "ToJson(it)" ++ "))"
toNimToJson s (TRecord n ps)              = (camelize n) ++ "ToJson(" ++ s ++ ")"
toNimToJson s _                           = "% " ++ s


export
toNimFromString : String -> Tipe -> String
toNimFromString s (TPrimType PTBool)   = s ++ ".parseBool"
toNimFromString s (TPrimType PTByte)   = "cast[uint8](" ++ s ++ ".parseInt)"
toNimFromString s (TPrimType PTShort)  = "cast[int16](" ++ s ++ ".parseInt)"
toNimFromString s (TPrimType PTUShort) = "cast[uint16](" ++ s ++ ".parseUInt)"
toNimFromString s (TPrimType PTInt)    = s ++ ".parseInt"
toNimFromString s (TPrimType PTUInt)   = s ++ ".parseUInt"
toNimFromString s (TPrimType PTLong)   = s ++ ".parseBiggestInt"
toNimFromString s (TPrimType PTULong)  = s ++ ".parseBiggestUInt"
toNimFromString s (TPrimType PTReal)   = s ++ ".parseFloat"
toNimFromString s (TPrimType PTChar)   = "if len(" ++ s ++ ") > 0: " ++ s ++ "[0] else: '\\0'"
toNimFromString s (TPrimType PTString) = s
toNimFromString s t@(TList _)          = toNimFromJson (s ++ ".parseJson()") t
toNimFromString s t@(TDict PTString _) = toNimFromJson (s ++ ".parseJson()") t
toNimFromString s t@(TRecord n _)      = toNimFromJson (s ++ ".parseJson()") t
toNimFromString s _                    = s

export
toNimString : Name -> Tipe -> String
toNimString n (TPrimType PTString) = n
toNimString n (TPrimType _)        = "$ " ++ n
toNimString n t@(TList _)          = "$ " ++ (toNimToJson n t)
toNimString n t@(TDict _ _)        = "$ " ++ (toNimToJson n t)
toNimString n t@(TRecord _ _)      = "$ " ++ (toNimToJson n t)
toNimString n _                    = n
