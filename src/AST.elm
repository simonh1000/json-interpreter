module AST (..) where

type alias Name = String
type alias Key = String
type alias Param = String
type alias Arg = String
    -- = Number Int
    -- | Arg Param


type Command
    = Str String
    | Strng
    | Itg
    | Flt
    | Bln
    | Null String
    | List Command
    | Arr Command
    | Tuple (List Command)
    | KV Command Command
    | At (List Command) Command
    | Object (List Command)
    | KeyValuePairs
    | Dict Command
    | JMaybe Command  -- no need to do anything here
    | OneOf (List Command)
    | Map Command
    | DFail Command          -- always a Str in practise
    | Succeed Command
    -- | AndThen Command Command      -- second Command will be a Proc
    | Custom Command      -- ignore the subsequent transformation
    -- | Value

    | Var Name Command    -- only used in emit
    | Proc Name (List Param) Command
    | Call Name (List Command)    -- call procedure

    | Error String    -- use Fail?????
