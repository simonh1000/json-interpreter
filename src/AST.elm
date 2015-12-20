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
    | KV Command Command
    | Object (List Command)
    | At (List Command) Command
    | List Command
    | Arr Command
    | Tuple (List Command)
    | Custom Command      -- ignore the subsequent transformation
    | Map Command
    | OneOf (List Command)
    | AndThen Command Command      -- second Command will be a Proc
    | Succeed Command
    | MaybeCommand   -- no need to do anything here
    | KeyValuePairs
    -- | OneOf (List Command)

    | Var Name Command    -- only used in emit
    | Proc Name (List Param) Command
    | Call Name (List Command)    -- call procedure

    | Error String
    -- | Cntxt String
