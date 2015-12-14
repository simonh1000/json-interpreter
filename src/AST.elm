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
    -- | AndThen Command     -- Decoder a
    | Succeed Command
    | MaybeCommand   -- no need to do anything here
    | KeyValuePairs

    | Var Name Command    -- only used in emit
    | Proc Name (List Param) Command
    -- | P String            -- param
    | Call Name (List Command)    -- call procedure
    -- | Func Name Command

    | Error String
    | Cntxt String
