module AST (Command(..)) where

type alias Name = String
type alias Key = String
type alias Param = String
type alias Arg = String
    -- = Number Int
    -- | Arg Param

type Command
    = Strng
    | Itg
    | Flt
    | Bln
    | KV Key Command
    | Object (List Command)
    | At (List String) Command
    | List Command
    | Arr Command
    | Tuple (List Command)
    | Custom Command      -- ignore the subsequent transformation
    | Map Command
    | OneOf (List Command)
    | AndThen  Command     -- Decoder a
    | Succeed
    | MaybeCommand   -- no need to do anything here
    | KeyValuePairs

    | Proc Name (List Param) Command
    | Call Name (List Arg)    -- call procedure
    -- | Func Name Command

    | Error String
    | Cntxt String
