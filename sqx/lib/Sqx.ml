type 'a typ =
  | Int : int typ
  | Float : float typ
  | String : string typ
  | Bool : bool typ
  | Null : 'a typ -> 'a option typ

let int = Int
let float = Float
let string = String
let bool = Bool
let null typ = Null typ

type field = Field : { typ : 'a typ } -> field [@@unboxed]

let field typ = Field { typ }

type schema = { fields : field list }

let schema fields = { fields }

module Schema = struct
  type t = schema

  let fields schema = schema.fields
end
