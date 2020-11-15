
module Iteratee_orig = struct
  type ('a, 'b) iteratee =
    | Done of 'b
    | Cont of ('a -> (('a, 'b) iteratee * 'a))

  let return x = Done x

  let rec (>>=) self f =
    match self with
    | Done b -> f b
    | Cont k ->
      let docase = function
        | Done b, a ->
          begin
            match f b with
            | Cont k' -> k a
            | iter -> (iter, a)
          end
        | (iter, a) -> (iter >>= f, a)
      in
      Cont (fun a -> docase (k a))
end

