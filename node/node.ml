open Core

module type S = sig
  type 'a t

  val get : 'a t -> 'a
end

module type Context = sig
  include S

  type typ
  type context
  type nonrec 'a result = ('a, Type.error Position.error) result

  val context : 'a t -> context
  val typ : 'a t -> typ
  val make : 'a -> ctx:context -> typ:typ -> 'a t
end

module Position = struct
  type 'a t = {
    value : 'a;
    position : Position.t;
  }

  let get { value } = value
  let position { position } = position
  let make ~position value = { value; position }
end