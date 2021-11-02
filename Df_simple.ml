module type Dataframe = sig
  type t

  type row

  type column

  val empty : t

  val add_row : t -> row -> t

  val add_column : t -> column -> t

  val bind_columns : t -> t -> t

  val bind_rows : t -> t -> t

  (** {1 Join rows} *)

  val join :
    on:column -> (row option -> row option -> row option) -> t -> t -> t
  (** [left_join df1 df2] includes all rows in [df1]. *)

  val join_left : on:column -> t -> t -> t
  (** [left_join df1 df2] includes all rows in [df1]. *)

  val join_right : on:column -> t -> t -> t
  (** [join_right df1 df2] includes all rows in [df2]. *)

  val join_inner : t -> t -> t
  (** [inner_join df1 df2] includes all rows in [df1] and [df2]. *)

  val join_full : t -> t -> t
  (** [join_full df1 df2] includes all rows in [df1] or [df2]. *)
end
