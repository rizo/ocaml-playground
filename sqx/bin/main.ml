(*
CREATE TABLE incoming (
    project_id bigint NOT NULL,
    -- message_id must be generated externally for all values in
    -- the same message and be synced with incoming_latest
    message_id varchar(12) NOT NULL,
    device_id bigint NOT NULL,
    -- created_time must be generated externally to be in sync with latest
    created_time timestamp NOT NULL,
    schema_version bigint NOT NULL,
    slot int4 NOT NULL,
    value jsonb NOT NULL,
    FOREIGN KEY (project_id, device_id) REFERENCES devices (project_id, id),
    FOREIGN KEY (schema_version) REFERENCES schemas(version)
) PARTITION BY HASH (project_id);
*)

let incoming =
  Sqx.schema
    [
      Sqx.field Sqx.int;
      Sqx.field Sqx.string;
      Sqx.field Sqx.int;
      Sqx.field Sqx.int;
      Sqx.field Sqx.int;
      Sqx.field Sqx.int;
      Sqx.field Sqx.bool;
    ]

let () = print_endline "Hello, World!"
