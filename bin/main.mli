val attach
  :  pid:int
  -> bpf:bool
  -> actions:Probes_lib.actions
  -> force:bool
  -> allow_gigatext:bool
  -> unit

val trace
  :  prog:string
  -> args:string list
  -> bpf:bool
  -> actions:Probes_lib.actions
  -> allow_gigatext:bool
  -> unit

val info : pid:int -> bpf:bool -> unit
