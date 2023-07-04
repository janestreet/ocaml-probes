(** Memory map of a running process *)
exception Error of string

type t =
  { vma_offset_text : int64
  ; vma_offset_data : int64
  ; vma_offset_semaphores : int64
  }

module Pid_or_self : sig
  type t

  val self : unit -> t
  val of_pid : int -> t
  val proc_path : filename:string -> t -> string
  val get_exe : t -> string
  val to_pid : t -> int
end

(** [read ~pid elf] reads /proc/pid/maps to determine the dynamic offsets of relocated
    .text, .data, and .probes sections in [pid]. These offsets are non-zero for position
    independent executables only. [pid] must be an instance of the program described by
    [elf].

    Requires permissions to read maps, such as calling this function from pid itself or a
    process attached to pid using ptrace, and pid should be stopped (or memory map might
    be modified by the OS during reading). *)
val read : pid:Pid_or_self.t -> Elf.t -> t

(** Control debug printing. *)
val verbose : bool ref
