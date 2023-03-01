(** Memory map of a running process *)
exception Error of string

module Page_size : sig
  type t =
    | Smallpages
    | Hugepages
    | Gigapages

  val in_bytes : t -> int
end

type t =
  { vma_offset_text : int64
  ; vma_offset_data : int64
  ; vma_offset_semaphores : int64
  }

(** [read ~pid elf] reads /proc/pid/maps to determine the dynamic offsets of relocated
    .text, .data, and .probes sections in [pid]. These offsets are non-zero for position
    independent executables only. [pid] must be an instance of the program described by
    [elf].

    Requires permissions to read maps, such as calling this function from pid itself or a
    process attached to pid using ptrace, and pid should be stopped (or memory map might
    be modified by the OS during reading). *)
val read : pid:int -> Elf.t -> t

(** [get_text_page_size ~pid elf] reads /proc/pid/numa_maps to determine the size of pages
    backing the process' .text segment. [pid] must be an instance of the program described
    by [elf]. Returns [None] when unable to determine the page size.

    Requires permissions to read maps, such as calling this function from pid itself or a
    process attached to pid using ptrace, and pid should be stopped (or memory map might
    be modified by the OS during reading). *)
val get_text_page_size : pid:int -> Elf.t -> Page_size.t option

(** Control debug printing. *)
val verbose : bool ref
