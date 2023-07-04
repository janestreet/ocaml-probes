exception Error of string

let verbose = ref false

module Pid_or_self = struct
  type t =
    | Self of int
    | Other of int

  let self () = Self (Unix.getpid ())

  let of_pid pid =
    match pid = Unix.getpid () with
    | true -> Self pid
    | false -> Other pid
  ;;

  let proc_path ~filename = function
    | Self _ -> "/proc/self/" ^ filename
    | Other pid -> Printf.sprintf "/proc/%d/%s" pid filename
  ;;

  let get_exe t = proc_path ~filename:"exe" t |> Unix.readlink

  let to_pid = function
    | Self pid | Other pid -> pid
  ;;
end

type entry =
  { addr : int64 (** start address of the segment *)
  ; offset : int64 (** file offset *)
  }

type t =
  { vma_offset_text : int64
  ; vma_offset_data : int64
  ; vma_offset_semaphores : int64
  }

let default = { vma_offset_text = 0L; vma_offset_data = 0L; vma_offset_semaphores = 0L }

(* The following calculation give the dynamic address of a symbol:
   sym_dynamic_addr
   "symbol's dynamic address"
   = "segment start"
   + "offset of symbol's static address from the start of its section"
   + "offset of its section from the base of the segment's offset in the file"
   = "segment start"
   + "symbol's static address" - "section start"
   + "section offset into the file" - "segment offset into the file"
   = seg_addr + (sym_static_addr - sec_addr) + (sec_offset - seg_offset)
   = seg_addr + sec_offset  - seg_offset - sec_addr + sym_static_addr
   Read "segment start" and "segment offset into file" from mmap.
   The rest is known from reading elf file.
   Precompute the offset of dynamic address from static address
   for each type of symbol, making sure it doesn't over/underflow.
*)
let _vma_offset mmap_entry (elf_section : Elf.section) =
  if mmap_entry.addr < elf_section.addr || elf_section.offset < mmap_entry.offset
  then raise (Failure "Unexpected section sizes");
  Int64.sub
    (Int64.add (Int64.sub mmap_entry.addr mmap_entry.offset) elf_section.offset)
    elf_section.addr
;;

let read ~pid:_ (elf : Elf.t) =
  if elf.pie
  then
    failwith
      "Probes-lib has not implemented support for position independent executables.";
  default
;;
