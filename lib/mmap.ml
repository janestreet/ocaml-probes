exception Error of string

let verbose = ref false

module Page_size = struct
  type t =
    | Smallpages
    | Hugepages
    | Gigapages

  let in_kb = function
    | Smallpages -> 4
    | Hugepages -> 2 * 1024
    | Gigapages -> 1024 * 1024
  ;;

  let in_bytes t = in_kb t * 1024
  let all = [ Smallpages; Hugepages; Gigapages ]
  let of_kb n = List.find_opt (fun t -> Int.equal n (in_kb t)) all
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

let in_range x a b =
  match Int64.compare a x, Int64.compare x b with
  | (-1 | 0), (-1 | 0) -> true
  | _ -> false
;;

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

let parse_page_size size =
  let size =
    match String.split_on_char '=' size with
    | [ "kernelpagesize_kB"; n ] -> int_of_string_opt n
    | _ -> None
  in
  match size with
  | Some n -> Page_size.of_kb n
  | None -> failwith "Probes-lib could not parse kernelpagesize_kB attribute."
;;

let get_start_of_text_segment ~pid (elf : Elf.t) =
  let text = Int64.add elf.text_section.addr (read ~pid elf).vma_offset_text in
  let mmaps = Owee_linux_maps.scan_pid pid in
  match
    List.find_opt
      (fun (entry : Owee_linux_maps.entry) ->
         in_range text entry.address_start entry.address_end)
      mmaps
  with
  | Some entry ->
    assert (entry.perm_read && entry.perm_execute);
    entry.address_start
  | None -> failwith "Probes-lib could not find .text segment in /proc/pid/maps."
;;

let get_text_page_size ~pid (elf : Elf.t) =
  let numa_maps = Printf.sprintf "/proc/%d/numa_maps" pid in
  (* Numa_maps is not created if hugepages are not enabled. *)
  if Sys.file_exists numa_maps
  then (
    let text = get_start_of_text_segment ~pid elf in
    let inc = In_channel.open_text numa_maps in
    (* The numa_maps format is described here:
       https://man7.org/linux/man-pages/man5/numa_maps.5.html.
       Each line contains a base address, memory policy, and list of attributes.
       The attribute "huge" will be included when hugepages are present.
       When using lib/segment_remapper, our configuration additionally includes the
       attribute "kernelpagesize_kB=N", which specifies the size of backing pages. *)
    let rec parse () =
      match In_channel.input_line inc with
      | Some line ->
        (match String.split_on_char ' ' line with
         | addr :: _policy :: attrs ->
           (match Int64.of_string_opt ("0x" ^ addr) with
            | Some addr when addr = text ->
              let pagesize =
                List.find_opt (String.starts_with ~prefix:"kernelpagesize_kB=") attrs
              in
              (match pagesize with
               | Some size -> parse_page_size size
               | None ->
                 if List.exists (String.equal "huge") attrs
                 then (* Hugepages are in use, but we do not know their size. *) None
                 else Some Smallpages)
            | _ -> parse ())
         | _ -> failwith ("Probes-lib got unexpected line in " ^ numa_maps))
      | None -> failwith ("Probes-lib could not find .text segment in " ^ numa_maps)
    in
    parse ())
  else Some Smallpages
;;
