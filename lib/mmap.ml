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

  let get_exe t = proc_path ~filename:"exe" t

  let to_pid = function
    | Self pid | Other pid -> pid
  ;;
end

type t =
  { vma_offset_text : int64
  ; vma_offset_semaphores : int64
  ; start_of_text_segment : int64
  ; numa_maps : string
  ; maps : string
  }

let vma_offset_text t = t.vma_offset_text
let vma_offset_semaphores t = t.vma_offset_semaphores

(*=The following calculation gives the dynamic address of a symbol:
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

let try_find_vma_offset (mmap_entry : Owee_linux_maps.entry) (elf_section : Elf.section) =
  if mmap_entry.address_start < elf_section.addr || elf_section.offset < mmap_entry.offset
  then None
  else (
    let ( + ), ( - ) = Int64.(add, sub) in
    Some
      (mmap_entry.address_start
       - mmap_entry.offset
       + elf_section.offset
       - elf_section.addr))
;;

(* as part of [read] because we don't always need it. *)
external stub_sysconf_pagesize : unit -> int = "probes_lib_sysconf_pagesize"

let pagesize_in_bytes ~allow_gigatext t =
  assert (not allow_gigatext);
  stub_sysconf_pagesize ()
;;

let in_range x (entry : Owee_linux_maps.entry) =
  entry.address_start <= x && x < entry.address_end
;;

external is_prelinking_enabled : unit -> bool = "probes_lib_is_prelinking_enabled"

let print_for_debug maps =
  In_channel.with_open_text maps (fun maps_channel ->
    Printf.printf "%s\n\n" (In_channel.input_all maps_channel))
;;

(* Position independent executables and dynamically linked shared objects will offset each
   section from the address given in [Elf.t] by an arbitrary amount. This function
   computes those offsets for .text, .data, and .probes using their respective base
   addresses and current real locations.

   However, this support is limited because we do not have a robust way to correlate the
   base .text and .data sections with the current list of sections in /proc/maps -
   checking permissions and file paths is insufficient. The paths for the text and data
   sections may change to something arbitrary if they are remapped. Using [Unix.map_file]
   will add rw-x sections that can't be easily distinguished from the real .data section.
   This could be fixed by getting the addresses of caml__code_begin/caml__data_begin from
   C stubs. *)
let read_shared_object ~maps ~numa_maps (elf : Elf.t) entries =
  let filename = elf.filename in
  let prelinking_enabled = is_prelinking_enabled () in
  let semaphores_section =
    match elf.semaphores_section, prelinking_enabled with
    | Some s, _ -> Some s
    | None, true -> None
    | None, false ->
      raise (Failure (Printf.sprintf "No .probes section in %s" elf.filename))
  in
  let is_mapped_from_file (e : Owee_linux_maps.entry) =
    not (e.inode = 0L && e.device_major = 0 && e.device_minor = 0)
  in
  (* Heuristic to find text and data segment addresses for [filename]. *)
  let update p (e : Owee_linux_maps.entry) (elf_section : Elf.section) =
    match try_find_vma_offset e elf_section with
    | None ->
      (* There may be multiple r-x mappings for the shared object, because they can be
         split apart by the [mprotect] call in [stub_write_probe_sites].
         [try_find_vma_offset] will return [None] for all but the first one, so we can
         just ignore this case (which is reached for all the other r-x mappings). *)
      ()
    | Some vma_offset ->
      let dynamic_address_of_start_of_section = Int64.add vma_offset elf_section.addr in
      if not (in_range dynamic_address_of_start_of_section e)
      then
        raise
          (Error
             (Printf.sprintf
                "Unexpected format of %s: vma of section start is outside the segment \
                 for %s"
                maps
                filename));
      (match !p with
       | None -> ()
       | Some (prev, (prev_e : Owee_linux_maps.entry)) ->
         if not (prev_e.offset = 0L)
         then
           raise
             (Error
                (Printf.sprintf
                   "Unexpected format of %s: two segments with the same permissions for %s\n\
                    0x%Lx previously found\n\
                    0x%Lx from 0x%Lx\n"
                   maps
                   filename
                   prev
                   vma_offset
                   e.address_start)));
      p := Some (vma_offset, e)
  in
  let vma_offset_text = ref None in
  let vma_offset_semaphores = ref None in
  let start_of_text_segment = ref None in
  List.iter
    (fun (e : Owee_linux_maps.entry) ->
      if is_mapped_from_file e && String.equal filename e.pathname
      then (
        match e.perm_read, e.perm_write, e.perm_execute, e.perm_shared with
        | true, false, true, false ->
          update vma_offset_text e elf.text_section;
          start_of_text_segment := Some e.address_start
        | true, true, false, false ->
          (match semaphores_section with
           | Some semaphores_section -> update vma_offset_semaphores e semaphores_section
           | None -> ())
        | _ -> ()))
    entries;
  if Option.is_none !vma_offset_text
  then (
    print_for_debug maps;
    raise
      (Error
         (Printf.sprintf
            "Unexpected format of %s: missing executable segment for %s"
            maps
            filename)));
  if (not prelinking_enabled) && Option.is_none !vma_offset_semaphores
  then (
    print_for_debug maps;
    raise
      (Error
         (Printf.sprintf
            "Unexpected format of %s: missing write segment for %s"
            maps
            filename)));
  { vma_offset_text =
      (if prelinking_enabled then 0L else Option.get !vma_offset_text |> fst)
  ; vma_offset_semaphores =
      (if prelinking_enabled then 0L else Option.get !vma_offset_semaphores |> fst)
  ; start_of_text_segment = Option.get !start_of_text_segment
  ; numa_maps
  ; maps
  }
;;

(* for debug *)
let print_file name =
  Printf.printf "READING %s\n" name;
  let inc = In_channel.open_text name in
  let rec print () =
    match In_channel.input_line inc with
    | None -> print_newline ()
    | Some line ->
      Printf.printf "%s\n" line;
      print ()
  in
  print ();
  In_channel.close inc
;;

let get_start_of_text_segment maps (elf : Elf.t) entries =
  let text = elf.text_section.addr in
  match
    List.find_opt
      (fun (entry : Owee_linux_maps.entry) ->
        in_range text entry && entry.perm_read && entry.perm_execute)
      entries
  with
  | Some entry -> entry.address_start
  | None ->
    raise
      (Error
         (Printf.sprintf
            "Unexpected format of %s: cannot find text segment containing address 0x%Lx"
            maps
            text))
;;

let read ~pid (elf : Elf.t) =
  let numa_maps = Pid_or_self.proc_path ~filename:"numa_maps" pid in
  let maps = Pid_or_self.proc_path ~filename:"maps" pid in
  if !verbose then print_file maps;
  let entries = Owee_linux_maps.scan_file maps in
  let exe_inode =
    let fd =
      Unix.openfile (Pid_or_self.proc_path ~filename:"exe" pid) [ Unix.O_RDONLY ] 0
    in
    Fun.protect ~finally:(fun () -> Unix.close fd) (fun () -> (Unix.fstat fd).st_ino)
  in
  if elf.shared_object && elf.inode = exe_inode (* the executable is PIE *)
  then failwith "ocaml-probes: position-independent executables are not supported"
  else if elf.shared_object
  then read_shared_object ~maps ~numa_maps elf entries
  else
    { vma_offset_text = 0L
    ; vma_offset_semaphores = 0L
    ; start_of_text_segment = get_start_of_text_segment maps elf entries
    ; maps
    ; numa_maps
    }
;;
