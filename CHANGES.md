## Release v0.17.0

- Calls to [Probes_lib.Self] now use `/proc/self` instead of `/proc/{getpid ()}`.
- Use process_write/read insead of ptrace to access semaphores.
  Bug fix for failures when accessing the last semaphore with ptrace.
- Use `mprotect` to update all probes on the same page instead
  of each probe site separately.
- Raise if no probes are specified when running [ocaml-probes {attach,trace}]
  instead of enabling everything.
