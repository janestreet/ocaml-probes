Test startup breakpoint handling independently of the build's prelinking setting.
The C tracee defines [prelinking_startup] and [caml_main] to select the prelinked
startup path in [With_ptrace.start], without needing any prelinked libraries.

  $ cat > tracee.c <<'EOF'
  > #include <signal.h>
  > #include <stdio.h>
  > #include <stdlib.h>
  > #include <unistd.h>
  > static void handle_trap(int sig) {
  >   (void)sig;
  >   const char message[] = "startup SIGTRAP handled\n";
  >   if (write(STDOUT_FILENO, message, sizeof(message) - 1) != sizeof(message) - 1)
  >     _exit(2);
  > }
  > void prelinking_startup(void) {
  >   if (getenv("PROBES_TEST_STARTUP_TRAP") == NULL) return;
  >   struct sigaction action = {0}, previous;
  >   action.sa_handler = handle_trap;
  >   sigemptyset(&action.sa_mask);
  >   if (sigaction(SIGTRAP, &action, &previous) != 0 ||
  >       raise(SIGTRAP) != 0 ||
  >       sigaction(SIGTRAP, &previous, NULL) != 0)
  >     abort();
  > }
  > void caml_main(void) { puts("reached caml_main"); }
  > int main(void) {
  >   prelinking_startup();
  >   caml_main();
  >   return 0;
  > }
  > EOF

Keep the call to [caml_main] uninlined and its address fixed. Like the prelinker, export
symbols to [.dynsym], where [probes_lib] looks them up.

  $ gcc -O0 -fno-pie -no-pie -rdynamic -Wall -Wextra -Werror tracee.c -o tracee
  $ tracer="$TESTDIR/test_trace.exe"
  $ "$tracer" ./tracee
  reached caml_main

An unrelated SIGTRAP before [caml_main] must reach its handler, not be mistaken for
the breakpoint.

  $ PROBES_TEST_STARTUP_TRAP=1 "$tracer" ./tracee
  startup SIGTRAP handled
  reached caml_main

Retry an interrupted wait for the breakpoint. Without [-f], strace traces only the
tracer, so the child remains available for [probes_lib] to trace. The first wait is for
exec; the second is the wait inside [run_to_address].

  $ strace -o interrupted.log -e inject=wait4:error=EINTR:when=2 "$tracer" ./tracee
  reached caml_main
  $ grep -o 'EINTR.*(INJECTED)' interrupted.log
  EINTR (Interrupted system call) (INJECTED)

If continuing fails after planting the breakpoint, kill and reap the child before
raising. The tracer's third ptrace call is CONT, after PEEKTEXT and POKETEXT.

  $ strace -o failed.log -e trace=ptrace,kill,wait4 -e inject=ptrace:error=EIO:when=3 "$tracer" ./tracee > /dev/null 2>&1
  [2]
  $ grep -q 'EIO.*(INJECTED)' failed.log
  $ sed -n '/^kill(/,$p' failed.log | grep -E '^(kill|wait4)\(' | sed -E 's/\b[1-9][0-9]*\b/PID/g; s/ += / = /'
  kill(PID, SIGKILL) = 0
  wait4(PID, NULL, 0, NULL) = PID

[.dynsym] survives stripping.

  $ gcc -O0 -fno-pie -no-pie -rdynamic -s tracee.c -o stripped-tracee
  $ "$tracer" ./stripped-tracee
  reached caml_main

Ordinary executables must still work. Rename the prelinking marker so that this
executable is not identified as prelinked.

  $ gcc -O0 -fno-pie -no-pie -rdynamic -s -Dprelinking_startup=ordinary_startup tracee.c -o stripped-ordinary-tracee
  $ "$tracer" ./stripped-ordinary-tracee
  reached caml_main

If a prelinked executable has no [caml_main] symbol, report it before starting the child.

  $ gcc -O0 -fno-pie -no-pie -rdynamic -s -Dcaml_main=other_main tracee.c -o missing-caml-main
  $ OCAMLRUNPARAM=b=0 "$tracer" ./missing-caml-main
  Fatal error: exception Probes_lib.Error("Cannot trace prelinked executable ./missing-caml-main: no caml_main symbol")
  [2]
