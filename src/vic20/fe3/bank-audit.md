FE3 bank-visibility audit
=========================

Reviewed the assembled FE3 bank layout, direct symbol references across
segments, pointers passed to other banks, and explicit bank-register writes.
This includes the shared routines, source and symbol-name bank mirrors,
viewers, monitor, expression/float code, linker/object logging, and native
execution transitions.

Confirmed issues and fixes:

| Path | Problem | Fix |
| --- | --- | --- |
| Symbol viewer | EXPR patched MAIN's format address in its own BLK3. Byte-valued symbols left the renderer expecting a word, corrupting the argument/return stack. | Patch the format through a MAIN-bank helper. |
| Monitor tracing message | MAIN's string was passed directly to monitor-bank history/file output. | Render into shared RAM before switching banks. |
| Monitor missing redirect filename | The monitor read MAIN's string while its own bank was mapped. | Render into shared RAM first. |
| Monitor invalid-instruction fallback | The `???` string had the same pointer-lifetime problem. | Render into shared RAM first. |
| Simulator native-handler boundary | Simulated stores still rejected `$7f80–$7fdf` after the native handler moved. | Protect `$7fe0–$7fff`, matching the virtual-memory API. |

The other reviewed direct references either stay in their owning bank or
execute from shared RAM with the required bank explicitly selected. Reviewed
banked filename, directory-row, and linker/object log paths already copy text
to shared/internal RAM before their consumers change banks. Source and
symbol-name loops use deliberately duplicated BLK5 code and restore the
expected bank before accessing private state.

All 17 FE3 regression tests pass. Boot relocation, native RESTORE, the symbol
viewer, and monitor messages were also rechecked against the current boot
loader changes. Validation covers real `mac.s` assembly followed by opening/sorting the viewer,
alternating zero-page/absolute/float symbol rendering with stack checks,
monitor output redirection, and simulated stores on both sides of `$7fe0`.
The full Blue Star example and its includes also assembled successfully
(383 symbols); its viewer opened, scrolled, and changed sort order while
preserving the stack and return bank. These are CPU-level cartridge tests
with file-I/O hooks, not a hardware timing validation.
