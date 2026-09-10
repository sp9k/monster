## Errors

The table below lists the errors that Monster may report while assembling,
linking, debugging, or performing file I/O.  Each error has a numeric code and
an on-screen message.  Some messages are self-explanatory; others are described
in more detail below.

| CODE  | MESSAGE                              | DESCRIPTION                                                                                                                                 |
|-------|--------------------------------------|---------------------------------------------------------------------------------------------------------------------------------------------|
| `1`   | STACK UNDERFLOW                      | Monster uses stacks for various things (e.g. nested `.IF` directives); a context-dependent stack has underflowed                            |
| `2`   | STACK OVERFLOW                       | as above, but a context-dependent stack has overflowed                                                                                      |
| `3`   | OVERSIZED LINE                       | a line was found that is over the maximum length of 40 characters                                                                           |
| `4`   | INVALID EXPRESSION                   | the expression could not be successfully parsed                                                                                             |
| `5`   | INVALID MACRO ARGS                   | the macro was invoked with the wrong number of, or invalid, arguments                                                                       |
| `6`   | SYNTAX ERROR                         | general error; typically means an instruction is malformed                                                                                  |
| `7`   | INVALID DIRECTIVE                    | a `.`-prefixed string was encountered, signifying a directive, but it doesn't match any of the known directives                             |
| `8`   | LABEL UNDEFINED                      | an attempt was made to look up a label with a name that no label exists for                                                                 |
| `9`   | ENDIF WITH NO IF                     | an `.ENDIF` directive was encountered, but it has no matching `.IF` directive                                                               |
| `10`  | TOO MANY MACROS                      | the maximum number of macros (128) has been exceeded                                                                                        |
| `11`  | LABEL NOT LEFT ALIGNED               | something that looks like a label definition was found, but it is not in the leftmost column                                                |
| `12`  | INVALID OPCODE                       | something that looks like an opcode was found, but it does not match any known opcode                                                       |
| `13`  | INVALID ADDR MODE                    | the address mode used for an instruction is not legal for that opcode (e.g. `sty $1000,x`)                                                  |
| `14`  | OVERSIZED OPERAND                    | the operand for an instruction is too big (e.g. `lda #500`)                                                                                 |
| `15`  | INVALID LABEL                        | a label definition was found, but it contains invalid characters                                                                            |
| `16`  | LABEL TOO LONG                       | a label definition was found, but it exceeds the maximum length (16 characters)                                                             |
| `17`  | UNEXPECTED CHAR                      | a garbage character was encountered; the instruction or operand may be malformed                                                            |
| `18`  | EXPECTED VALUE                       | a value was expected but none was found (e.g. a missing operand)                                                                            |
| `19`  | I/O ERROR                             | general-purpose error from disk access                                                                                                      |
| `20`  | NO MACRO NAME                        | a macro definition was made, but it is missing a name (1st parameter)                                                                       |
| `21`  | LABEL UNRESOLVABLE                   | a label was referenced in the second assembly pass, but the assembler still hasn't seen a definition for it                                 |
| `22`  | CYCLIC INCLUDE                       | a file includes itself, directly or indirectly                                                                                              |
| `23`  | FAILED TO OPEN INCLUDE FILE          | an `.INC` directive was found, but the file it references could not be found/opened                                                         |
| `24`  | SEGMENT OVERLAP                      | two segments overlap in the final linked layout                                                                                             |
| `25`  | TOO MANY FILES                       | there are too many files                                                                                                                    |
| `26`  | PARAM NAME TOO LONG                  | a macro's parameter name is too long — parameter names must be 16 characters at most                                                        |
| `27`  | LINE NOT FOUND FOR ADDRESS           | no line was found for the address that was used to look one up                                                                              |
| `28`  | ORIGIN UNSET                         | an instruction was found before the program was given an origin (`.ORG`)                                                                    |
| `29`  | RANGE ERROR                          | branch target is too far; relative branch targets must be within [-128, 127] bytes of the branch                                            |
| `30`  | FILE NOT FOUND                       | a file was not found for the requested filename                                                                                             |
| `31`  | UNKNOWN SEGMENT                      | a segment was referenced that is not defined (linker)                                                                                       |
| `32`  | IMPORT UNDEFINED                     | an imported symbol has no matching export at link time                                                                                      |
| `33`  | PC TARGET UNWRITABLE                 | the PC has reached an address that is not writable (e.g. ROM), so the assembler cannot write to it                                          |
| `34`  | CANNOT REDUCE EXPRESSION             | an expression contains an invalid combination of relative and absolute references; define the symbol ahead of its usage so it is resolvable |
| `35`  | TOO MANY OPEN FILES                  | the maximum number of files that may be open (15) has been exceeded                                                                         |
| `36`  | LOGICAL FILE IN USE                  | the logical file number is already in use                                                                                                   |
| `37`  | NO DRIVE RESPONSE                    | the selected disk drive did not respond                                                                                                     |
| `38`  | FILE TOO BIG                         | the file requested to be loaded is too big to fit in a source buffer (24KB)                                                                 |
| `39`  | UNNAMED BUFFER                       | an operation (e.g. producing debug information for a line) requires the buffer to have a name, but it has none                              |
| `40`  | TOO MANY OPEN BUFFERS                | the maximum number of source buffers (8) is already open                                                                                    |
| `41`  | NO FILENAME                         | the command requires a filename (e.g. SAVE), but none was given                                                                            |
| `42`  | NO OPEN SCOPE                        | a local label was defined, but there is no open scope for it to be defined under                                                            |
| `43`  | LABEL ALREADY DEFINED                | a label with the name of another one already exists                                                                                         |
| `44`  | TOO MANY LABELS                      | the maximum number of named labels (640) has been exceeded                                                                                  |
| `45`  | LABEL NOT KNOWN IN PASS 1            | the address of a label was incorrectly inferred in pass 1; typically an operand assumed absolute turned out to be zero page                  |
| `46`  | INVALID COMMAND                      | an invalid monitor command was provided                                                                                                     |
| `47`  | COPY TOO BIG                         | a copy was attempted that exceeded the maximum size                                                                                         |
| `48`  | BUFFER NAME EXISTS                   | the buffer could not be given the suggested name because a buffer of the same name is already loaded                                        |
| `49`  | BUFFER FULL                          | an insertion into a buffer could not be completed because the buffer is already full                                                        |
| `50`  | TOO MANY GLOBAL REFS                 | the linker's global reference table has been exceeded                                                                                       |
| `51`  | TOO MANY SEGMENTS                    | the maximum number of segments (64) has been exceeded (linker)                                                                              |
| `52`  | SECTION OVERRUN                      | a section is too small to hold the segments assigned to it (linker)                                                                         |
| `53`  | CONFLICTING ADDRESS MODES FOR SYMBOL | a symbol was defined/used with conflicting address modes (zero page vs absolute) across object files                                         |
| `54`  | MULTIPLE DEFINITIONS FOR SYMBOL      | a symbol is exported (defined) in more than one object file                                                                                 |
| `55`  | UNKNOWN TYPE                         | an unknown segment TYPE was specified in the LINK file                                                                                      |
| `56`  | UNEXPECTED TYPE                      | a segment TYPE was used somewhere it is not allowed                                                                                         |
| `57`  | CONFLICTING SEGMENT TYPES            | a segment was declared with conflicting types across object files                                                                           |
| `58`  | BSS SEGMENT CONTAINS NONZERO DATA    | a BSS segment (which must be uninitialized) contains nonzero data                                                                           |
| `59`  | OUT OF MEMORY                        | a buffer (context-dependent) has run out of memory                                                                                          |
| `60`  | CONTEXT FULL                         | an internal context buffer is full                                                                                                          |
| `61`  | NO MATCHING SCOPE                    | a scope-closing construct has no matching open scope                                                                                        |
| `62`  | FILENAME TOO LONG                    | a filename exceeds the maximum length                                                                                                       |
| `63`  | TOO MANY WATCHES                     | the maximum number of watches (8) has been exceeded                                                                                         |
| `64`  | TOO MANY BREAKPOINTS                 | the maximum number of breakpoints (16) has been exceeded                                                                                    |
| `65`  | RORG REQUIRES ABSOLUTE ORIGIN        | `.RORG` was used without a preceding absolute `.ORG`                                                                                        |
| `66`  | UNCLOSED .IF                         | an `.IF` block was not closed with a matching `.ENDIF`                                                                                      |
| `67`  | UNCLOSED .MAC OR .REP                | a `.MAC` or `.REP` block was not closed with `.ENDMAC`/`.ENDREP`                                                                            |
| `68`  | CANNOT ASSEMBLE LOG                  | an attempt was made to assemble the LOG buffer                                                                                              |
| `69`  | DIVIDE BY ZERO                       | an expression attempted a division by zero                                                                                                  |
| `70`  | DUPLICATE BLOCK                      | a LINK file declares its `MEMORY` or `SEGMENTS` block more than once                                                                        |
| `71`  | UNKNOWN KEY                          | a LINK file uses an unrecognized property key in a memory section or segment definition                                                     |
| `72`  | NO SEGMENTS DEFINED                  | no segments were defined for the linker to place                                                                                            |
| `73`  | TOO MANY NESTED IFS                  | the maximum nesting depth of 8 `.IF`/`.IFDEF` blocks has been exceeded                                                                      |
| `74`  | TOO MANY IFDEFS                      | the maximum of 255 `.IFDEF` directives in one assembly has been exceeded                                                                    |
| `75`  | IFDEF PASS MISMATCH                  | pass 2 encountered an `.IFDEF` with no corresponding result recorded during pass 1                                                          |
| `76`  | MISSING REQUIRED KEY                 | a LINK file definition omits a required property (`START`/`END` for a memory section, `LOAD` for a segment)                                 |
| `77`  | TOO MANY OBJECTS                     | more object files were given to the linker than it can link at once (16)                                                                    |
| `78`  | DUPLICATE NAME                       | a LINK file declares two memory sections, or two segments, with the same name                                                               |
| `79`  | SEGMENT OUT OF RANGE                 | a segment's placement runs past the top of the address space (`$FFFF`)                                                                      |
| `80`  | NON INTEGRAL VALUE                   | a floating point value was used where only an integer is allowed                                                                            |
| `81`  | INVALID ALIGNMENT                    | a LINK file segment's `ALIGN` is invalid or outside the range 1–65535                                                                                           |
| `82`  | WATCH ALREADY EXISTS                 | the monitor already has a watch for the same address or exact range; remove it before adding another mode |
