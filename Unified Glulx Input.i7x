Version 1 of Unified Glulx Input by Andrew Plotkin begins here.

Unified Glulx Input ends here.

---- DOCUMENTATION ----

* Old plan

Keyboard: high-level (called only from parser, three spots)
- loop until nonblank:
- save oops_workspace
- prompt
- status line
- KeyboardPrimitive
- do oops substitution
- handle undo, save undo

KeyboardPrimitive: thin wrapper around VM_ReadKeyboard (called from Keyboard, YesOrNo, READ_FINAL_ANSWER_R) (always with a status line)
- divert to TestKeyboardPrimitive if there's a "test me"
- VM_ReadKeyboard

VM_ReadKeyboard: low-level (called only from KeyboardPrimitive)
- grab command-stream entry if available
- glk_request_line_event
- loop until line input:
-   glk_select
-   HandleGlkEvent on events (can break or continue loop)
-   also status line on arrange events
- write to transcript stream
- VM_Tokenise
- close quote window if open

VM_KeyDelay: low-level (but called from extensions)
- glk_request_char_event, glk_request_timer_events
- loop until line input:
-   glk_select
-   HandleGlkEvent on events (can break or continue loop)
-   fails to status line on arrange events!
- cancel both event types

* New plan

Keyboard: same call context. (Except the return value changes. It now returns an input event representation, of which the buffer/parse is just one part.) (We should add a third array argument for nontextual input.)
- loop until nonblank: (make this optional?)
- save oops_workspace (if line input is not in progress!)
- set up line input request (ditto!)
- AwaitInput
- do oops substitution (ditto!)
- handle undo, save undo (ditto?)
- return result of AwaitInput

VM_KeyDelay: now a high-level call, parallel to Parser__parse use of Keyboard.
- set up char input request 
- AwaitInput
- (AwaitInput's rulebook should handle status line, stop on key or timer.)
- return key or timer info

AwaitInput: the low-level routine. Callers set up request variables. Leaves input requested only actually if in progress, so we only need "real" cancels, not bureaucratic cancels. (I.e.: we minimize request/cancel calls.)
- display prompt (if beginning line input) (must be context-sensitive!)
- check command-stream entry if available
- check test status if available (TestKeyboardPrimitive should return flag!)
- status line
- loop:
-   glk_select
-   dispatch events via rulebook.
-   the rulebook can continue the loop; stop the loop; fill out line buffer and stop the loop. (That last may involving cancelling input-in-progress, or not.)
-   (by default, arrange event refreshes the status line and continues loop)
- return event info

Now, when Parser__parse calls Keyboard, it may discover a non-textbuffer result. It should fill out parser_results (via rulebook) and immediately return. Skip over all again/buffering and similar textual hackery. If the rulebook doesn't decide on a result, we have a "parser error"; complain and jump to ReType.

The secondary Keyboard calls (in NounDomain) cause messiness. They should certainly cause NounDomain to return REPARSE_CODE (the "treat as new command" case). Some NounDomain calls are in Parser__parse and loop straight back to Reparse, which is fine; that leads to the rulebook-and-return code path. Others are in ParseToken__. Those worry me.

YesOrNo will set up line input request and call AwaitInput. It should reject all non-textbuffer results. Stay in the loop until "yes" or "no" is typed. (Special case: run the rulebook, accept saying-yes/saying-no actions as an answer? Rulebook will have to be context-sensitive.) (If you call YesOrNo when line input is already in progress, it gets interrupted.)

READ_FINAL_ANSWER_R is similar to YesOrNo, except it only accepts textbuffer input. (But this textbuffer may result from the glk_select rulebook.)

Questions:
- Handle/save undo on non-textbuffer input? The rule should be that we only save undo if the player *could* request undo. (Otherwise they'll be trapped in a move.)
- Always run in no-echo mode? Should probably make this a global variable; certain tricks are only possible in no-echo mode.
- HandleGlkEvent? Entirely replaced by the AwaitInput rulebook.


