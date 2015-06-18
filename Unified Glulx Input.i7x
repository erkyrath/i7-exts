Version 1 of Unified Glulx Input (for Glulx only) by Andrew Plotkin begins here.

Text-input-mode is a kind of value. The text-input-modes are no-input, char-input, line-input.

A glk-window is a kind of object.
A glk-window has a text-input-mode called the input-request.
A glk-window can be hyperlink-input.
Include (-
	with current_input_request (+ no-input +), ! of type text-input-modes
-) when defining a glk-window.

The story-window is a glk-window. The input-request of the story-window is line-input.
The status-window is a glk-window.

Include (-
Array inputevent --> 4;
-) after "Variables and Arrays" in "Glulx.i6t";

Include (-

[ AwaitInput a_buffer a_table a_event    done;
	! ### probably we put prompt-and-status inside the loop
	
	! ### prompt
	print ">"; !###
	! ### test input
	
	if ( (+ story-window +).current_input_request == (+ line-input +) ) {
		glk_cancel_line_event(gg_mainwin, gg_event);
		print "(DEBUG) cancel line input mode^";
	}
	else if ( (+ story-window +).current_input_request == (+ char-input +) ) {
		glk_cancel_char_event(gg_mainwin);
		print "(DEBUG) cancel char input mode^";
	}
	(+ story-window +).current_input_request = (+ no-input +);
	
	if (GProperty(OBJECT_TY, (+ story-window +), (+ input-request +) ) == (+ line-input +)) {
		!print "(DEBUG) req line input mode^";
		!### permit preload
		glk_request_line_event(gg_mainwin, a_buffer+WORDSIZE, INPUT_BUFFER_LEN-WORDSIZE, 0);
		(+ story-window +).current_input_request = (+ line-input +);
	}
	else if (GProperty(OBJECT_TY, (+ story-window +), (+ input-request +) ) == (+ char-input +)) {
		!print "(DEBUG) req char input mode^";
		glk_request_char_event(gg_mainwin);
		(+ story-window +).current_input_request = (+ line-input +);
	}

	!### we should call this before any blocking input. in minor cases (a rejected keystroke) we could skip it, but that's probably too much work.
	if (location ~= nothing && parent(player) ~= nothing) DrawStatusLine();
	
	done = false;
	while (~~done) {
		glk_select(gg_event);
		!### rulebook
		switch (gg_event-->0) {
			evtype_Arrange:
				DrawStatusLine();
			evtype_LineInput:
				if (gg_event-->1 == gg_mainwin) {
					(+ story-window +).current_input_request = (+ no-input +); ! complete
					a_buffer-->0 = gg_event-->2;
					VM_Tokenise(a_buffer, a_table);
					!### write to command stream if open
					done = true;
				}
		}
	}
	
	!### we should call this after every player input which is accepted
	quotewin_close_if_open();
];

[ quotewin_close_if_open;
	if (gg_quotewin) {
		glk_window_close(gg_quotewin, 0);
		gg_quotewin = 0;
	}
];

[ VM_ReadKeyboard a_buffer a_table;
	AwaitInput(a_buffer, a_table, 0);
];

-) instead of "Keyboard Input" in "Glulx.i6t".

Unified Glulx Input ends here.

---- DOCUMENTATION ----

Here's the summary:

All input will occur in an AwaitInput routine. This will be able to return any kind of event (not just a text buffer). It will display the prompt, draw/redraw the status line, accept input, filter it for the caller, and return it.

Displaying the prompt and filtering input events will become rulebooks, so that games can customize them. (The HandleGlkEvent hook will change to a rulebook.)

AwaitInput will be responsible for requesting and cancelling Glk input. (Again, customizable via rulebook, so games can add custom input types.)

The parser will invoke a rulebook/activity to convert new event types directly into actions.

The first draft will be concerned only with the story window. I will extend it to status window input later. Eventually it will have to be compatible with the Multiple Windows extension, but I don't know whether that means adapting this extension to that one or vice versa. (Probably both.)


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
- write event to transcript stream
- VM_Tokenise (if line input)
- close quote window if open
- return event info

Now, when Parser__parse calls Keyboard, it may discover a non-textbuffer result. It should fill out parser_results (via rulebook) and immediately return. Skip over all again/buffering and similar textual hackery. If the rulebook doesn't decide on a result, we have a "parser error"; complain and jump to ReType.

The secondary Keyboard calls (in NounDomain) cause messiness. They should certainly cause NounDomain to return REPARSE_CODE (the "treat as new command" case). Some NounDomain calls are in Parser__parse and loop straight back to Reparse, which is fine; that leads to the rulebook-and-return code path. Others are in ParseToken__. Those worry me.

YesOrNo will set up line input request and call AwaitInput. It should reject all non-textbuffer results. Stay in the loop until "yes" or "no" is typed. (Special case: run the rulebook, accept saying-yes/saying-no actions as an answer? Rulebook will have to be context-sensitive.) (If you call YesOrNo when line input is already in progress, it gets interrupted.)

READ_FINAL_ANSWER_R is similar to YesOrNo, except it only accepts textbuffer input. (But this textbuffer may result from the glk_select rulebook.)

Questions:
- Handle/save undo on non-textbuffer input? The rule should be that we only save undo if the player *could* request undo. (Otherwise they'll be trapped in a move.)
- Always run in no-echo mode? Should probably make this a global variable; certain tricks are only possible in no-echo mode.
- HandleGlkEvent? Entirely replaced by the AwaitInput rulebook.
- ParseToken__: Still worrying.

