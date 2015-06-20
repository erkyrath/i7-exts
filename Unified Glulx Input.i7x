Version 1 of Unified Glulx Input (for Glulx only) by Andrew Plotkin begins here.

Input-context is a kind of value. The input-contexts are primary context, disambig context, yes-no question context, final question context.

The prompt displaying rules are an input-context based rulebook.

Rule for prompt displaying the yes-no question context (this is the yes-no question prompt rule):
	instead say ">" (A).
Rule for prompt displaying the final question context (this is the final question prompt rule):
	instead say ">" (A).
Rule for prompt displaying an input-context (this is the default prompt rule):
	instead say the command prompt.
The final question prompt rule is listed last in the prompt displaying rules.
The yes-no question prompt rule is listed last in the prompt displaying rules.
The default prompt rule is listed last in the prompt displaying rules. [really truly last]

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

! AwaitInput: block and await an acceptable input. What "acceptable" means is customizable. Typically the caller will be interested in some event types (e.g., line input), will allow others to do their job (arrange events redrawing the status window), and will ignore the rest (keep awaiting input).
! This is the low-level entry point to the Glk input system; all input requests funnel down to this function. It sets up the Glk input request events and calls glk_select().
! This function also handles displaying the prompt and redrawing the status line. (Through customizable rulebooks and activities, of course.)
! AwaitInput takes three arguments: a line input buffer, a buffer for parsing words from line input, and an event structure. (If the caller is not interested in line input, the first two arguments are ignored.)

[ AwaitInput incontext a_event a_buffer a_table    done;
	! ### probably we put prompt-and-status inside the loop
	
	! ### prompt
	FollowRulebook((+ prompt displaying rules +), incontext, true);
	
	! ### test or command-stream input
	
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

-) instead of "Keyboard Input" in "Glulx.i6t".

Include (-

[ Keyboard  a_buffer a_table a_event    nw i w w2 x1 x2;
	sline1 = score; sline2 = turns; !### should be part of status-line-updating

	! Repeat loop until an acceptable input arrives.
	while (true) {
		! Save the start of the buffer, in case "oops" needs to restore it
		!### but not if input is still in progress?
		for (i=0 : i<64 : i++) oops_workspace->i = a_buffer->i;
		
		!### set keyboard-input? Customizably!
		WriteGProperty(OBJECT_TY, (+ story-window +), (+ input-request +), (+ line-input +) );
		AwaitInput( (+ primary context +), a_event, a_buffer, a_table);
		
		! Set nw to the number of words
		nw = a_table-->0;
		
		! If the line was blank, get a fresh line
		if (nw == 0) {
			@push etype; etype = BLANKLINE_PE;
			players_command = 100;
			BeginActivity(PRINTING_A_PARSER_ERROR_ACT);
			if (ForActivity(PRINTING_A_PARSER_ERROR_ACT) == false) {
				PARSER_ERROR_INTERNAL_RM('X', noun); new_line;
			}
			EndActivity(PRINTING_A_PARSER_ERROR_ACT);
			@pull etype;
			continue;
		}
		
		! Unless the opening word was OOPS, return
		! Conveniently, a_table-->1 is the first word on both the Z-machine and Glulx
		
		w = a_table-->1;
		if (w == OOPS1__WD or OOPS2__WD or OOPS3__WD) {
			if (oops_from == 0) { PARSER_COMMAND_INTERNAL_RM('A'); new_line; continue; }
			if (nw == 1) { PARSER_COMMAND_INTERNAL_RM('B'); new_line; continue; }
			if (nw > 2) { PARSER_COMMAND_INTERNAL_RM('C'); new_line; continue; }
		
			! So now we know: there was a previous mistake, and the player has
			! attempted to correct a single word of it.
		
			for (i=0 : i<INPUT_BUFFER_LEN : i++) buffer2->i = a_buffer->i;
			x1 = a_table-->6; ! Start of word following "oops"
			x2 = a_table-->5; ! Length of word following "oops"
		
			! Repair the buffer to the text that was in it before the "oops"
			! was typed:
			for (i=0 : i<64 : i++) a_buffer->i = oops_workspace->i;
			VM_Tokenise(a_buffer,a_table);
		
			! Work out the position in the buffer of the word to be corrected:
			w = a_table-->(3*oops_from);      ! Start of word to go
			w2 = a_table-->(3*oops_from - 1); ! Length of word to go
		
			! Write spaces over the word to be corrected:
			for (i=0 : i<w2 : i++) a_buffer->(i+w) = ' ';
		
			if (w2 < x2) {
				! If the replacement is longer than the original, move up...
				for ( i=INPUT_BUFFER_LEN-1 : i>=w+x2 : i-- )
					a_buffer->i = a_buffer->(i-x2+w2);
		
				! ...increasing buffer size accordingly.
				a_buffer-->0 = (a_buffer-->0) + (x2-w2);
			}
		
			! Write the correction in:
			for (i=0 : i<x2 : i++) a_buffer->(i+w) = buffer2->(i+x1);
		
			VM_Tokenise(a_buffer, a_table);
			nw = a_table-->0;
		
			return nw;
		}

		! Undo handling
	
		if ((w == UNDO1__WD or UNDO2__WD or UNDO3__WD) && (nw==1)) {
			Perform_Undo();
			continue;
		}
		i = VM_Save_Undo();
		#ifdef PREVENT_UNDO; undo_flag = 0; #endif;
		#ifndef PREVENT_UNDO; undo_flag = 2; #endif;
		if (i == -1) undo_flag = 0;
		if (i == 0) undo_flag = 1;
		if (i == 2) {
			VM_RestoreWindowColours();
			VM_Style(SUBHEADER_VMSTY);
			SL_Location(); print "^";
			! print (name) location, "^";
			VM_Style(NORMAL_VMSTY);
			IMMEDIATELY_UNDO_RM('E'); new_line;
			continue;
		}
		return nw;
	}
];

-) instead of "Reading the Command" in "Parser.i6t".

Include (-

[ YesOrNo i j;
    for (::) {
        AwaitInput( (+ yes-no question context +), inputevent, buffer, parse);
        j = parse-->0;
        if (j) { ! at least one word entered
            i = parse-->1;
            if (i == YES1__WD or YES2__WD or YES3__WD) rtrue;
            if (i == NO1__WD or NO2__WD or NO3__WD) rfalse;
        }
        ! bad response; try again
        YES_OR_NO_QUESTION_INTERNAL_RM('A'); !###
    }
];

[ YES_OR_NO_QUESTION_INTERNAL_R; ];

-) instead of "Yes/No Questions" in "Parser.i6t".

The print the final prompt rule is not listed in any rulebook.

Include (-

[ READ_FINAL_ANSWER_R;
    AwaitInput( (+ final question context +), inputevent, buffer, parse);
	players_command = 100 + WordCount();
	num_words = WordCount();
	wn = 1;
	rfalse;
];

-) instead of "Read The Final Answer Rule" in "OrderOfPlay.i6t".

Include (-

!### temp shim, called from Tests.i6t
[ VM_ReadKeyboard a_buffer a_parse;
];

-) instead of "Keyboard Primitive" in "Parser.i6t".


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

Keyboard: same call context. (Except the return value changes. It now returns an input event representation, of which the buffer/parse is just one part.) (We should add a third array argument for nontextual input.) (And rename it to ParserInput!)
- loop until nonblank: (make this optional?)
- save oops_workspace (if line input is not in progress!)
- set up line input request (ditto!)
- AwaitInput
- do oops substitution (ditto!)
- handle undo, save undo (ditto?)
- return result of AwaitInput

VM_KeyDelay: now a high-level call, parallel to Parser__parse use of Keyboard. (Rename...)
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

Test cases:
- Change the prompt for YesOrNo, or for the main game
- Keystroke-only game -- char input rather than line input
- Keystroke-only game that calls YesOrNo for a single line input
- A game with a timer interrupt
- A timer-only game
- Hyperlinks only
- Hyperlinks or text input
- Hyperlinks that create synthetic text input
