Version 1 of Unified Glulx Input (for Glulx only) by Andrew Plotkin begins here.

[### This extension is a work in progress. You can do some stuff with it now, though!]

Chapter - Constants and Variables

Section - Basic Types

Use pass blank input lines translates as (- Constant PASS_BLANK_INPUT_LINES; -). 

[Input-contexts allow us to organize the rulebooks described in this extension. Every time the game stops to await input, it does so in an input context.]

An input-context is a kind of value. The input-contexts are primary context, disambiguation context, yes-no question context, extended yes-no question context, repeat yes-no question context, final question context, keystroke-wait context.

Definition: an input-context is command if it is primary context or it is disambiguation context.
Definition: an input-context is yes-no if it is yes-no question context or it is extended yes-no question context or it is repeat yes-no question context.

[We need a proxy object for each Glk window. Currently we just define two. (This extension is not yet compatible with Multiple Windows.)]

A glk-window is a kind of object.
The story-window is a glk-window.
The status-window is a glk-window.

[When setting up input (in a particular context), the game indicates which input requests each window should make. These are the input-request and hyperlink-input-request properties.]

A text-input-mode is a kind of value. The text-input-modes are no-input, char-input, line-input.
A glk-window has a text-input-mode called the input-request.
A glk-window has a text called the preload-input-text.
A glk-window can be hyperlink-input-request.

[Timer input is global -- not attached to any window.]

The timer-request is a number that varies. The timer-request is 0.

[These I6 properties are used internally by the library to track whether specific Glk requests have been made or fulfilled. The game should never touch these.]

Include (-
	with current_input_request (+ no-input +), ! of type text-input-mode
	with current_hyperlink_request false,      ! true or false
-) when defining a glk-window.

Include (-
! Milliseconds, or 0 for no timer events.
Global current_timer_request = 0;
-) after "Variables and Arrays" in "Glulx.i6t";

[These values match up to evtype_Timer, evtype_CharInput, etc because they are defined in the same order.]
A g-event is a kind of value. The g-events are timer-event, char-event, line-event, mouse-event, arrange-event, redraw-event, sound-notify-event, and hyperlink-event.

To decide which g-event is null-event: (- 0 -)


Section - A Few Globals

[These event structures are now used in parallel with the buffer and parse arrays. For example, we'll call ParserInput(context, inputevent, buffer, parse) or ParserInput(context, inputevent2, buffer2, parse2).]

Include (-
Array inputevent --> 4;
Array inputevent2 --> 4;
-) after "Variables and Arrays" in "Glulx.i6t";

[Indicates whether the parser_results has been filled in by ParserInput (and thus we need no text parsing).]

Include (-
Global parser_results_set;
-) after "Global Variables" in "Output.i6t";


Section - Input Rulebook Data

[Globals used within the accepting input and handling input rulebooks. (Logically, these could be rulebook variables, but that turns out to be awkward. In several ways.)]

Include (-
! Array contains: a_event, a_buffer, a_table
Constant INPUT_RULEBOOK_SIZE 4;
Constant IRDAT_RB_CURRENT 0;
Constant IRDAT_EVENT 1;
Constant IRDAT_BUFFER 2;
Constant IRDAT_TABLE 3;
Array input_rulebook_data --> INPUT_RULEBOOK_SIZE;
-) after "Variables and Arrays" in "Glulx.i6t";

Include (-
[ InputRDataInit rb a_event a_buffer a_table;
	input_rulebook_data-->IRDAT_RB_CURRENT = rb;
	input_rulebook_data-->IRDAT_EVENT = a_event;
	input_rulebook_data-->IRDAT_BUFFER = a_buffer;
	input_rulebook_data-->IRDAT_TABLE = a_table;
];

[ InputRDataFinal;
	input_rulebook_data-->IRDAT_RB_CURRENT = 0;
	input_rulebook_data-->IRDAT_EVENT = 0;
	input_rulebook_data-->IRDAT_BUFFER = 0;
	input_rulebook_data-->IRDAT_TABLE = 0;
];
-).

[Some phrases for use in the accepting input and handling input rulebooks. Do not try to use them anywhere else!]

To decide what g-event is the/-- current input event type: (- InputRDataEvType() -).
To decide whether handling (E - g-event): (- InputRDataEvTypeIs({E}) -).
To decide what Unicode character is the/-- current input event char/character: (- InputRDataEvChar() -).
To decide what number is the/-- current input event link/hyperlink number: (- InputRDataEvHyperlink() -).
To decide what object is the/-- current input event link/hyperlink object: (- ObjectValueIsSafe(InputRDataEvHyperlink()) -).
To decide what number is the/-- current input event line word count: (- InputRDataEvLineWordCount() -).

To replace the/-- current input event with the/-- line (T - text): (- InputRDataSetEvent(evtype_LineInput, {T}); -).
To replace the/-- current input event with the/-- char/character (C - Unicode character): (- InputRDataSetEvent(evtype_CharInput, {C}); -).

To handle the/-- current input event as (act - stored action): (- InputRDataParseAction({-by-reference:act}); -);

Include (-

[ InputRDataEvType;
	if (~~input_rulebook_data-->IRDAT_EVENT)
		return evtype_None;
	return (input_rulebook_data-->IRDAT_EVENT)-->0;
];

[ InputRDataEvTypeIs typ;
	if (~~input_rulebook_data-->IRDAT_EVENT)
		return false;
	if ((input_rulebook_data-->IRDAT_EVENT)-->0 == typ)
		return true;
	return false;
];

[ InputRDataEvChar;
	if (~~input_rulebook_data-->IRDAT_EVENT)
		return 0;
	if ((input_rulebook_data-->IRDAT_EVENT)-->0 ~= evtype_CharInput)
		return 0;
	return (input_rulebook_data-->IRDAT_EVENT)-->2;
];

[ InputRDataEvHyperlink;
	if (~~input_rulebook_data-->IRDAT_EVENT)
		return 0;
	if ((input_rulebook_data-->IRDAT_EVENT)-->0 ~= evtype_Hyperlink)
		return 0;
	return (input_rulebook_data-->IRDAT_EVENT)-->2;
];

[ InputRDataEvLineWordCount;
	if (~~input_rulebook_data-->IRDAT_EVENT)
		return 0;
	if ((input_rulebook_data-->IRDAT_EVENT)-->0 ~= evtype_LineInput)
		return 0;
	if (~~input_rulebook_data-->IRDAT_TABLE)
		return 0;
	return (input_rulebook_data-->IRDAT_TABLE)-->0;
];

[ InputRDataSetEvent typ arg    ev len;
	if (~~input_rulebook_data-->IRDAT_EVENT)
		return;
	ev = input_rulebook_data-->IRDAT_EVENT;
	ev-->0 = typ;
	switch (typ) {
		evtype_CharInput:
			ev-->1 = gg_mainwin;
			ev-->2 = arg;
		evtype_LineInput:
			if (~~input_rulebook_data-->IRDAT_BUFFER)
				return; ! No text buffer!
			ev-->1 = gg_mainwin;
			len = VM_PrintToBuffer(input_rulebook_data-->IRDAT_BUFFER, INPUT_BUFFER_LEN-WORDSIZE, TEXT_TY_Say, arg);
			ev-->2 = len;
			if (input_rulebook_data-->IRDAT_TABLE) {
				VM_Tokenise(input_rulebook_data-->IRDAT_BUFFER, input_rulebook_data-->IRDAT_TABLE);
			}
	}
];

! InputRDataParseAction: Tell AwaitInput to interrupt char/line input, so that text can be printed. If there is no char/line input going on, this does nothing. (So it's safe to call it more than once.)
! This is just a temporary interruption. If AwaitInput continues, it will re-request input according to the setting-up-input rulebook.
! This may only be called from the accepting-input rulebook. 
[ InputRDataInterruptInput winproxy   win;
	if (~~input_rulebook_data-->IRDAT_EVENT)
		return;
		
	if (winproxy == (+ story-window +) )
		win = gg_mainwin;
	else if (winproxy == (+ status-window +) )
		win = gg_statuswin;
	else
		return;
	
	if (winproxy.current_input_request == (+ line-input +) ) {
		glk_cancel_line_event(win, gg_event);
		winproxy.current_input_request = (+ no-input +);
	}
	else if (winproxy.current_input_request == (+ char-input +) ) {
		glk_cancel_char_event(win);
		winproxy.current_input_request = (+ no-input +);
	}
];

! InputRDataParseAction: Parse out a stored action into parser_results form. We also set parsed_number, actor, and the parser_results_set flag. I don't think we have to set up anything else. (Note that this will be immediately followed by calls to TreatParserResults and GENERATE_ACTION_R.)
! I had to stare at a lot of parser code to work out this transformation. Action data doesn't normally flow this way (from stored action into parser_results). I hope I covered all the necessary cases.
! In the interests of sanity, we don't try to handle actions which include text (topics). These are really only useful when parsing player-typed commands, and the whole point of this routine is to bypass textual input.
! We also don't handle multiple-object actions. Stored actions can't store those.
[ InputRDataParseAction stora   acname at acmask req count val;
	acname = BlkValueRead(stora, STORA_ACTION_F);
	at = FindAction(acname);
	if (~~at)
		print_ret "InputRDataParseAction: cannot find action ", (SayActionName) acname, ".";
		
	req = BlkValueRead(stora, STORA_REQUEST_F);
	if (req == 1) {
		val = BlkValueRead(stora, STORA_ACTOR_F);
		print_ret "InputRDataParseAction: cannot set up an ~asking ", (name) val, " to try ", (SayActionName) acname, "~ action. Did you mean ~", (name) val, " ", (SayActionName) acname, "~?";
	}
	if (req)
		print_ret "InputRDataParseAction: cannot set up an action (", (SayActionName) acname, ") which involves a topic.";
	
	parser_results_set = true;
	actor = BlkValueRead(stora, STORA_ACTOR_F);
	parser_results-->ACTION_PRES = acname;
	count = 0;
	parser_results-->INP1_PRES = 0;
	parser_results-->INP2_PRES = 0;
	
	acmask = ActionData-->(at+AD_REQUIREMENTS);
	val = BlkValueRead(stora, STORA_NOUN_F);
	if (acmask & NEED_NOUN_ABIT) {
		if (ActionData-->(at+AD_NOUN_KOV) == OBJECT_TY) {
			parser_results-->(INP1_PRES+count) = val;
		}
		else {
			parser_results-->(INP1_PRES+count) = 1;
			parsed_number = val;
		}
		count++;
	}

	val = BlkValueRead(stora, STORA_SECOND_F);
	if (acmask & NEED_SECOND_ABIT) {
		if (ActionData-->(at+AD_SECOND_KOV) == OBJECT_TY) {
			parser_results-->(INP1_PRES+count) = val;
		}
		else {
			parser_results-->(INP1_PRES+count) = 1;
			parsed_number = val;
		}
		count++;
	}

	parser_results-->NO_INPS_PRES = count;
];

! This returns val unchanged if it's really an object value. If it's anything else (zero, a string, a function, an invalid memory address) the return value will be zero (nothing).
! (This could be made safer with the I6 6.34 compiler, which allows more introspection into how many objects and classes exist in the game.)
! (Not currently compatible with Dynamic Objects.)
[ ObjectValueIsSafe val   limit;
	if (val < Class)
		return 0;
	if (val >= #grammar_table)
		return 0;
	@getmemsize limit;
	if (val > limit - (1 + NUM_ATTR_BYTES + 6*4))
		return 0;
	if (val->0 ~= $70)
		return 0;
	return val;
];

-).


Section - Glk Special Keycodes

[These are not true Unicode characters, but character input events can contain them, so we need definitions. Do not try to print these values using the usual I7 say phrase! The phrase "say extended C" will print both normal and special Unicode values.]

To decide which Unicode character is special keycode left: (- keycode_Left -).
To decide which Unicode character is special keycode right: (- keycode_Right -).
To decide which Unicode character is special keycode up: (- keycode_Up -).
To decide which Unicode character is special keycode down: (- keycode_Down -).
To decide which Unicode character is special keycode return: (- keycode_Return -).
To decide which Unicode character is special keycode delete: (- keycode_Delete -).
To decide which Unicode character is special keycode escape: (- keycode_Escape -).
To decide which Unicode character is special keycode tab: (- keycode_Tab -).
To decide which Unicode character is special keycode pageup: (- keycode_PageUp -).
To decide which Unicode character is special keycode pagedown: (- keycode_PageDown -).
To decide which Unicode character is special keycode home: (- keycode_Home -).
To decide which Unicode character is special keycode end: (- keycode_End -).
To decide which Unicode character is special keycode func1: (- keycode_Func1 -).
To decide which Unicode character is special keycode func2: (- keycode_Func2 -).
To decide which Unicode character is special keycode func3: (- keycode_Func3 -).
To decide which Unicode character is special keycode func4: (- keycode_Func4 -).
To decide which Unicode character is special keycode func5: (- keycode_Func5 -).
To decide which Unicode character is special keycode func6: (- keycode_Func6 -).
To decide which Unicode character is special keycode func7: (- keycode_Func7 -).
To decide which Unicode character is special keycode func8: (- keycode_Func8 -).
To decide which Unicode character is special keycode func9: (- keycode_Func9 -).
To decide which Unicode character is special keycode func10: (- keycode_Func10 -).
To decide which Unicode character is special keycode func11: (- keycode_Func11 -).
To decide which Unicode character is special keycode func12: (- keycode_Func12 -).

Definition: a Unicode character is a special keycode if I6 routine "UnicodeCharIsSpecial" says so (it is a control key like tab, escape, or the arrow keys).
To say extended (C - Unicode character): (- PrintUnicodeSpecialName({C}); -).

Include (-
[ UnicodeCharIsSpecial ch;
	if (ch < 0 && ch >= -keycode_MAXVAL)
		rtrue;
	rfalse;
];

[ PrintUnicodeSpecialName ch;
	if (ch >= 0 && ch < $110000) {
		@streamunichar ch;
		return;
	}
	switch (ch) {
		keycode_Left: print "left";
		keycode_Right: print "right";
		keycode_Up: print "up";
		keycode_Down: print "down";
		keycode_Return: print "return";
		keycode_Delete: print "delete";
		keycode_Escape: print "escape";
		keycode_Tab: print "tab";
		keycode_PageUp: print "pageup";
		keycode_PageDown: print "pagedown";
		keycode_Home: print "home";
		keycode_End: print "end";
		keycode_Func1: print "func1";
		keycode_Func2: print "func2";
		keycode_Func3: print "func3";
		keycode_Func4: print "func4";
		keycode_Func5: print "func5";
		keycode_Func6: print "func6";
		keycode_Func7: print "func7";
		keycode_Func8: print "func8";
		keycode_Func9: print "func9";
		keycode_Func10: print "func10";
		keycode_Func11: print "func11";
		keycode_Func12: print "func12";
		default: print "unknown";
	}
];

-).


Chapter - New Rulebooks

Section - Setting Up Input

[This rulebook sets up the input requests for parser input (the command contexts, not the yes-or-no or final questions). It is called at the top of the ParserInput loop.]
The setting up input rules are an input-context based rulebook.

To clear all input requests for (W - glk-window) (this is all-input-request-clearing):
	now the input-request of W is no-input;
	now the preload-input-text of W is "";
	now W is not hyperlink-input-request.

First setting up input rule (this is the initial clear input requests rule):
	clear all input requests for the story-window.

Setting up input rule (this is the standard parser input line request rule):
	now the input-request of the story-window is line-input.


Section - Prompt Displaying

[This rulebook, as you might guess, is in charge of displaying the prompt before player input. Its parameter is an input-context, which distinguishes regular game input from other questions (yes-or-no, the final game question, etc).]
The prompt displaying rules are an input-context based rulebook.

[These globals are used by the extended yes-or-no system (YesOrNoPrompt). The classic YesOrNo does not use them.]
The extended yes-no prompt is a text that varies.
The repeat yes-no prompt is a text that varies.

[The default prompt rules are a bit complicated because we want to keep supporting the old prompt customization tools, which date from across Inform's history. For example, the default prompt rule displays the old "command prompt" global variable, which defaults to ">".]

Last prompt displaying rule for the keystroke-wait context (this is the keystroke-wait prompt rule):
	instead say "" (A).
Last prompt displaying rule for the yes-no question context (this is the yes-no question prompt rule):
	instead say ">" (A).
Last prompt displaying rule for the extended yes-no question context (this is the extended yes-no question prompt rule):
	instead say "[extended yes-no prompt] >" (A).
Last prompt displaying rule for the repeat yes-no question context (this is the repeat yes-no question prompt rule):
	instead say "[repeat yes-no prompt] >" (A).
Last prompt displaying rule for the final question context (this is the final question prompt rule):
	instead follow the print the final prompt rule.
Last prompt displaying rule for an input-context (this is the default prompt rule):
	[really truly last]
	instead say the command prompt.


Section - Accepting Input

The accepting input rules are an input-context based rulebook.

[Called in AwaitEvent. On success (acceptance), AwaitEvent returns the event. On no result or failure (rejection), AwaitEvent continues waiting.]

[Handy synonyms for "rule succeeds" and "rule fails".]
To accept the/-- input event: (- RulebookSucceeds(); rtrue; -).
To reject the/-- input event: (- RulebookFails(); rtrue; -).

[Same definition as in Basic Screen Effects.]
To update/redraw the/-- status line: (- DrawStatusLine(); -).

To interrupt text input for (W - glk-window): (- InputRDataInterruptInput({W}); -).

[Standard rules:]

Accepting input rule when handling arrange-event (this is the standard redraw status line on arrange rule):
	redraw the status line;
	reject input event.

Last accepting input rule (this is the standard accept all requested input rule):
	let T be the current input event type;
	if T is:
		-- line-event:
			if the input-request of the story-window is line-input:
				accept input event;
		-- char-event:
			if the input-request of the story-window is char-input:
				accept input event;
		-- hyperlink-event:
			if the story-window is hyperlink-input-request:
				accept input event;
	[This doesn't cover non-human-generated events like timer events.]
	reject input event.


Section - Handling Input

The handling input rules are an input-context based rulebook.

[Called after (not within) any invocation of ParserInput. If a specific action is generated, that action is performed. On failure: no action is parsed or performed.]

[This rulebook is empty by default. On no result, the parser proceeds with its normal behavior: line input is parsed, other input is rejected with "I beg your pardon?"]


Chapter - Our Core Routines

Section - AwaitInput

Include (-

! AwaitInput: block and await an acceptable input. What "acceptable" means is customizable. Typically the caller will be interested in some event types (e.g., line input), will allow others to do their job (arrange events redrawing the status window), and will ignore the rest (keep awaiting input).
! This is the low-level entry point to the Glk input system; all input requests funnel down to this function. It sets up the Glk input request events and calls glk_select().
! This function also handles displaying the prompt and redrawing the status line. (Through customizable rulebooks and activities, of course.)
! AwaitInput takes four arguments: the input context, an event structure, a line input buffer, and a buffer for parsing words from line input. (The latter two arguments are optional; if not supplied then line input cannot be accepted. If a_buffer is supplied but a_table is not, then line input will be accepted but not tokenized.)

[ AwaitInput incontext a_event a_buffer a_table     runonprompt wanttextinput wantlinkinput res val len;
	! Clear our argument arrays (if present).
	a_event-->0 = evtype_None;
	if (a_buffer) {
		a_buffer-->0 = 0;
	}
	if (a_table) {
		a_table-->0 = 0;
	}
	
	! In the old-fashioned YesOrNo sequence, we want to print the prompt after the caller's printed question, with no line break. In all other cases, we ensure a line break before the prompt.
	runonprompt = (incontext == (+ yes-no question context +) );
	
	! When this function begins, the window is not awaiting any input (except perhaps timer input).
	
	if (input_rulebook_data-->IRDAT_RB_CURRENT ~= 0) {
		print "(BUG) AwaitInput called recursively!^";
	}
	InputRDataInit( (+ accepting input rules +), a_event, a_buffer, a_table);
	
	while (true) {
	
		! Before input, we do the work that was in the old PrintPrompt call. The conditions have evolved a bit, though.

		! If the story window isn't tied up awaiting input, print any pending run-time problems. (We don't care if this screws up the prompt. RTPs are allowed to be ugly.)
		if ( (+ story-window +).current_input_request == (+ no-input +) ) {
			RunTimeProblemShow();
			ClearRTP();
		}
	
		! If we're not already awaiting input, print the prompt. We also take care of other beginning-of-turn business, like showing quote-boxes and cleaning up leftover italics and what not.
		! (Note that this stanza will always run on the first trip through the AwaitInput loop, because we entered awaiting no input. After that, we'll re-run the stanza (re-print the prompt) every time an input event cancels or completes text input.)
		!### what about input events when there's no text input at all? E.g. a hyperlink-only game. We should only re-print the prompt if the event printed output, but this may be hard to check. Might need a phrase to signal "I printed stuff".
		if ( (+ story-window +).current_input_request == (+ no-input +) ) {
			style roman;
			if (~~runonprompt) {
				EnsureBreakBeforePrompt();
			}
			runonprompt = false;
			
			FollowRulebook((+ prompt displaying rules +), incontext, true);
			
			ClearBoxedText();  ! this *displays* a pending quotation
			ClearParagraphing(14);
		}
	
		! Redraw the status line.
		! (This currently assumes that the status line never accepts text input.)
		sline1 = score; sline2 = turns;
		if (location ~= nothing && parent(player) ~= nothing) DrawStatusLine();
	
		! If test input is pending, grab it rather than requesting new input.
		#Ifdef DEBUG; #Iftrue ({-value:NUMBER_CREATED(test_scenario)} > 0);
		res = CheckTestInput(a_event, a_buffer);
		if (res && a_event-->0) {
			jump GotEvent;
		}
		#Endif; #Endif;
		
		! If replay-stream input is pending, grab that.
		if (gg_commandstr && gg_command_reading) {
			res = RecordingReadEvent(a_event, a_buffer);
			if (res && a_event-->0) {
				jump GotEvent;
			}
		}
		
		! Adjust the Glk input requests to match what the game wants. This may involve setting or cancelling requests.
		wantlinkinput = GetEitherOrProperty( (+ story-window +), (+ hyperlink-input-request +) );
		wanttextinput = GProperty(OBJECT_TY, (+ story-window +), (+ input-request +) );
		if (wanttextinput == (+ line-input +) && a_buffer == 0) {
			print "(BUG) AwaitInput: called with a line input request but no buffer argument";
			wanttextinput = (+ no-input +);
		}
	
		if ( (+ story-window +).current_input_request == (+ line-input +) && wanttextinput ~= (+ line-input +) ) {
			glk_cancel_line_event(gg_mainwin, gg_event);
			(+ story-window +).current_input_request = (+ no-input +);
			!print "(DEBUG) cancel line input mode^";
		}
		if ( (+ story-window +).current_input_request == (+ char-input +) && wanttextinput ~= (+ char-input +) ) {
			glk_cancel_char_event(gg_mainwin);
			(+ story-window +).current_input_request = (+ no-input +);
			!print "(DEBUG) cancel char input mode^";
		}
		if ( (+ story-window +).current_hyperlink_request && ~~wantlinkinput) {
			if (glk_gestalt(gestalt_Hyperlinks, 0))
				glk_cancel_hyperlink_event(gg_mainwin);
			(+ story-window +).current_hyperlink_request = false;
			!print "(DEBUG) cancel hyperlink input mode^";
		}
	
		if ( (+ story-window +).current_input_request ~= (+ line-input +) && wanttextinput == (+ line-input +)) {
			!print "(DEBUG) req line input mode^";
			len = 0;
			val = GProperty(OBJECT_TY, (+ story-window +), (+ preload-input-text +) );
			if (~~TEXT_TY_Empty(val)) {
				len = VM_PrintToBuffer(a_buffer, INPUT_BUFFER_LEN-WORDSIZE, TEXT_TY_Say, val);
			}
			glk_request_line_event(gg_mainwin, a_buffer+WORDSIZE, INPUT_BUFFER_LEN-WORDSIZE, len);
			(+ story-window +).current_input_request = (+ line-input +);
		}
		if ( (+ story-window +).current_input_request ~= (+ char-input +) && wanttextinput == (+ char-input +)) {
			!print "(DEBUG) req char input mode^";
			glk_request_char_event_uni(gg_mainwin);
			(+ story-window +).current_input_request = (+ char-input +);
		}
		if ((+ story-window +).current_hyperlink_request == false && wantlinkinput) {
			if (glk_gestalt(gestalt_Hyperlinks, 0)) {
				!print "(DEBUG) req hyperlink input mode^";
				glk_request_hyperlink_event(gg_mainwin);
				(+ story-window +).current_hyperlink_request = true;
			}
		}

		glk_select(a_event);
		.GotEvent;
		
		! Some required bookkeeping before we invoke the rulebook.
		switch (a_event-->0) {
			evtype_CharInput:
				if (a_event-->1 == gg_mainwin) {
					(+ story-window +).current_input_request = (+ no-input +); ! request complete
				}
			evtype_LineInput:
				if (a_event-->1 == gg_mainwin) {
					(+ story-window +).current_input_request = (+ no-input +); ! request complete
					a_buffer-->0 = a_event-->2;
					if (a_table) {
						VM_Tokenise(a_buffer, a_table);
					}
				}
			evtype_Hyperlink:
				if (a_event-->1 == gg_mainwin) {
					(+ story-window +).current_hyperlink_request = false; ! request complete
				}
		}
		if (gg_commandstr && ~~gg_command_reading)
			RecordingWriteEvent(a_event, a_buffer);
		FollowRulebook((+ accepting input rules +), incontext, true);
		if (RulebookSucceeded()) {
			break;
		}
		! End of loop.
	}
	
	InputRDataFinal();

	! Cancel any remaining input requests.
	if ( (+ story-window +).current_input_request == (+ line-input +) ) {
		glk_cancel_line_event(gg_mainwin, gg_event);
		(+ story-window +).current_input_request = (+ no-input +);
		!print "(DEBUG) cancel line input mode^";
	}
	if ( (+ story-window +).current_input_request == (+ char-input +) ) {
		glk_cancel_char_event(gg_mainwin);
		(+ story-window +).current_input_request = (+ no-input +);
		!print "(DEBUG) cancel char input mode^";
	}
	if ((+ story-window +).current_hyperlink_request) {
		if (glk_gestalt(gestalt_Hyperlinks, 0))
			glk_cancel_hyperlink_event(gg_mainwin);
		(+ story-window +).current_hyperlink_request = false;
		!print "(DEBUG) cancel hyperlink input mode^";
	}
	
	! We can close any quote box that was displayed during the input loop.
	quotewin_close_if_open();

	! When this function exits, the window is (once again) not awaiting any input (except perhaps timer input).
];

[ quotewin_close_if_open;
	if (gg_quotewin) {
		glk_window_close(gg_quotewin, 0);
		gg_quotewin = 0;
	}
];

-) instead of "Keyboard Input" in "Glulx.i6t".

[These phrases provide I7 access to the AwaitInput call.]

To await input in (C - input-context): (- AwaitInput({C}, inputevent, 0, 0); -).
To await input in (C - input-context) with primary buffer: (- AwaitInput({C}, inputevent, buffer, parse); -).
To await input in (C - input-context) with secondary buffer: (- AwaitInput({C}, inputevent2, buffer2, parse2); -).


Section - ParserInput

Include (-

! ParserInput: block and await acceptable input. Returns an event in a_event; tokenized line data will be in a_buffer and a_table.
! This is a wrapper around AwaitInput which adds "OOPS" and "UNDO" support -- features appropriate for the main parser input loop. It also permits the game to customize what kinds of input are accepted for that loop.
! This is called from Parser Letter A (primary command input) and NounDomain (disambig inputs).
! (Context-specific questions, such as YesOrNo and the end-game question, do not use this wrapper. They call AwaitInput directly.)
! In this function, unlike in AwaitInput, a_buffer and a_table are both mandatory. They may be either buffer/table (primary context) or buffer2/table2 (disambiguation context).

[ ParserInput  incontext a_event a_buffer a_table    evtyp nw i w w2 x1 x2;
	! Repeat loop until an acceptable input arrives.
	while (true) {
		! Save the start of the buffer, in case "oops" needs to restore it
		Memcpy(oops_workspace, a_buffer, 64);
		
		! Set up the input requests. (Normally just line input, but the game can customize this.)
		FollowRulebook((+ setting up input rules +), incontext, true);
		
		! The input deed itself.
		AwaitInput(incontext, a_event, a_buffer, a_table);

		! We have an input event now, but it could be any type. If it's line input, it's been tokenized.
		
		evtyp = a_event-->0;
		nw = 0;
		
		if (evtyp == evtype_LineInput) {
			! Set nw to the number of words
			nw = a_table-->0;
		}
		
		#ifndef PASS_BLANK_INPUT_LINES;
		! If the line was blank, get a fresh line.
		if (evtyp == evtype_LineInput && nw == 0) {
			! The old Keyboard routine cleared players_command here (to 100). I'm not sure why. If we're on buffer2/table2, the players_command snippet doesn't apply at all.
			EmptyInputParserError();
			continue;
		}
		#endif; ! PASS_BLANK_INPUT_LINES;
		
		! If this is line input, fetch the opening word.
		w = 0;
		if (evtyp == evtype_LineInput && nw > 0) {
			w = a_table-->1;
		}
		
		! Oops handling
		
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
			Memcpy(a_buffer, oops_workspace, 64);
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
		
			return;
		}

		! Undo handling
		! ### only if the player *could* have entered an UNDO command!
	
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
		
		! Neither OOPS nor UNDO; we're done.
		return;
	}
];

-) instead of "Reading the Command" in "Parser.i6t".


Chapter - High-Level Input Routines

Section - Yes-No Questions

Include (-

! YesOrNo routines. These block and await line input.
! Unlike in ParserInput, the input format cannot be customized. These routines are inherently about getting a typed response.

[ YesOrNo i j;
	for (::) {
		((+ all-input-request-clearing +)-->1)( (+ story-window +) );
		WriteGProperty(OBJECT_TY, (+ story-window +), (+ input-request +),  (+ line-input +) );
		
		AwaitInput( (+ yes-no question context +), inputevent, buffer, parse);
				
		j = parse-->0;
		if (j) { ! at least one word entered
			i = parse-->1;
			if (i == YES1__WD or YES2__WD or YES3__WD) rtrue;
			if (i == NO1__WD or NO2__WD or NO3__WD) rfalse;
		}
		! bad response; try again
		YES_OR_NO_QUESTION_INTERNAL_RM('A');
	}
];

[ YesOrNoPrompt i j incontext;
	incontext = (+ extended yes-no question context +);
	for (::) {
		((+ all-input-request-clearing +)-->1)( (+ story-window +) );
		WriteGProperty(OBJECT_TY, (+ story-window +), (+ input-request +),  (+ line-input +) );
		
		AwaitInput(incontext, inputevent, buffer, parse);
				
		j = parse-->0;
		if (j) { ! at least one word entered
			i = parse-->1;
			if (i == YES1__WD or YES2__WD or YES3__WD) rtrue;
			if (i == NO1__WD or NO2__WD or NO3__WD) rfalse;
		}
		! bad response; try again
		incontext = (+ repeat yes-no question context +);
	}
];

[ YES_OR_NO_QUESTION_INTERNAL_R; ];

-) instead of "Yes/No Questions" in "Parser.i6t".

To decide whether YesOrNoPrompt: (- YesOrNoPrompt() -).

To decide whether player consents asking (T1 - text):
	now the extended yes-no prompt is T1;
	now the repeat yes-no prompt is "Please answer yes or no. ";
	if YesOrNoPrompt:
		decide yes.

To decide whether player consents asking (T1 - text) and (T2 - text):
	now the extended yes-no prompt is T1;
	now the repeat yes-no prompt is T2;
	if YesOrNoPrompt:
		decide yes.

The print the final prompt rule is not listed in any rulebook. [Prompt-printing is now taken care of by the prompt displaying rules.]
The display final status line rule is not listed in any rulebook. [This rule updated status line globals. That's now built into AwaitInput.]


Section - The Final Question

Include (-

! Unlike in ParserInput, the input format cannot be customized. The "standard respond to final question rule" is looking for a typed response, so we must supply one.

[ READ_FINAL_ANSWER_R;
	((+ all-input-request-clearing +)-->1)( (+ story-window +) );
	WriteGProperty(OBJECT_TY, (+ story-window +), (+ input-request +),  (+ line-input +) );
		
	AwaitInput( (+ final question context +), inputevent, buffer, parse);
	
	players_command = 100 + WordCount();
	num_words = WordCount();
	wn = 1;
	rfalse;
];

-) instead of "Read The Final Answer Rule" in "OrderOfPlay.i6t".


Section - Wait For A Key

[Block and wait for a key to be struck. The return value will be a Unicode character or one of the Glk special keycodes.]
To decide what Unicode character is the/-- key waited for: (- InputKeystroke() -).

[These phrases follow the definitions in Basic Screen Effects.]
To wait for any key: (- InputKeysUntilAny(); -).
To wait for the/-- SPACE key: (- InputKeysUntilSpace(); -).

[None of the above phrases print a prompt. The prompt displaying rulebook has a rule which prints nothing for the keystroke-wait context. You can override this rule, or simply print your own prompt before calling the phrase.]

Include (-

[ InputKeystroke;
	((+ all-input-request-clearing +)-->1)( (+ story-window +) );
	WriteGProperty(OBJECT_TY, (+ story-window +), (+ input-request +),  (+ char-input +) );
	
	AwaitInput( (+ keystroke-wait context +), inputevent, 0, 0);

	return inputevent-->2;			
];

! Wait for space or Enter.
[ InputKeysUntilSpace ch;
	while (true) {
		ch = InputKeystroke();
		if (ch == ' ' || ch == keycode_Return)
			return;
	}
];

! Wait for a safe (non-navigating) key. The user might press Down/PgDn or use the mouse scroll wheel to scroll a page of text, so we will ignore those.
[ InputKeysUntilAny ch;
	while (true) {
		ch = InputKeystroke();
		if (ch == keycode_Up or keycode_Down or keycode_PageUp or keycode_PageDown or keycode_Home or keycode_End)
			continue;
		return;
	}
];

-).


Chapter - Parser Code Replacements

Section - Parser__parse

[Replacements for the parser code that used to call Keyboard(). It now calls ParserInput() with a slightly different calling convention. Only the bits of code around the ParserInput() calls has changed.]

Include (-
    parser_results_set = false;

    if (held_back_mode == 1) {
        held_back_mode = 0;
        VM_Tokenise(buffer, parse);
        jump ReParse;
    }

  .ReType;

	cobj_flag = 0;
	actors_location = ScopeCeiling(player);
	
    BeginActivity(READING_A_COMMAND_ACT); if (ForActivity(READING_A_COMMAND_ACT)==false) {
    	.ReParserInput;
		num_words = 0; players_command = 100;
		ParserInput( (+ primary context +), inputevent, buffer, parse);
		if (input_rulebook_data-->IRDAT_RB_CURRENT ~= 0) {
			print "(BUG) Reading-a-command called recursively!^";
		}
		parser_results_set = false;
		InputRDataInit( (+ handling input rules +), inputevent, buffer, parse);
		FollowRulebook((+ handling input rules +), (+ primary context +), true);
		InputRDataFinal();
		if (RulebookFailed()) {
			jump ReParserInput;
		}
		if (inputevent-->0 == evtype_LineInput) {
			num_words = WordCount(); players_command = 100 + num_words;
		}
		if (parser_results_set && parser_results-->ACTION_PRES ~= 0) {
			! If we're not parsing, reading a command shouldn't show the input.
			num_words = 0; players_command = 100;
		}
    } if (EndActivity(READING_A_COMMAND_ACT)) jump ReType;

  .ReParse;
  
	if (parser_results_set && parser_results-->ACTION_PRES ~= 0) {
		! The rulebook gave us an explicit action.
		rtrue;
	}
	
	num_words = 0; players_command = 100;
	if (inputevent-->0 == evtype_LineInput) {
		num_words = WordCount(); players_command = 100 + num_words;
	}
	
	if (num_words == 0) {
		! Either this was a blank line or it was not line input at all. Reject it.
		! (Blank line input could reach this point if the PASS_BLANK_INPUT_LINES option is set.)
		EmptyInputParserError();
		jump ReType;
	}

    parser_inflection = name;

    ! Initially assume the command is aimed at the player, and the verb
    ! is the first word

    wn = 1; inferred_go = false;

    #Ifdef LanguageToInformese;
    LanguageToInformese();
    ! Re-tokenise:
    VM_Tokenise(buffer,parse);
    #Endif; ! LanguageToInformese

    num_words = WordCount(); players_command = 100 + num_words;

    k=0;
    #Ifdef DEBUG;
    if (parser_trace >= 2) {
        print "[ ";
        for (i=0 : i<num_words : i++) {

            #Ifdef TARGET_ZCODE;
            j = parse-->(i*2 + 1);
            #Ifnot; ! TARGET_GLULX
            j = parse-->(i*3 + 1);
            #Endif; ! TARGET_
            k = WordAddress(i+1);
            l = WordLength(i+1);
            print "~"; for (m=0 : m<l : m++) print (char) k->m; print "~ ";

            if (j == 0) print "?";
            else {
                #Ifdef TARGET_ZCODE;
                if (UnsignedCompare(j, HDR_DICTIONARY-->0) >= 0 &&
                    UnsignedCompare(j, HDR_HIGHMEMORY-->0) < 0)
                     print (address) j;
                else print j;
                #Ifnot; ! TARGET_GLULX
                if (j->0 == $60) print (address) j;
                else print j;
                #Endif; ! TARGET_
            }
            if (i ~= num_words-1) print " / ";
        }
        print " ]^";
    }
    #Endif; ! DEBUG
    verb_wordnum = 1;
    actor = player;
    actors_location = ScopeCeiling(player);
    usual_grammar_after = 0;

  .AlmostReParse;

    scope_token = 0;
    action_to_be = NULL;

    ! Begin from what we currently think is the verb word

  .BeginCommand;

    wn = verb_wordnum;
    verb_word = NextWordStopped();

    ! If there's no input here, we must have something like "person,".

    if (verb_word == -1) {
        best_etype = STUCK_PE; jump GiveError;
    }
	if (verb_word == comma_word) {
		best_etype = COMMABEGIN_PE; jump GiveError;
	}

    ! Now try for "again" or "g", which are special cases: don't allow "again" if nothing
    ! has previously been typed; simply copy the previous text across

    if (verb_word == AGAIN2__WD or AGAIN3__WD) verb_word = AGAIN1__WD;
    if (verb_word == AGAIN1__WD) {
        if (actor ~= player) {
            best_etype = ANIMAAGAIN_PE;
			jump GiveError;
        }
        #Ifdef TARGET_ZCODE;
        if (buffer3->1 == 0) {
            PARSER_COMMAND_INTERNAL_RM('D'); new_line;
            jump ReType;
        }
        #Ifnot; ! TARGET_GLULX
        if (buffer3-->0 == 0) {
            PARSER_COMMAND_INTERNAL_RM('D'); new_line;
            jump ReType;
        }
        #Endif; ! TARGET_
        for (i=0 : i<INPUT_BUFFER_LEN : i++) buffer->i = buffer3->i;
        VM_Tokenise(buffer,parse);
		num_words = WordCount(); players_command = 100 + num_words;
    	jump ReParse;
    }

    ! Save the present input in case of an "again" next time

    if (verb_word ~= AGAIN1__WD)
        for (i=0 : i<INPUT_BUFFER_LEN : i++) buffer3->i = buffer->i;

    if (usual_grammar_after == 0) {
        j = verb_wordnum;
        i = RunRoutines(actor, grammar); 
        #Ifdef DEBUG;
        if (parser_trace >= 2 && actor.grammar ~= 0 or NULL)
            print " [Grammar property returned ", i, "]^";
        #Endif; ! DEBUG

        if ((i ~= 0 or 1) && (VM_InvalidDictionaryAddress(i))) {
            usual_grammar_after = verb_wordnum; i=-i;
        }

        if (i == 1) {
            parser_results-->ACTION_PRES = action;
            parser_results-->NO_INPS_PRES = 0;
            parser_results-->INP1_PRES = noun;
            parser_results-->INP2_PRES = second;
            if (noun) parser_results-->NO_INPS_PRES = 1;
            if (second) parser_results-->NO_INPS_PRES = 2;
            rtrue;
        }
        if (i ~= 0) { verb_word = i; wn--; verb_wordnum--; }
        else { wn = verb_wordnum; verb_word = NextWord(); }
    }
    else usual_grammar_after = 0;

-) instead of "Parser Letter A" in "Parser.i6t".


Section - NounDomain

Include (-

[ NounDomain domain1 domain2 context dont_ask
	first_word i j k l answer_words marker;
    #Ifdef DEBUG;
    if (parser_trace >= 4) {
        print "   [NounDomain called at word ", wn, "^";
        print "   ";
        if (indef_mode) {
            print "seeking indefinite object: ";
            if (indef_type & OTHER_BIT)  print "other ";
            if (indef_type & MY_BIT)     print "my ";
            if (indef_type & THAT_BIT)   print "that ";
            if (indef_type & PLURAL_BIT) print "plural ";
            if (indef_type & LIT_BIT)    print "lit ";
            if (indef_type & UNLIT_BIT)  print "unlit ";
            if (indef_owner ~= 0) print "owner:", (name) indef_owner;
            new_line;
            print "   number wanted: ";
            if (indef_wanted == INDEF_ALL_WANTED) print "all"; else print indef_wanted;
            new_line;
            print "   most likely GNAs of names: ", indef_cases, "^";
        }
        else print "seeking definite object^";
    }
    #Endif; ! DEBUG

    match_length = 0; number_matched = 0; match_from = wn;

    SearchScope(domain1, domain2, context);

    #Ifdef DEBUG;
    if (parser_trace >= 4) print "   [ND made ", number_matched, " matches]^";
    #Endif; ! DEBUG

    wn = match_from+match_length;

    ! If nothing worked at all, leave with the word marker skipped past the
    ! first unmatched word...

    if (number_matched == 0) { wn++; rfalse; }

    ! Suppose that there really were some words being parsed (i.e., we did
    ! not just infer).  If so, and if there was only one match, it must be
    ! right and we return it...

    if (match_from <= num_words) {
        if (number_matched == 1) {
            i=match_list-->0;
            return i;
        }

        ! ...now suppose that there was more typing to come, i.e. suppose that
        ! the user entered something beyond this noun.  If nothing ought to follow,
        ! then there must be a mistake, (unless what does follow is just a full
        ! stop, and or comma)

        if (wn <= num_words) {
            i = NextWord(); wn--;
            if (i ~=  AND1__WD or AND2__WD or AND3__WD or comma_word
                   or THEN1__WD or THEN2__WD or THEN3__WD
                   or BUT1__WD or BUT2__WD or BUT3__WD) {
                if (lookahead == ENDIT_TOKEN) rfalse;
            }
        }
    }

    ! Now look for a good choice, if there's more than one choice...

    number_of_classes = 0;

    if (number_matched == 1) {
    	i = match_list-->0;
		if (indef_mode == 1 && indef_type & PLURAL_BIT ~= 0) {
			if (context == MULTI_TOKEN or MULTIHELD_TOKEN or
				MULTIEXCEPT_TOKEN or MULTIINSIDE_TOKEN or
				NOUN_TOKEN or HELD_TOKEN or CREATURE_TOKEN) {
				BeginActivity(DECIDING_WHETHER_ALL_INC_ACT, i);
				if ((ForActivity(DECIDING_WHETHER_ALL_INC_ACT, i)) &&
					(RulebookFailed())) rfalse;
				EndActivity(DECIDING_WHETHER_ALL_INC_ACT, i);
			}
		}
    }
    if (number_matched > 1) {
		i = true;
	    if (number_matched > 1)
	    	for (j=0 : j<number_matched-1 : j++)
				if (Identical(match_list-->j, match_list-->(j+1)) == false)
					i = false;
		if (i) dont_infer = true;
        i = Adjudicate(context);
        if (i == -1) rfalse;
        if (i == 1) rtrue;       !  Adjudicate has made a multiple
                             !  object, and we pass it on
    }

    ! If i is non-zero here, one of two things is happening: either
    ! (a) an inference has been successfully made that object i is
    !     the intended one from the user's specification, or
    ! (b) the user finished typing some time ago, but we've decided
    !     on i because it's the only possible choice.
    ! In either case we have to keep the pattern up to date,
    ! note that an inference has been made and return.
    ! (Except, we don't note which of a pile of identical objects.)

    if (i ~= 0) {
    	if (dont_infer) return i;
        if (inferfrom == 0) inferfrom=pcount;
        pattern-->pcount = i;
        return i;
    }

	if (dont_ask) return match_list-->0;

    ! If we get here, there was no obvious choice of object to make.  If in
    ! fact we've already gone past the end of the player's typing (which
    ! means the match list must contain every object in scope, regardless
    ! of its name), then it's foolish to give an enormous list to choose
    ! from - instead we go and ask a more suitable question...

    if (match_from > num_words) jump Incomplete;

    ! Now we print up the question, using the equivalence classes as worked
    ! out by Adjudicate() so as not to repeat ourselves on plural objects...

	BeginActivity(ASKING_WHICH_DO_YOU_MEAN_ACT);
	if (ForActivity(ASKING_WHICH_DO_YOU_MEAN_ACT)) jump SkipWhichQuestion;
	j = 1; marker = 0;
	for (i=1 : i<=number_of_classes : i++) {
		while (((match_classes-->marker) ~= i) && ((match_classes-->marker) ~= -i))
			marker++;
		if (match_list-->marker hasnt animate) j = 0;
	}
	if (j) PARSER_CLARIF_INTERNAL_RM('A');
	else PARSER_CLARIF_INTERNAL_RM('B');

    j = number_of_classes; marker = 0;
    for (i=1 : i<=number_of_classes : i++) {
        while (((match_classes-->marker) ~= i) && ((match_classes-->marker) ~= -i)) marker++;
        k = match_list-->marker;

        if (match_classes-->marker > 0) print (the) k; else print (a) k;

        if (i < j-1)  print ", ";
        if (i == j-1) {
			#Ifdef SERIAL_COMMA;
			if (j ~= 2) print ",";
        	#Endif; ! SERIAL_COMMA
        	PARSER_CLARIF_INTERNAL_RM('H');
        }
    }
    print "?^";

	.SkipWhichQuestion; EndActivity(ASKING_WHICH_DO_YOU_MEAN_ACT);

    ! ...and get an answer:

  .WhichOne;
    #Ifdef TARGET_ZCODE;
    for (i=2 : i<INPUT_BUFFER_LEN : i++) buffer2->i = ' ';
    #Endif; ! TARGET_ZCODE
    
    .ReParserInput;
    ParserInput( (+ disambiguation context +), inputevent2, buffer2, parse2);
    
	if (input_rulebook_data-->IRDAT_RB_CURRENT ~= 0) {
		print "(BUG) NounDomain called recursively!^";
	}
	parser_results_set = false;
	InputRDataInit( (+ handling input rules +), inputevent, buffer, parse);
	FollowRulebook((+ handling input rules +), (+ disambiguation context +), true);
	InputRDataFinal();
	if (RulebookFailed()) {
		jump ReParserInput;
	}
	
	if (parser_results_set && parser_results-->ACTION_PRES ~= 0) {
		! The rulebook gave us an explicit action. We'll return REPARSE_CODE.
		num_words = 0; players_command = 100;
		jump REPARSE_NO_INPUT;
	}
	
	answer_words = 0;
	if (inputevent2-->0 == evtype_LineInput) {
		#Ifdef TARGET_ZCODE; answer_words = parse2->1; #ifnot; answer_words = parse2-->0; #endif;
	}
	
	if (~~answer_words) {
		! Either this was a blank line or it was not line input at all. Reject it.
		! (Blank line input could reach this point if the PASS_BLANK_INPUT_LINES option is set.)
		EmptyInputParserError();
		jump ReParserInput;
	}

    ! Conveniently, parse2-->1 is the first word in both ZCODE and GLULX.
    first_word = (parse2-->1);

    ! Take care of "all", because that does something too clever here to do
    ! later on:

    if (first_word == ALL1__WD or ALL2__WD or ALL3__WD or ALL4__WD or ALL5__WD) {
        if (context == MULTI_TOKEN or MULTIHELD_TOKEN or MULTIEXCEPT_TOKEN or MULTIINSIDE_TOKEN) {
            l = multiple_object-->0;
            for (i=0 : i<number_matched && l+i<MATCH_LIST_WORDS : i++) {
                k = match_list-->i;
                multiple_object-->(i+1+l) = k;
            }
            multiple_object-->0 = i+l;
            rtrue;
        }
        PARSER_CLARIF_INTERNAL_RM('C');
        jump WhichOne;
    }

	! Look for a comma, and interpret this as a fresh conversation command
	! if so:

	for (i=1 : i<=answer_words : i++)
		if (WordFrom(i, parse2) == comma_word) {
            VM_CopyBuffer(buffer, buffer2);
            jump RECONSTRUCT_INPUT;		
		}

    ! If the first word of the reply can be interpreted as a verb, then
    ! assume that the player has ignored the question and given a new
    ! command altogether.
    ! (This is one time when it's convenient that the directions are
    ! not themselves verbs - thus, "north" as a reply to "Which, the north
    ! or south door" is not treated as a fresh command but as an answer.)

    #Ifdef LanguageIsVerb;
    if (first_word == 0) {
        j = wn; first_word = LanguageIsVerb(buffer2, parse2, 1); wn = j;
    }
    #Endif; ! LanguageIsVerb
    if (first_word ~= 0) {
        j = first_word->#dict_par1;
        if ((0 ~= j&1) && ~~LanguageVerbMayBeName(first_word)) {
            VM_CopyBuffer(buffer, buffer2);
            jump RECONSTRUCT_INPUT;
        }
    }

    ! Now we insert the answer into the original typed command, as
    ! words additionally describing the same object
    ! (eg, > take red button
    !      Which one, ...
    !      > music
    ! becomes "take music red button".  The parser will thus have three
    ! words to work from next time, not two.)

    #Ifdef TARGET_ZCODE;
    k = WordAddress(match_from) - buffer; l=buffer2->1+1;
    for (j=buffer + buffer->0 - 1 : j>=buffer+k+l : j-- ) j->0 = 0->(j-l);
    for (i=0 : i<l : i++) buffer->(k+i) = buffer2->(2+i);
    buffer->(k+l-1) = ' ';
    buffer->1 = buffer->1 + l;
    if (buffer->1 >= (buffer->0 - 1)) buffer->1 = buffer->0;
    #Ifnot; ! TARGET_GLULX
    k = WordAddress(match_from) - buffer;
    l = (buffer2-->0) + 1;
    for (j=buffer+INPUT_BUFFER_LEN-1 : j>=buffer+k+l : j-- ) j->0 = j->(-l);
    for (i=0 : i<l : i++) buffer->(k+i) = buffer2->(WORDSIZE+i);
    buffer->(k+l-1) = ' ';
    buffer-->0 = buffer-->0 + l;
    if (buffer-->0 > (INPUT_BUFFER_LEN-WORDSIZE)) buffer-->0 = (INPUT_BUFFER_LEN-WORDSIZE);
    #Endif; ! TARGET_

    ! Having reconstructed the input, we warn the parser accordingly
    ! and get out.

	.RECONSTRUCT_INPUT;

	num_words = WordCount(); players_command = 100 + num_words;
    wn = 1;
    #Ifdef LanguageToInformese;
    LanguageToInformese();
    ! Re-tokenise:
    VM_Tokenise(buffer,parse);
    #Endif; ! LanguageToInformese
	num_words = WordCount(); players_command = 100 + num_words;
	.REPARSE_NO_INPUT;
    actors_location = ScopeCeiling(player);
	FollowRulebook(Activity_after_rulebooks-->READING_A_COMMAND_ACT);

    return REPARSE_CODE;

    ! Now we come to the question asked when the input has run out
    ! and can't easily be guessed (eg, the player typed "take" and there
    ! were plenty of things which might have been meant).

  .Incomplete;

    if (context == CREATURE_TOKEN) PARSER_CLARIF_INTERNAL_RM('D', actor);
    else                           PARSER_CLARIF_INTERNAL_RM('E', actor);
    new_line;

    #Ifdef TARGET_ZCODE;
    for (i=2 : i<INPUT_BUFFER_LEN : i++) buffer2->i=' ';
    #Endif; ! TARGET_ZCODE
    
    .ReParserInput2;
    ParserInput( (+ disambiguation context +), inputevent2, buffer2, parse2);
    
	if (input_rulebook_data-->IRDAT_RB_CURRENT ~= 0) {
		print "(BUG) NounDomain called recursively!^";
	}
	parser_results_set = false;
	InputRDataInit( (+ handling input rules +), inputevent, buffer, parse);
	FollowRulebook((+ handling input rules +), (+ disambiguation context +), true);
	InputRDataFinal();
	if (RulebookFailed()) {
		jump ReParserInput2;
	}
	
	if (parser_results_set && parser_results-->ACTION_PRES ~= 0) {
		! The rulebook gave us an explicit action. We'll return REPARSE_CODE.
		num_words = 0; players_command = 100;
		jump REPARSE_NO_INPUT;
	}
	
	answer_words = 0;
	if (inputevent2-->0 == evtype_LineInput) {
		#Ifdef TARGET_ZCODE; answer_words = parse2->1; #ifnot; answer_words = parse2-->0; #endif;
	}

	if (~~answer_words) {
		! Either this was a blank line or it was not line input at all. Reject it.
		! (Blank line input could reach this point if the PASS_BLANK_INPUT_LINES option is set.)
		EmptyInputParserError();
		jump ReParserInput2;
	}

	! Look for a comma, and interpret this as a fresh conversation command
	! if so:

	for (i=1 : i<=answer_words : i++)
		if (WordFrom(i, parse2) == comma_word) {
			VM_CopyBuffer(buffer, buffer2);
			return REPARSE_CODE;
		}

    first_word=(parse2-->1);
    #Ifdef LanguageIsVerb;
    if (first_word==0) {
        j = wn; first_word=LanguageIsVerb(buffer2, parse2, 1); wn = j;
    }
    #Endif; ! LanguageIsVerb

    ! Once again, if the reply looks like a command, give it to the
    ! parser to get on with and forget about the question...

    if (first_word ~= 0) {
        j = first_word->#dict_par1;
        if ((0 ~= j&1) && ~~LanguageVerbMayBeName(first_word)) {
            VM_CopyBuffer(buffer, buffer2);
            return REPARSE_CODE;
        }
    }

    ! ...but if we have a genuine answer, then:
    !
    ! (1) we must glue in text suitable for anything that's been inferred.

    if (inferfrom ~= 0) {
        for (j=inferfrom : j<pcount : j++) {
            if (pattern-->j == PATTERN_NULL) continue;
            #Ifdef TARGET_ZCODE;
            i = 2+buffer->1; (buffer->1)++; buffer->(i++) = ' ';
            #Ifnot; ! TARGET_GLULX
            i = WORDSIZE + buffer-->0;
            (buffer-->0)++; buffer->(i++) = ' ';
            #Endif; ! TARGET_

            #Ifdef DEBUG;
            if (parser_trace >= 5)
            	print "[Gluing in inference with pattern code ", pattern-->j, "]^";
            #Endif; ! DEBUG

            ! Conveniently, parse2-->1 is the first word in both ZCODE and GLULX.

            parse2-->1 = 0;

            ! An inferred object.  Best we can do is glue in a pronoun.
            ! (This is imperfect, but it's very seldom needed anyway.)

            if (pattern-->j >= 2 && pattern-->j < REPARSE_CODE) {
                PronounNotice(pattern-->j);
                for (k=1 : k<=LanguagePronouns-->0 : k=k+3)
                    if (pattern-->j == LanguagePronouns-->(k+2)) {
                        parse2-->1 = LanguagePronouns-->k;
                        #Ifdef DEBUG;
                        if (parser_trace >= 5)
                        	print "[Using pronoun '", (address) parse2-->1, "']^";
                        #Endif; ! DEBUG
                        break;
                    }
            }
            else {
                ! An inferred preposition.
                parse2-->1 = VM_NumberToDictionaryAddress(pattern-->j - REPARSE_CODE);
                #Ifdef DEBUG;
                if (parser_trace >= 5)
                	print "[Using preposition '", (address) parse2-->1, "']^";
                #Endif; ! DEBUG
            }

            ! parse2-->1 now holds the dictionary address of the word to glue in.

            if (parse2-->1 ~= 0) {
                k = buffer + i;
                #Ifdef TARGET_ZCODE;
                @output_stream 3 k;
                 print (address) parse2-->1;
                @output_stream -3;
                k = k-->0;
                for (l=i : l<i+k : l++) buffer->l = buffer->(l+2);
                i = i + k; buffer->1 = i-2;
                #Ifnot; ! TARGET_GLULX
                k = Glulx_PrintAnyToArray(buffer+i, INPUT_BUFFER_LEN-i, parse2-->1);
                i = i + k; buffer-->0 = i - WORDSIZE;
                #Endif; ! TARGET_
            }
        }
    }

    ! (2) we must glue the newly-typed text onto the end.

    #Ifdef TARGET_ZCODE;
    i = 2+buffer->1; (buffer->1)++; buffer->(i++) = ' ';
    for (j=0 : j<buffer2->1 : i++,j++) {
        buffer->i = buffer2->(j+2);
        (buffer->1)++;
        if (buffer->1 == INPUT_BUFFER_LEN) break;
    }
    #Ifnot; ! TARGET_GLULX
    i = WORDSIZE + buffer-->0;
    (buffer-->0)++; buffer->(i++) = ' ';
    for (j=0 : j<buffer2-->0 : i++,j++) {
        buffer->i = buffer2->(j+WORDSIZE);
        (buffer-->0)++;
        if (buffer-->0 == INPUT_BUFFER_LEN) break;
    }
    #Endif; ! TARGET_

    ! (3) we fill up the buffer with spaces, which is unnecessary, but may
    !     help incorrectly-written interpreters to cope.

    #Ifdef TARGET_ZCODE;
    for (: i<INPUT_BUFFER_LEN : i++) buffer->i = ' ';
    #Endif; ! TARGET_ZCODE

	jump RECONSTRUCT_INPUT; ! fix c.f. Mantis 1694
    !return REPARSE_CODE;

]; ! end of NounDomain

[ PARSER_CLARIF_INTERNAL_R; ];

-) instead of "Noun Domain" in "Parser.i6t".


Section - Test Input

Include (-

#Iftrue ({-value:NUMBER_CREATED(test_scenario)} > 0);

[ TestScriptSub;
	switch(special_word) {
{-call:PL::Parsing::TestScripts::compile_switch}
	default:
		print ">--> The following tests are available:^";
{-call:PL::Parsing::TestScripts::compile_printout}
	}
];

#ifdef TARGET_GLULX;
Constant TEST_STACK_SIZE = 128;
#ifnot;
Constant TEST_STACK_SIZE = 48;
#endif;

Array test_stack --> TEST_STACK_SIZE;
Global test_sp = 0;
[ TestStart T R l k;
	if (test_sp >= TEST_STACK_SIZE) ">--> Testing too many levels deep";
	test_stack-->test_sp = T;
	test_stack-->(test_sp+1) = 0;
	test_stack-->(test_sp+3) = l;
	test_sp = test_sp + 4;
	if ((R-->0) && (R-->0 ~= real_location)) {
	     print "(first moving to ", (name) R-->0, ")^";
	     PlayerTo(R-->0, 1);
	}
	k=1;
	while (R-->k) {
	    if (R-->k notin player) {
	        print "(first acquiring ", (the) R-->k, ")^";
	        move R-->k to player;
	    }
	    k++;
	}
	print "(Testing.)^"; say__p = 1;
];

! CheckTestInput: If a test input is pending, this fills out the event and buffer structure and returns true. Otherwise it returns false.
! This function is allowed to return any event type (even arrange or timer events). However, Inform's current TEST-ME system can only generate line input events. So this code only returns events of that type.
! Note that a_buffer may be 0. If so, we cannot return a line input event, so we do nothing. (We could return other event types if we had them.)
! (This replaces TestKeyboardPrimitive, but is invoked differently. We no longer call through to VM_ReadKeyboard when no tests are available. We just return false. Also, arguments are event/buffer instead of buffer/table.)

[ CheckTestInput a_event a_buffer    p i j l spaced ch;
	if (test_sp == 0) {
		test_stack-->2 = 1;
		return false;
	}
	
	if (~~a_buffer) {
		return false;
	}

	a_event-->0 = evtype_None;
	
	p = test_stack-->(test_sp-4);
	i = test_stack-->(test_sp-3);
	l = test_stack-->(test_sp-1);
	print "[";
	print test_stack-->2;
	print "] ";
	test_stack-->2 = test_stack-->2 + 1;
	style bold;
	while ((i < l) && (p->i ~= '/')) {
		ch = p->i;
		if (spaced || (ch ~= ' ')) {
			if ((p->i == '[') && (p->(i+1) == '/') && (p->(i+2) == ']')) {
				ch = '/'; i = i+2;
			}
			a_buffer->(j+WORDSIZE) = ch;
			print (char) ch;
			i++; j++;
			spaced = true;
		} else i++;
	}
	style roman;
	print "^";
	
	! Fill out the event structure.
	a_event-->0 = evtype_LineInput;
	a_event-->1 = gg_mainwin;
	a_event-->2 = j;
	! (We don't have to tokenize; AwaitInput will handle that.)
	
	if (p->i == '/') i++;
	if (i >= l) {
		test_sp = test_sp - 4;
	} else test_stack-->(test_sp-3) = i;
	
	return true;
];

#IFNOT;

[ TestScriptSub;
	">--> No test scripts exist for this game.";
];

#ENDIF;

-) instead of "Test Command" in "Tests.i6t".

Section - Command Stream Input

Include (-

#ifdef DEBUG;

! RecordingReadEvent: If a line of input is pending from the command stream, this fills out the event and buffer structure and returns true. Otherwise it returns false.
! If the command stream ends, this closes it.
[ RecordingReadEvent a_event a_buffer;
	if (~~gg_commandstr)
		rfalse;
	if (~~gg_command_reading)
		rfalse;
	!###
	rfalse;
];

! RecordingWriteEvent: Write an event to the command stream.
[ RecordingWriteEvent a_event a_buffer   val;
	if (~~gg_commandstr)
		return;
	if (gg_command_reading)
		return;
	
	switch (a_event-->0) {
	
		evtype_CharInput:
			RecordingWriteRawString(">$char ");
			val = a_event-->2;
			if (val == ' ') {
				RecordingWriteRawString("space");
			}
			else if (val < ' ' || UnicodeCharIsSpecial(val)) {
				RecordingWriteRawFunc(PrintUnicodeSpecialName, val);
			}
			else {
				glk_put_char_stream_uni(gg_commandstr, val);
			}
			glk_put_char_stream(gg_commandstr, 10); ! newline
			
		evtype_LineInput:
			glk_put_char_stream(gg_commandstr, '>');
			glk_put_char_stream(gg_commandstr, ' ');
			if (a_buffer)
				glk_put_buffer_stream(gg_commandstr, a_buffer+WORDSIZE, a_buffer-->0);
			glk_put_char_stream(gg_commandstr, 10); ! newline
			
		evtype_Hyperlink:
			RecordingWriteRawString(">$link ");
			RecordingWriteRawNumber(a_event-->2);
			glk_put_char_stream(gg_commandstr, 10); ! newline
			
	}
];

[ RecordingWriteRawString val  oldstr;
	oldstr = glk_stream_get_current();
	glk_stream_set_current(gg_commandstr);
	print (string) val;
	glk_stream_set_current(oldstr);
];

[ RecordingWriteRawNumber val  oldstr;
	oldstr = glk_stream_get_current();
	glk_stream_set_current(gg_commandstr);
	print val;
	glk_stream_set_current(oldstr);
];

[ RecordingWriteRawFunc func val  oldstr;
	oldstr = glk_stream_get_current();
	glk_stream_set_current(gg_commandstr);
	func(val);
	glk_stream_set_current(oldstr);
];

#ifnot; ! DEBUG

! Stubs which do nothing in release mode.

[ RecordingReadEvent a_event a_buffer;
	rfalse;
];

[ RecordingWriteEvent a_event a_buffer;
	rfalse;
];

#endif; ! DEBUG

-).


Section - Miscellaneous

Include (-

! This prints the "I beg your pardon" parser error. We use this when the player enters a blank line, or a input event which is not text at all (and which is not otherwise handled). There are several points in the code where we check this, so it makes sense to factor out a simple function.
[ EmptyInputParserError   oldetype;
	oldetype = etype; etype = BLANKLINE_PE;
	BeginActivity(PRINTING_A_PARSER_ERROR_ACT);
	if (ForActivity(PRINTING_A_PARSER_ERROR_ACT) == false) {
		PARSER_ERROR_INTERNAL_RM('X'); new_line;
	}
	EndActivity(PRINTING_A_PARSER_ERROR_ACT);
	etype = oldetype;
];

-) after "Reading the Command" in "Parser.i6t".

Include (-
! KeyboardPrimitive no longer exists.
-) instead of "Keyboard Primitive" in "Parser.i6t".

Include (-
! PrintPrompt no longer exists.
-) instead of "Prompt" in "Printing.i6t".


Unified Glulx Input ends here.

---- DOCUMENTATION ----

Here's the summary:

All input will occur in an AwaitInput routine. This will be able to return any kind of event (not just a text buffer). It will display the prompt, draw/redraw the status line, accept input, filter it for the caller, and return it.

Displaying the prompt and filtering input events will become rulebooks, so that games can customize them. (The HandleGlkEvent hook will change to a rulebook.)

AwaitInput will be responsible for requesting and cancelling Glk input. (Again, customizable via rulebook, so games can add custom input types.)

The parser will invoke a rulebook/activity to convert new event types directly into actions.

The first draft will be concerned only with the story window. I will extend it to status window input later. Eventually it will have to be compatible with the Multiple Windows extension, but I don't know whether that means adapting this extension to that one or vice versa. (Probably both.)

----

Unified Glulx Input is an attempt to tidy up all the messy I6 APIs that you need to customize your game's input system. 

The Glulx Entry Points extension does this already, but that exposes all the mess -- you have to understand how Glk works to use to correctly. Unified Glulx Input tries to offer you a simple model which handles common cases easily.

Chapter: Basic concepts

Section: Input-contexts: why are we awaiting input?

A game can stop and await input for many reasons. It can be waiting for a command, or for a yes-or-no answer (an "if the player consents..." test). It can be waiting for the player to hit any key to continue.

In Unified Glulx Input, all of these inputs invoke the same mechanism. The mechanism can be customized to behave in different ways -- printing different prompts, waiting for different sorts of input. But it's always the same mechanism underneath.

You customize it by writing rules. The extension has four rulebooks which cover various aspects of the problem. The default rules cover the normal input situations of an IF game -- the ones mentioned above. Of course, you can modify these or add more.

We distinguish these situations with a value called an "input-context". The rulebooks we mentioned are input-context based rulebooks. The UGI extension comes with seven:

	primary context, disambiguation context (the command input-contexts)
	yes-no question context, extended yes-no question context, repeat yes-no question context (the yes-no input-contexts)
	final question context
	keystroke-wait context

These are more finely divided than you usually need to bother with. For example, the game might be waiting for a normal command (primary context) or for a disambiguation reply (disambiguation context). In most games these will appear the same. So you can handle them together. For example:

	Prompt displaying rule for a command input-context:
		instead say ">>>";

This rule changes the command prompt for both primary and disambiguation inputs.

Section: G-events: what kind of input are we awaiting?

Most IF input comes as lines of text. But a game can also accept keystroke input, as in a "hit any key" prompt or a menu controlled with arrow keys.

Glulx currently supports eight types of input, described as "g-event" values:

	char-event - a keystroke
	line-event - a line of text
	hyperlink-event - selection of a hyperlink
	timer-event - event repeated at fixed intervals
	mouse-event - a mouse click
	arrange-event - window sizes have changed
	redraw-event - graphics windows need redrawing
	sound-notify-event - sound finished playing

Section: Glk-windows: what window is awaiting input?

This version of UGI is only concerned with one window, the "story-window". You can set various properties of the story-window object to customize its behavior.

There is also a "status-window" object, but it is not yet functional. Future versions of UGI will support this. Future versions will also integrate with the Multiple Windows extension to support multi-window games.

Chapter: High-level tasks

These phrases can be used with no deeper knowledge of UGI.

Section: Yes-no questions

	if the player consents: ...

This works just as it always has (WWI 11.5); it waits for the player to type "yes" or "no". You are expected to print the question first.

	if the player consents asking (T1 - text): ...
	if the player consents asking (T1 - text) and (T2 - text): ...

Similar, except UGI uses your text as the prompt (through the magic of the prompt displaying rulebook). T1 should be the initial question; T2 is a followup if the player fails to answer. If you don't supply T2, it defaults to "Please answer yes or no."

Section: Waiting for a key

These phrases are copied from the Basic Screen Effects extension; they have been updated here to work with UGI.

	wait for any key
	wait for the SPACE key

By default no prompt is printed. You can print your own beforehand, or write a prompt displaying rule:

	Prompt displaying rule for keystroke-wait context:
		say ">>>";

If you want to wait for a key and pay attention to the value:

	let C be the key waited for
	
The result will be a Unicode character. (Include the Unicode Character Names extension for a complete list, or my ASCII Character Names extension for the basic ones.) The result may also be a special value representing a control key:

	special keycode left, special keycode right, special keycode up, special keycode down, special keycode return, special keycode delete, special keycode escape, special keycode tab, special keycode pageup, special keycode pagedown, special keycode home, special keycode end, special keycode func1, ... special keycode func12

These are not part of the Unicode spec, but I7 represents them as Unicode character values. (They are outside the official Unicode range of $0 to $10FFFF.) Don't try to print them or insert them into normal texts -- they have no normal printed representation. If you need to print a special character, use this phrase:

	say extended (C - Unicode character)

This will print special characters as "left", "right", and so on. Normal Unicode characters will be printed directly.

Chapter: The four rulebooks

We've talked about the four rulebooks, and now it's time to introduce them:

	setting up input rules - decide what kinds of input are desired
	prompt displaying rules - display a prompt, traditionally ">"
	accepting input rules - accept, reject, or alter individual input events
	handling input rules - convert an input event into an action

Section: Setting up input rules

####

Section: Prompt displaying rules

####

Section: Accepting input rules

####
### least used
### the event-peering phrases

Section: Handling input rules

####
### accept as action

Chapter: ### low-level invocations

Chapter: ### what about the reading a command activity?

Chapter: ### under the hood -- parser changes


Example: * Changing the Prompt - Changing the command prompt in various contexts.

The old "command prompt" global variable still works, but we'd like a more rule-based approach. Here we use "What now?" for the command prompt (both primary and disambiguation) and "Answer now!" for the final question.

We also use an extended form of the "player consents" phrase, in which we supply the prompt question to use.

	*: "Changing the Prompt"
	
	Include Unified Glulx Input by Andrew Plotkin.

	Prompt displaying rule for a command input-context:
		instead say "What now? ".

	Prompt displaying rule for the final question context:
		instead say "Answer now! ".

	The Kitchen is a room. "You are in a kitchen."
	The Outdoors is outside from the Kitchen.

	Check going outside:
		if the player consents asking "Outdoors is scary. Are you sure?" and "That was a yes/no question.":
			instead end the story finally;
		else:
			instead say "You fail to overcome your agoraphobia.";

	Test me with "out / maybe / no / out / yes".


Example: ** Keystroke Input - Controlling the game with single keystrokes.

In this example, the underworld uses a different input mechanism: single keystrokes. Character events are translated into line input for the parser. (This is a crude approach; see the next example for a tidier model.)

	*: "Keystroke Input"

	Include Unified Glulx Input by Andrew Plotkin.
	Include Unicode Character Names by Graham Nelson.

	The Kitchen is a room. "You are in a kitchen. An open trap door beckons you downward."

	Aboveground is a region. The Kitchen is in Aboveground.

	Maze10 is a room. "You are in a maze of twisty passages, basically all alike."
	Maze20 is a room. "You are in a maze of twisty passages, all pretty much alike."
	Maze01 is a room. "You are in a maze of twisty passages, all basically alike."
	Maze11 is a room. "You are in a maze of twisty passages, all kind of alike."
	Maze21 is a room. "You are in a maze of twisty passages, more or less all alike."
	Maze02 is a room. "You are in a maze of twisty passages, pretty much all alike."
	Maze12 is a room. "You are in a maze of twisty passages, all alike."
	Maze22 is a room. "You are in a maze of twisty passages, all more or less alike."
	Maze32 is a room. "You are in a maze of twisty passages, all sort of alike."
	Maze03 is a room. "You are in a maze of twisty passages, kind of all alike."
	Maze13 is a room. "You are in a maze of twisty passages, all quite alike."
	Maze23 is a room. "You are in a maze of twisty passages, quite all alike."
	Maze33 is a room. "You are in a maze of twisty passages, sort of all alike."

	Maze12 is below the Kitchen.
	Maze10 is west of Maze20. 
	Maze10 is north of Maze11. Maze20 is north of Maze21.
	Maze01 is west of Maze11. Maze11 is west of Maze21.
	Maze01 is north of Maze02. Maze11 is north of Maze12.
	Maze02 is west of Maze12. Maze12 is west of Maze22. Maze22 is west of Maze32.
	Maze12 is north of Maze13. Maze22 is north of Maze23. Maze32 is north of Maze33.
	Maze03 is west of Maze13. Maze23 is west of Maze33.

	Rule for printing the name of a room (called R) when R is not in Aboveground:
		say "Maze".

	Check going down from the Kitchen:
		say "(Down here, single-keystroke commands rule. Use the arrow keys or NSEW to move around; U or escape to quit.)";
		continue the action.

	Check going up when the location is not in Aboveground:
		say "You fumble your way back to the light.";
		now the player is in the Kitchen;
		stop the action.

	Prompt displaying rule when the location is not in Aboveground:
		instead say "==>".

	Setting up input rule when the location is not in Aboveground:
		now the input-request of the story-window is char-input;
		rule succeeds.

	Accepting input rule when the location is not in Aboveground and handling char-event:
		let C be the current input event character;
		if C is special keycode left or C is Unicode Latin small letter w or C is Unicode Latin capital letter W:
			say "GO WEST[line break]";
			replace the current input event with the line "go west";
			rule succeeds;
		if C is special keycode right or C is Unicode Latin small letter e or C is Unicode Latin capital letter E:
			say "GO EAST[line break]";
			replace the current input event with the line "go east";
			rule succeeds;
		if C is special keycode up or C is Unicode Latin small letter n or C is Unicode Latin capital letter N:
			say "GO NORTH[line break]";
			replace the current input event with the line "go north";
			rule succeeds;
		if C is special keycode down or C is Unicode Latin small letter s or C is Unicode Latin capital letter S:
			say "GO SOUTH[line break]";
			replace the current input event with the line "go south";
			rule succeeds;
		if C is special keycode escape or C is Unicode Latin small letter u or C is Unicode Latin capital letter U:
			say "GO UP[line break]";
			replace the current input event with the line "go up";
			rule succeeds;
		say "('[extended C]' is not a valid key.)";
		reject the input event.

Example: ** Keystroke Input 2 - Controlling the game with single keystrokes.

This is nearly the same as the previous example. But now, instead of changing char input events to line input events, we handle char events directly as going actions.

The work is now done in the accepting input rulebook. We no longer pretend that we're entering line input, and the parser is entirely bypassed.

	*: "Keystroke Input 2"

	Include Unified Glulx Input by Andrew Plotkin.
	Include Unicode Character Names by Graham Nelson.

	The Kitchen is a room. "You are in a kitchen. An open trap door beckons you downward."

	Aboveground is a region. The Kitchen is in Aboveground.

	Maze10 is a room. "You are in a maze of twisty passages, basically all alike."
	Maze20 is a room. "You are in a maze of twisty passages, all pretty much alike."
	Maze01 is a room. "You are in a maze of twisty passages, all basically alike."
	Maze11 is a room. "You are in a maze of twisty passages, all kind of alike."
	Maze21 is a room. "You are in a maze of twisty passages, more or less all alike."
	Maze02 is a room. "You are in a maze of twisty passages, pretty much all alike."
	Maze12 is a room. "You are in a maze of twisty passages, all alike."
	Maze22 is a room. "You are in a maze of twisty passages, all more or less alike."
	Maze32 is a room. "You are in a maze of twisty passages, all sort of alike."
	Maze03 is a room. "You are in a maze of twisty passages, kind of all alike."
	Maze13 is a room. "You are in a maze of twisty passages, all quite alike."
	Maze23 is a room. "You are in a maze of twisty passages, quite all alike."
	Maze33 is a room. "You are in a maze of twisty passages, sort of all alike."

	Maze12 is below the Kitchen.
	Maze10 is west of Maze20.
	Maze10 is north of Maze11. Maze20 is north of Maze21.
	Maze01 is west of Maze11. Maze11 is west of Maze21.
	Maze01 is north of Maze02. Maze11 is north of Maze12.
	Maze02 is west of Maze12. Maze12 is west of Maze22. Maze22 is west of Maze32.
	Maze12 is north of Maze13. Maze22 is north of Maze23. Maze32 is north of Maze33.
	Maze03 is west of Maze13. Maze23 is west of Maze33.

	Rule for printing the name of a room (called R) when R is not in Aboveground:
		say "Maze".

	Check going down from the Kitchen:
		say "(Down here, single-keystroke commands rule. Use the arrow keys or NSEW to move around; U or escape to quit.)";
		continue the action.

	Check going up when the location is not in Aboveground:
		say "You fumble your way back to the light.";
		now the player is in the Kitchen;
		stop the action.

	Prompt displaying rule when the location is not in Aboveground:
		instead say "==>".

	Setting up input rule when the location is not in Aboveground:
		now the input-request of the story-window is char-input;
		rule succeeds.

	Handling input rule when the location is not in Aboveground and handling char-event:
		let C be the current input event character;
		if C is special keycode left or C is Unicode Latin small letter w or C is Unicode Latin capital letter W:
			say "(You try going west...)";
			handle the current input event as the action of going west;
			rule succeeds;
		if C is special keycode right or C is Unicode Latin small letter e or C is Unicode Latin capital letter E:
			say "(You try going east...)";
			handle the current input event as the action of going east;
			rule succeeds;
		if C is special keycode up or C is Unicode Latin small letter n or C is Unicode Latin capital letter N:
			say "(You try going north...)";
			handle the current input event as the action of going north;
			rule succeeds;
		if C is special keycode down or C is Unicode Latin small letter s or C is Unicode Latin capital letter S:
			say "(You try going south...)";
			handle the current input event as the action of going south;
			rule succeeds;
		if C is special keycode escape or C is Unicode Latin small letter u or C is Unicode Latin capital letter U:
			say "(You try going up...)";
			handle the current input event as the action of going up;
			rule succeeds;
		say "('[extended C]' is not a valid key.)";
		reject the input event.


Example: ** A Study In Memoriam - A pure-hyperlink game.

Here we drop text input entirely. (Dropping the standard parser input line request rule accomplishes this.) Instead we set up input to be hyperlink-based. Hyperlink events are translated into the examining action, which happily works for both on-stage objects and off-stage memories.

	*: "A Study In Memoriam"

	Include Unified Glulx Input by Andrew Plotkin.

	The Study is a room. "You are in your cluttered study."

	The messy desk is a supporter in the Study. The description is "You found this battered antique in a battered [antique shop] in Chapel Hill. As you recall, it was sitting behind [a microscope]."

	A fossil is on the desk. The description is "Some species of [ammonite]."

	A copper fulgurite is on the desk. The description is "It's a chunk of melted copper that you picked up from the base of a telephone pole. It's not really fulgurite, minerallogically speaking. But it [italic type]was[roman type] formed by [lightning]!"

	A memory is a kind of thing.

	The ammonite is a memory. The description is "You've dreamed of drifting through antediluvian seas, the master of all you survey... Then the bony fishes come. The damned bony fishes."

	The microscope is a memory. The printed name is "defunct electron microscope". The description is "Yes, you once discovered an electron microsope in [an antique shop]. It was a console device, like a kitchen counter with switches all over the top and vacuum pumps hanging out underneath. You wonder if anyone ever purchased it."

	The antique shop is a memory. The description is "You discovered a mad antique shop on a years-ago trip to North Carolina. The prize of its collection was [a microscope], though you doubt anyone else would think of it that way. You bought [hyperlink desk]an old desk[/hyperlink] instead; the very one in this room."

	The lightning is a memory. The description is "One can never remember lightning properly."

	To say hyperlink (O - object): (- if (glk_gestalt(gestalt_Hyperlinks, 0)) { glk_set_hyperlink({O}); } -).
	To say /hyperlink: (- if (glk_gestalt(gestalt_Hyperlinks, 0)) { glk_set_hyperlink(0); } -).

	Rule for printing the name of something (called O):
		say "[hyperlink O][printed name of O][/hyperlink]";

	The standard parser input line request rule does nothing.

	Setting up input rule:
		now the story-window is hyperlink-input-request.

	Handling input rule for a command input-context when handling hyperlink-event:
		let O be current input event hyperlink object;
		if O is nothing:
			reject the input event;
		handle the current input event as the action of examining O;
		rule succeeds.	


Example: *** Requesting a Number - A phrase to query the player for a number.

In this example we ask the player for a number. It's very much like "if the player consents..." except that we get back a number rather than a yes-or-no decision.

We'll want a new input-context for this operation, so we invent one: numeric context. Our prompt displaying rule relies on this to give the player a special prompt for the number-typing operation.

The "number waited for" phrase demonstrates doing a low-level input operation. First we must clear all input requests for the story window. (We don't want a request left over from regular command input.) Then we set the input request we want -- line input only. Then we "await input", using our special numeric context.

Then it gets a little bit messy. There's no easy I7 access to the input buffer. We can't use "the player's command" because that's a parser variable. We're doing this behind the parser's back, so no player's command is ever set. Nor can we match against an I7 token like "[number]".

Instead, we rely on an existing I6 function called TryNumber. This returns the parsed number, or -1000 if no number was typed. Loop until we get something valid.

Unfortunately, TryNumber is set up to use the parser's input buffer. That's why the input phrase is

	await input in numeric context with primary buffer;

This winds up stomping on "the player's command"; that snippet will be invalid for the rest of the turn. Too bad! (Really, "if the player consents" has the same bug.)

A better implementation would rely on the secondary buffer. We couldn't use TryNumber, though, so it would be a longer and messier example.

	*: "Requesting a Number"

	Include Unified Glulx Input by Andrew Plotkin.

	The Alley is a room. "You are in that alley from that spy game. A steel door is to the east; next to it is a button and a speaker grille."

	The steel door is scenery in the Alley. The description is "The door is closed and locked."

	Check entering the steel door:
		instead say "The door isn't open."
	Check going east in the Alley:
		instead try entering the steel door.
	Check going south in the Alley:
		instead say "You can't quit now."

	The secret number is initially 0.

	The button is scenery in the Alley.

	Check pushing the button:
		if the secret number is 0:
			now the secret number is a random number between 100 and 500;
		say "A crackly voice asks, 'What's the secret number?'";
		let N be the number waited for;
		if N is the secret number:
			say "'Did you say [N]? You're right!' The door pops open and someone on the other side tasers you.";
			end the story finally;
		else:
			say "'Did you say [N]? That's not right. The secret number today is [secret number].'[paragraph break]The door does not open.";
		stop the action.

	Numeric context is an input-context.

	Prompt displaying rule for numeric context:
		instead say "==>".

	To decide what number is the number waited for:
		while 1 is 1:
			clear all input requests for the story-window;
			now the input-request of the story-window is line-input;
			await input in numeric context with primary buffer;
			let N be the hacky low-level number check;
			if N is not -1000:
				decide on N;
			say "Please enter a number.";

	To decide what number is the hacky low-level number check: (- TryNumber(1) -).

