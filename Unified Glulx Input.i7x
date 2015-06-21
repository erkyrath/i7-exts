Version 1 of Unified Glulx Input (for Glulx only) by Andrew Plotkin begins here.

Input-context is a kind of value. The input-contexts are primary context, disambig context, yes-no question context, extended yes-no question context, repeat yes-no question context, final question context.

The prompt displaying rules are an input-context based rulebook.

The extended yes-no prompt is a text that varies.
The repeat yes-no prompt is a text that varies.

Rule for prompt displaying the yes-no question context (this is the yes-no question prompt rule):
	instead say ">" (A).
Rule for prompt displaying the extended yes-no question context (this is the extended yes-no question prompt rule):
	instead say "[extended yes-no prompt] >" (A).
Rule for prompt displaying the repeat yes-no question context (this is the repeat yes-no question prompt rule):
	instead say "[repeat yes-no prompt] >" (A).
Rule for prompt displaying the final question context (this is the final question prompt rule):
	instead follow the print the final prompt rule.
Rule for prompt displaying an input-context (this is the default prompt rule):
	instead say the command prompt.
The final question prompt rule is listed last in the prompt displaying rules.
The yes-no question prompt rule is listed last in the prompt displaying rules.
The extended yes-no question prompt rule is listed last in the prompt displaying rules.
The repeat yes-no question prompt rule is listed last in the prompt displaying rules.
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
Array inputevent2 --> 4;
-) after "Variables and Arrays" in "Glulx.i6t";

Include (-

! AwaitInput: block and await an acceptable input. What "acceptable" means is customizable. Typically the caller will be interested in some event types (e.g., line input), will allow others to do their job (arrange events redrawing the status window), and will ignore the rest (keep awaiting input).
! This is the low-level entry point to the Glk input system; all input requests funnel down to this function. It sets up the Glk input request events and calls glk_select().
! This function also handles displaying the prompt and redrawing the status line. (Through customizable rulebooks and activities, of course.)
! AwaitInput takes three arguments: an event structure, a line input buffer, and a buffer for parsing words from line input. (If the caller is not interested in line input, the latter two arguments are ignored.)

[ AwaitInput incontext a_event a_buffer a_table    done;
	a_event-->0 = evtype_None;
	
	! ### probably we put prompt-and-status inside the loop
	
	! This block emulates the old PrintPrompt call. ### make activity before/after?
	! ### How do we show runtime problems for a game with no prompts? Maybe this should *not* going in the before-prompt-printing stage. But then, maybe we're skipping the prompt because keyboard input is already active! In which case we can't print anything. Sigh.
	RunTimeProblemShow();
	ClearRTP();
	style roman;
	! In the old-fashioned YesOrNo sequence, we want to print the prompt after the caller's printed question, with no line break. In all other cases, we ensure a line break.
	if (incontext ~= (+ yes-no question context +) ) {
		EnsureBreakBeforePrompt();
	}
	FollowRulebook((+ prompt displaying rules +), incontext, true);
	ClearBoxedText();
	ClearParagraphing(14);
	
	! ### test or command-stream input
	
	! ### really we should be able to continue line/char input here -- skip prompt
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
	sline1 = score; sline2 = turns;
	if (location ~= nothing && parent(player) ~= nothing) DrawStatusLine();
	
	done = false;
	while (~~done) {
		! We always use gg_event as a short-term event buffer (as does the rest of the library). The a_event argument refers to a separate buffer which the caller provides to return an event in.
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

[ ParserInput  a_event a_buffer a_table    nw i w w2 x1 x2;
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
        YES_OR_NO_QUESTION_INTERNAL_RM('A');
    }
];

[ YesOrNoPrompt i j incontext;
	incontext = (+ extended yes-no question context +);
    for (::) {
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
	now the repeat yes-no prompt is "Please answer yes or no";
	if YesOrNoPrompt:
		decide yes.

To decide whether player consents asking (T1 - text) and (T2 - text):
	now the extended yes-no prompt is T1;
	now the repeat yes-no prompt is T2;
	if YesOrNoPrompt:
		decide yes.

The print the final prompt rule is not listed in any rulebook. [Prompt-printing is now taken care of by the prompt displaying rules.]
The display final status line rule is not listed in any rulebook. [This rule updated status line globals. That's now built into AwaitInput.]

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

    if (held_back_mode == 1) {
        held_back_mode = 0;
        VM_Tokenise(buffer, parse);
        jump ReParse;
    }

  .ReType;

	cobj_flag = 0;
	actors_location = ScopeCeiling(player);
    BeginActivity(READING_A_COMMAND_ACT); if (ForActivity(READING_A_COMMAND_ACT)==false) {
		ParserInput(inputevent, buffer,parse);
		num_words = WordCount(); players_command = 100 + num_words;
    } if (EndActivity(READING_A_COMMAND_ACT)) jump ReType;

  .ReParse;

    parser_inflection = name;

    ! Initially assume the command is aimed at the player, and the verb
    ! is the first word

    num_words = WordCount(); players_command = 100 + num_words;
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
    answer_words=ParserInput(inputevent, buffer2, parse2);

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
    answer_words = ParserInput(inputevent, buffer2, parse2);

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

    return REPARSE_CODE;

]; ! end of NounDomain

[ PARSER_CLARIF_INTERNAL_R; ];

-) instead of "Noun Domain" in "Parser.i6t".

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

ParserInput: same call context. (Except the return value changes. It now returns an input event representation, of which the buffer/parse is just one part.) (We should add a third array argument for nontextual input.) (And rename it to ParserInput!)
- loop until nonblank: (make this optional?)
- save oops_workspace (if line input is not in progress!)
- set up line input request (ditto!)
- AwaitInput
- do oops substitution (ditto!)
- handle undo, save undo (ditto?)
- return result of AwaitInput

VM_KeyDelay: now a high-level call, parallel to Parser__parse use of ParserInput. (Rename...)
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

Now, when Parser__parse calls ParserInput, it may discover a non-textbuffer result. It should fill out parser_results (via rulebook) and immediately return. Skip over all again/buffering and similar textual hackery. If the rulebook doesn't decide on a result, we have a "parser error"; complain and jump to ReType.

The secondary ParserInput calls (in NounDomain) cause messiness. They should certainly cause NounDomain to return REPARSE_CODE (the "treat as new command" case). Some NounDomain calls are in Parser__parse and loop straight back to Reparse, which is fine; that leads to the rulebook-and-return code path. Others are in ParseToken__. Those worry me.

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
