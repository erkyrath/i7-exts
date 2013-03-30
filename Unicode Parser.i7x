Version 2 of Unicode Parser (for Glulx only) by Andrew Plotkin begins here.

[Tell the I6 compiler to generate a dictionary containing Unicode values rather than 8-bit characters. This requires I6 version 6.32 or later.]
Use DICT_CHAR_SIZE of 4.


Include (-
Array gg_event --> 4;
Array gg_arguments buffer 28;
Global gg_mainwin = 0;
Global gg_statuswin = 0;
Global gg_quotewin = 0;
Global gg_scriptfref = 0;
Global gg_scriptstr = 0;
Global gg_savestr = 0;
Global gg_commandstr = 0;
Global gg_command_reading = 0;      ! true if gg_commandstr is being replayed
Global gg_foregroundchan = 0;
Global gg_backgroundchan = 0;

Constant INPUT_BUFFER_LEN = 260;    ! No extra byte necessary
Constant MAX_BUFFER_WORDS = 20;
Constant PARSE_BUFFER_LEN = 61;

! The buffer arrays have "table" structure, but we're going to be writing to
! entry zero a lot, so we just use word arrays.
Array  buffer    --> INPUT_BUFFER_LEN+1;
Array  buffer2   --> INPUT_BUFFER_LEN+1;
Array  buffer3   --> INPUT_BUFFER_LEN+1;
Array  parse     --> PARSE_BUFFER_LEN;
Array  parse2    --> PARSE_BUFFER_LEN;
-) instead of "Variables and Arrays" in "Glulx.i6t".


[I am Replacing VM_ReadKeyboard, rather than using a template replacement, because the "Keyboard Input" section has three functions and I only want to touch one of them.]

Include (-
Replace VM_ReadKeyboard;
-) before "Glulx.i6t".

Include (-
! Modified VM_ReadKeyboard to read into a word array, rather than a byte array.

[ VM_ReadKeyboard  a_buffer a_table done ix;
    if (gg_commandstr ~= 0 && gg_command_reading ~= false) {
        done = glk_get_line_stream_uni(gg_commandstr, a_buffer+WORDSIZE,
            (INPUT_BUFFER_LEN-1)-1);
        if (done == 0) {
            glk_stream_close(gg_commandstr, 0);
            gg_commandstr = 0;
            gg_command_reading = false;
            ! L__M(##CommandsRead, 5); would come after prompt
            ! fall through to normal user input.
        }
        else {
            ! Trim the trailing newline
            if ((a_buffer+WORDSIZE)-->(done-1) == 10) done = done-1;
            a_buffer-->0 = done;
            VM_Style(INPUT_VMSTY);
            glk_put_buffer_uni(a_buffer+WORDSIZE, done);
            VM_Style(NORMAL_VMSTY);
            print "^";
            jump KPContinue;
        }
    }
    done = false;
    glk_request_line_event_uni(gg_mainwin, a_buffer+WORDSIZE, INPUT_BUFFER_LEN-1, 0);
    while (~~done) {
        glk_select(gg_event);
        switch (gg_event-->0) {
          5: ! evtype_Arrange
            DrawStatusLine();
          3: ! evtype_LineInput
            if (gg_event-->1 == gg_mainwin) {
                a_buffer-->0 = gg_event-->2;
                done = true;
            }
        }
        ix = HandleGlkEvent(gg_event, 0, a_buffer);
        if (ix == 2) done = true;
        else if (ix == -1) done = false;
    }
    if (gg_commandstr ~= 0 && gg_command_reading == false) {
        glk_put_buffer_stream(gg_commandstr, a_buffer+WORDSIZE, a_buffer-->0);
        glk_put_char_stream(gg_commandstr, 10); ! newline
    }
  .KPContinue;
    VM_Tokenise(a_buffer,a_table);
    ! It's time to close any quote window we've got going.
    if (gg_quotewin) {
        glk_window_close(gg_quotewin, 0);
        gg_quotewin = 0;
    }
    #ifdef ECHO_COMMANDS;
    print "** ";
    for (ix=0: ix<(a_buffer-->0): ix++) print (char) a_buffer-->(1+ix);
    print "^";
    #endif; ! ECHO_COMMANDS
];

-) after "Keyboard Input" in "Glulx.i6t".


Include (-
[ VM_CopyBuffer bto bfrom i;
    for (i=0: i<INPUT_BUFFER_LEN: i++) bto-->i = bfrom-->i;
];

! Prints an object (string, function, etc) to a buffer whose format is like
! the input buffer: a word array whose zeroth entry is the length (in
! characters). Do not use this with a byte array.
[ VM_PrintToBuffer buf len a b c;
    if (b) {
        if (metaclass(a) == Object && a.#b == WORDSIZE
            && metaclass(a.b) == String)
            buf-->0 = Glulx_PrintAnyToArrayUni(buf+WORDSIZE, len, a.b);
        else if (metaclass(a) == Routine)
            buf-->0 = Glulx_PrintAnyToArrayUni(buf+WORDSIZE, len, a, b, c);
        else
            buf-->0 = Glulx_PrintAnyToArrayUni(buf+WORDSIZE, len, a, b);
    }
    else if (metaclass(a) == Routine)
        buf-->0 = Glulx_PrintAnyToArrayUni(buf+WORDSIZE, len, a, b, c);
    else
        buf-->0 = Glulx_PrintAnyToArrayUni(buf+WORDSIZE, len, a);
    if (buf-->0 > len) buf-->0 = len;
    return buf-->0;
];

Constant LOWERCASE_BUF_SIZE = 2*DICT_WORD_SIZE;
Array gg_lowercasebuf --> LOWERCASE_BUF_SIZE;

[ VM_Tokenise buf tab
    cx numwords len bx ix wx wpos wlen val res dictlen ch bytesperword uninormavail;
    len = buf-->0;
    buf = buf+WORDSIZE;

    ! First, split the buffer up into words. We use the standard Infocom
    ! list of word separators (comma, period, double-quote).

    cx = 0;
    numwords = 0;
    while (cx < len) {
        while (cx < len && buf-->cx == ' ') cx++;
        if (cx >= len) break;
        bx = cx;
        if (buf-->cx == '.' or ',' or '"') cx++;
        else {
            while (cx < len && buf-->cx ~= ' ' or '.' or ',' or '"') cx++;
        }
        tab-->(numwords*3+2) = (cx-bx);
        tab-->(numwords*3+3) = 1+bx;
        numwords++;
        if (numwords >= MAX_BUFFER_WORDS) break;
    }
    tab-->0 = numwords;

    ! Now we look each word up in the dictionary.

    dictlen = #dictionary_table-->0;
    bytesperword = DICT_WORD_SIZE * DICT_CHAR_SIZE;
    uninormavail = glk($0004, 16, 0);

    for (wx=0 : wx<numwords : wx++) {
        wlen = tab-->(wx*3+2);
        wpos = tab-->(wx*3+3);

        ! Copy the word into the gg_tokenbuf array, clipping to DICT_WORD_SIZE
        ! characters and lower case. We'll do this in two steps, because
        ! lowercasing might (theoretically) condense characters and allow more
        ! to fit into gg_tokenbuf.
        if (wlen > LOWERCASE_BUF_SIZE) wlen = LOWERCASE_BUF_SIZE;
        cx = wpos - 1;
        for (ix=0 : ix<wlen : ix++) {
            ch = buf-->(cx+ix);
            gg_lowercasebuf-->ix = ch;
        }
        wlen = glk_buffer_to_lower_case_uni(gg_lowercasebuf, LOWERCASE_BUF_SIZE, wlen);
        if (uninormavail) {
            ! Also normalize the Unicode -- combine accent marks with letters
            ! where possible.
            wlen = glk($0124, gg_lowercasebuf, LOWERCASE_BUF_SIZE, wlen); ! buffer_canon_normalize_uni
        }
        if (wlen > DICT_WORD_SIZE) wlen = DICT_WORD_SIZE;
        for (ix=0 : ix<wlen : ix++) {
            gg_tokenbuf-->ix = gg_lowercasebuf-->ix;
        }
        for (: ix<DICT_WORD_SIZE : ix++) gg_tokenbuf-->ix = 0;

        val = #dictionary_table + WORDSIZE;
        @binarysearch gg_tokenbuf bytesperword val DICT_ENTRY_BYTES dictlen 4 1 res;
        tab-->(wx*3+1) = res;
    }
];

! Insert a character into the (global) buffer array.
! (See DM4 appendix A3.)
[ LTI_Insert i ch  b y;

    ! In the original code, buffer was a funny array type. Now it isn't,
    ! but I am minimizing code changes, so we'll keep this alias.
    b = buffer;

    ! Insert character ch into buffer at point i.
    ! Being careful not to let the buffer possibly overflow:
    y = b-->0;
    if (y > INPUT_BUFFER_LEN) y = INPUT_BUFFER_LEN;

    ! Move the subsequent text along one character:
    for ( y=y+1 : y>i : y-- ) b-->y = b-->(y-1);
    b-->i = ch;

    ! And the text is now one character longer:
    if (b-->0 < INPUT_BUFFER_LEN) (b-->0)++;
];
-) instead of "Buffer Functions" in "Glulx.i6t".


Include (-

[ VM_InvalidDictionaryAddress addr;
    if (addr < 0) rtrue;
    rfalse;
];

[ VM_DictionaryAddressToNumber w; return w; ];
[ VM_NumberToDictionaryAddress n; return n; ];

Array gg_tokenbuf --> DICT_WORD_SIZE;

[ GGWordCompare str1 str2 ix jx;
    for (ix=0 : ix<DICT_WORD_SIZE : ix++) {
        jx = (str1-->ix) - (str2-->ix);
        if (jx ~= 0) return jx;
    }
    return 0;
];

-) instead of "Dictionary Functions" in "Glulx.i6t".

Include (-

! Like Glulx_PrintAnyToArray, but it writes to a word array. Returns
! the number of characters printed. (If the text printed is longer than
! the array, the extra characters are safely dropped rather than overflowing
! the array; they are still counted in the returned count.)
[ Glulx_PrintAnyToArrayUni _vararg_count arr arrlen str oldstr len;
    @copy sp arr;
    @copy sp arrlen;
    _vararg_count = _vararg_count - 2;

    oldstr = glk_stream_get_current();
    str = glk_stream_open_memory_uni(arr, arrlen, 1, 0);
    if (str == 0) return 0;

    glk_stream_set_current(str);

    @call Glulx_PrintAnything _vararg_count 0;

    glk_stream_set_current(oldstr);
    @copy $ffffffff sp;
    @copy str sp;
    @glk $0044 2 0; ! stream_close
    @copy sp len;
    @copy sp 0;
    return len;
];

-) after "Glulx-Only Printing Routines" in "Glulx.i6t".

Include (-

Constant SHORT_NAME_BUFFER_LEN = 250;
Array StorageForShortName --> SHORT_NAME_BUFFER_LEN;

! Replacement for the CPrintOrRun routine, using modern printing commands.
! This is a Glulx-only implementation, but then this whole extension is
! Glulx-only.
[ CPrintOrRun obj prop  v length;
    if ((obj ofclass String or Routine) || (prop == 0))
        length = Glulx_PrintAnyToArrayUni(StorageForShortName, SHORT_NAME_BUFFER_LEN, obj);
    else {
    	if (obj.prop == NULL) rfalse;
        if (metaclass(obj.prop) == Routine or String)
            length = Glulx_PrintAnyToArrayUni(StorageForShortName, SHORT_NAME_BUFFER_LEN, obj.prop);
        else return RunTimeError(2, obj, prop);
	}
	
	! Perhaps the name contained more than 250 characters. If so, it was
	! truncated (safely) to the array length.
	if (length > SHORT_NAME_BUFFER_LEN) length = SHORT_NAME_BUFFER_LEN;
	
	! This is the best way to print text with the first character capitalized:
	!   length = glk_buffer_to_title_case_uni(StorageForShortName, SHORT_NAME_BUFFER_LEN, length, false);
	!   glk_put_buffer_uni(StorageForShortName, length);
	
	! However, that crashes on the Mac IDE (6G60), apparently due to a Zoom
	! bug. So we do it the old-fashioned way. Hopefully a future version can
	! be made Unicode-aware.
	if (length)
		StorageForShortName-->0 = VM_LowerToUpperCase(StorageForShortName-->0);
	glk_put_buffer_uni(StorageForShortName, length);

    if (length) say__p = 1;

    return;
];

[ Cap str nocaps;
    if (nocaps) print (string) str;
    else CPrintOrRun(str, 0);
];

-) instead of "Short Name Storage" in "Printing.i6t".

Unicode Parser ends here.


---- DOCUMENTATION ----

When you include this extension, I7 will appear to behave as it always does. However, the command line will be read using a Unicode-friendly input call, and the internal parsing dictionary will contain Unicode strings instead of byte strings. This means that, theoretically, you can define nouns and verbs using any Unicode character (not just basic Latin-1.)

However, the I7 language does not currently permit this. So we have to indulge in some trickery to make these definitions possible.

Section: Unicode synonyms for verbs

To define a verb synonym with Unicode characters:

	Include
	(- Verb '@{3C0}@{3B1}@{3AF}@{3C1}@{3BD}@{3C9}' = 'get';
	Verb '@{3C0}@{3B1}@{3B9}@{3C1}@{3BD}@{3C9}' = 'get';
	Verb '@{11D}et' = 'get'; -)
	after "Grammar" in "Output.i6t".

The strings here are single-quoted strings of characters defined with the I6 '@{hexadecimal}' format. The first line is the Greek word "παίρνω". (I apologize for butchering Greek here -- all my translation is due to Google!) With this definition, the command "παίρνω lamp" will work. So will "Παίρνω Lamp"; as usual, commands are converted to lower case where possible.

The second line is the same word, but without the accent mark. The dictionary considers accents significant while matching, so if you want to accept the verb "παιρνω" (or "Παιρνω") you need this line. (Again, I don't know if a Greek speaker would leave off the accent mark! Probably not.)

The third line defines the verb "ĝet" in the same way. This is by way of demonstrating normalization. The Unicode standard permits two ways to define this string: "ĝet" and "ĝet". These probably look the same to you, but they're not. The former is three characters long, as you might expect; it starts with the Unicode character named LATIN SMALL LETTER G WITH CIRCUMFLEX. (Unicode loves these verbose names.) The second example is *four* characters long; the first character is LATIN SMALL LETTER G, and the second is COMBINING CIRCUMFLEX ACCENT. The "^" stacks on top of the "g" when the pair is displayed.

The combined form is more common, but a player might type either form. Therefore, this extension *tries* to accept both, by "normalizing" the input words. However, the Glk normalization function is relatively new, and may not be available. The Mac Inform IDE 6G60 lacks this call, for example. So the four-character form will not be recognized by the verb definition shown above. To accept it, we'd need an additional line:

	Include (- Verb 'g@{302}et' = 'get'; -)
	after "Grammar" in "Output.i6t".

You can also define an entire verb line (with prepositions and everything), using the I6 syntax:

	Include (- Verb '@{11D}et' * 'i@{3B7}' noun -> Enter; -)
	after "Grammar" in "Output.i6t".

(Accepts the command "ĝet iη boat".)

However, it is currently not possible to refer to a custom action this way -- only to the predefined ones.


Section: Unicode synonyms for nouns

This is also ugly. To define a synonym for an object, we have to define an I6 class:

	Include (- Class rock_name_class
	with name '@{3B2}@{3C1}@{3AC}@{3C7}@{3BF}@{3C2}' '@{3B2}@{3C1}@{3AC}@{3C7}@{3BF}@{3C3}'; -)
	before "Object Tree" in "Output.i6t".
	
	The rock is a thing.
	Include (- class rock_name_class -) when defining the rock.

The rock_name_class class acts as a mix-in which adds the strings "βράχος" and "βράχοσ" to the rock. (I'm including the two variations on the final letter sigma.) We can now accept the command "παίρνω βράχος" (or "Παίρνω Βράχος").


Section: Details for the I6 hacker

This extension modifies Inform's internal command buffers to be Unicode arrays (arrays of 32-bit integers) rather than plain character arrays (arrays of 8-bit characters). These are the "buffer", "buffer2", and "buffer3" arrays.

We also update the parser functions that manage these arrays: VM_ReadKeyboard(), VM_CopyBuffer(), VM_PrintToBuffer(), VM_Tokenise(), LTI_Insert(), GGWordCompare(). We add a Glulx_PrintAnyToArrayUni() function, which prints to a Unicode array.

Warning: Any extension that uses I6 code to manipulate the command buffer directly will break when used with this extension!


Section: Caveats

This extension is extremely untested! Things which probably don't work:

- Disambiguation
- Replacing snippets in the player's command
- Writing and reading command-history files
- All the internal uses of CPrintOrRun() which I think I broke


Example: ** Ungrammatical Greek - Defining verb and noun synonyms containing Unicode characters.

"Ungrammatical Greek" by Andrew Plotkin.

Include Unicode Parser by Andrew Plotkin.

The Kitchen is a room. The description is "A sign reads: Test me with 'παίρνω βράχος'!"

The lamp is in the Kitchen. The rock is in the Kitchen.

Include (- Class rock_name_class
with name '@{3B2}@{3C1}@{3AC}@{3C7}@{3BF}@{3C2}' '@{3B2}@{3C1}@{3AC}@{3C7}@{3BF}@{3C3}'; -)
before "Object Tree" in "Output.i6t".

Include (- class rock_name_class -) when defining the rock.

Include (- Verb '@{3C0}@{3B1}@{3AF}@{3C1}@{3BD}@{3C9}' '@{3C0}@{3B1}@{3B9}@{3C1}@{3BD}@{3C9}' = 'get'; -)
after "Grammar" in "Output.i6t".


