Version 1 of Unicode Parser (for Glulx only) by Andrew Plotkin begins here.

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

! The buffer arrays have "table" structure, but we're going to be writing to entry zero a lot, so we just use word arrays.
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
    cx numwords len bx ix wx wpos wlen val res dictlen ch bytesperword;
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

    for (wx=0 : wx<numwords : wx++) {
        wlen = tab-->(wx*3+2);
        wpos = tab-->(wx*3+3);

        ! Copy the word into the gg_tokenbuf array, clipping to DICT_WORD_SIZE characters and lower case. We'll do this in two steps, because lowercasing might (theoretically) condense characters and allow more to fit into gg_tokenbuf.
        ! ### normalize as well!
        if (wlen > LOWERCASE_BUF_SIZE) wlen = LOWERCASE_BUF_SIZE;
        cx = wpos - 1;
        for (ix=0 : ix<wlen : ix++) {
            ch = buf-->(cx+ix);
            gg_lowercasebuf-->ix = ch;
        }
        wlen = glk($0120, gg_lowercasebuf, LOWERCASE_BUF_SIZE, wlen); ! buffer_to_lower_case_uni
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

[ LTI_Insert i ch  b y;

    ! In the original code, buffer was a funny array type. Now it isn't, but I am minimizing code changes, so we'll keep this alias.
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

Unicode Parser ends here.


---- DOCUMENTATION ----

This modifies Inform's internal parse buffers to be Unicode arrays (arrays of 32-bit integers) rather than plain character arrays (arrays of 8-bit characters). It also updates the functions that manage these arrays.

