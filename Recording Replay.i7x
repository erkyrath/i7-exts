Version 1 of Recording Replay (for Glulx only) by Andrew Plotkin begins here.

[Recording will never become active in release mode.]
To decide whether the/-- command stream is inactive: (- (gg_commandstr==0) -).
To decide whether the/-- command stream is recording: (- (gg_commandstr && ~~gg_command_reading) -).
To decide whether the/-- command stream is replaying: (- (gg_commandstr && gg_command_reading) -).

Volume - not for release

Recording-toggle is an action out of world applying to nothing.
Recording-on is an action out of world applying to nothing.
Recording-off is an action out of world applying to nothing.
Recording-read is an action out of world applying to nothing.

Understand "recording" as recording-toggle.
Understand "recording on" as recording-on.
Understand "recording off" as recording-off.
Understand "replay" as recording-read.

Carry out recording-toggle:
	if command stream is inactive:
		instead follow the carry out recording-on rulebook;
	else:
		instead follow the carry out recording-off rulebook;

The turn on recording rule is listed in the carry out recording-on rulebook.
The turn off recording rule is listed in the carry out recording-off rulebook.
The start replaying recording rule is listed in the carry out recording-read rulebook.

The turn on recording rule translates into I6 as "RECORDING_ON_R" with
	"[bracket]Command recording on.[close bracket]" (A),
	"[bracket]Commands are currently replaying.[close bracket]" (B),
	"[bracket]Command recording already on.[close bracket]" (C),
	"[bracket]Command recording failed.[close bracket]" (D).

The turn off recording rule translates into I6 as "RECORDING_OFF_R" with
	"[bracket]Command recording off.[close bracket]" (A),
	"[bracket]Command recording already off.[close bracket]" (B),
	"[bracket]Command replay complete.[close bracket]" (C).

The start replaying recording rule translates into I6 as "RECORDING_READ_R" with
	"[bracket]Replaying commands.[close bracket]" (A),
	"[bracket]Commands are already replaying.[close bracket]" (B),
	"[bracket]Command replay failed. Command recording is on.[close bracket]" (C),
	"[bracket]Command replay failed.[close bracket]" (D).


Include (-

#ifndef ResponseAndLineBreak;
[ ResponseAndLineBreak rule letter;
	rule(letter);
	new_line;
];
#endif;

[ RECORDING_ON_R fref;
	if (gg_commandstr ~= 0) {
		if (gg_command_reading)
			return ResponseAndLineBreak(RECORDING_ON_RM, 'B'); ! Commands are currently replaying.
		else
			return ResponseAndLineBreak(RECORDING_ON_RM, 'C'); ! Command recording already on.
	}
	fref = glk_fileref_create_by_prompt($103, $01, 0);
	if (fref == 0)
		return ResponseAndLineBreak(RECORDING_ON_RM, 'D'); ! Command recording failed.
	gg_command_reading = false;
	gg_commandstr = glk_stream_open_file(fref, $01, GG_COMMANDWSTR_ROCK);
	glk_fileref_destroy(fref);
	if (gg_commandstr == 0)
		return ResponseAndLineBreak(RECORDING_ON_RM, 'D'); ! Command recording failed.
	return ResponseAndLineBreak(RECORDING_ON_RM, 'A'); ! Command recording on.
];

[ RECORDING_OFF_R;
	if (gg_commandstr == 0)
		return ResponseAndLineBreak(RECORDING_OFF_RM, 'B'); ! Command recording already off.
	if (gg_command_reading)
		return ResponseAndLineBreak(RECORDING_OFF_RM, 'C'); ! Command replay complete.

	glk_stream_close(gg_commandstr, 0); ! stream_close
	gg_commandstr = 0;
	gg_command_reading = false;
	return ResponseAndLineBreak(RECORDING_OFF_RM, 'A'); ! Command recording off.
];

[ RECORDING_READ_R fref;
	if (gg_commandstr ~= 0) {
		if (gg_command_reading)
			return ResponseAndLineBreak(RECORDING_READ_RM, 'B'); ! Commands are already replaying.
		else
			return ResponseAndLineBreak(RECORDING_READ_RM, 'C'); ! Command replay failed. Command recording is on.
		return;
	}
	fref = glk_fileref_create_by_prompt($103, $02, 0);
	if (fref == 0)
		return ResponseAndLineBreak(RECORDING_READ_RM, 'D'); ! Command replay failed.
	gg_command_reading = true;
	gg_commandstr = glk_stream_open_file(fref, $02, GG_COMMANDRSTR_ROCK);
	glk_fileref_destroy(fref);
	if (gg_commandstr == 0)
		return ResponseAndLineBreak(RECORDING_READ_RM, 'D'); ! Command replay failed.
	return ResponseAndLineBreak(RECORDING_READ_RM, 'A'); ! Replaying commands.
];

-).

Recording Replay ends here.

---- DOCUMENTATION ----

This implements the RECORDING and REPLAY debug verbs which were available in Inform 6.

RECORDING (ON) prompts you to create a file. Commands will be written out here as the player types them. RECORDING OFF stops recording and closes the file.

REPLAY prompts you for a recorded file. The commands in the file will be played back, running through the game.

