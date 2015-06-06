Version 1 of Integrated Tutorial by Andrew Plotkin begins here.

To decide what number is the currently chosen row: (- ({-my:ct_1}) -).

Current-tut-style is initially false.

To say tut-style:
	say italic type;
	say bracket;
	now current-tut-style is true;

To say /tut-style:
	now current-tut-style is false;
	say close bracket;
	say roman type;
	say line break;

To say cmd-style:
	say fixed letter spacing;

To say /cmd-style:
	say variable letter spacing;
	if current-tut-style is true:
		say italic type;

Table of Tutorial Prompts
prompt	complete (truth state)	order (number)
"You interact with this game by typing commands. Type [cmd-style]LOOK[/cmd-style] to repeat the description of what you see."	--	1
"You've got the hang of it! This is the end of the tutorial. Remember, if you're not sure what to try, [cmd-style]LOOK[/cmd-style] around and try to [cmd-style]EXAMINE[/cmd-style] whatever you see."	--	9

The active prompt row is initially 0.

When play begins:
	repeat through the Table of Tutorial Prompts:
		if there is no order entry:
			now the order entry is 2;
		if the order entry is 0:
			blank out the whole row;
	sort the Table of Tutorial Prompts in order order;
	cue the next prompt;

To cue the next prompt:
	repeat through the Table of Tutorial Prompts:
		if there is a complete entry and the complete entry is true:
			next;
		now the active prompt row is the currently chosen row;
		stop;
	say "### tutorial is complete.";

Before reading a command:
	if the active prompt row is not 0:
		choose row active prompt row in the Table of Tutorial Prompts;
		say "[tut-style][prompt entry][/tut-style]";

Integrated Tutorial ends here.

---- Documentation ----


[Have you played interactive fiction before?]
> NO

[I'll give you some help, then.]

[Hit any key to begin the game.]

... if YES: [Great! On with the show.] (And don't wait for a key.)

[You interact with this game by typing commands. Type LOOK to repeat the description of what you see.]

> LOOK

# on looking...
[Use the EXAMINE command to take a closer look at something. Try EXAMINE LAMP now (or just X LAMP).]

# on any successful examine...
[You can pick things up and carry them around. Try TAKE LAMP now.]

# on unsuccessful take...
[Not everything can be picked up, obviously. Try TAKE LAMP now.]

# on successful take...
[To check what you're carrying, type INVENTORY (or just I).]

# on inventory...
[You generally move around by typing compass directions. You can go north from here; try GO NORTH (or just N).]

# on move...
[You've got the hang of it! This is the end of the tutorial. Remember, if you're not sure what to try, LOOK around and try to EXAMINE whatever you see.]

### if the player jumps ahead, silently mark rules as done

> DFGFGDG
That's not a verb I recognise.
# repeat last prompt (if tutorial active)
# same response from hitting ENTER a bunch of times

# during-tutorial failure messages for particular actions
# cue up a particular prompt, out-of-band? from EXAMINE LAMP to X LAMP
# counters for each prompt
# "help" repeats a prompt (but don't rely on this, games may strike it)
# self-completing prompts, cue up next thing next turn

