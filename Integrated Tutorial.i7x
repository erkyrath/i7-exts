Version 1 of Integrated Tutorial by Andrew Plotkin begins here.

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

A tutorial-prompt is a kind of object.
A tutorial-prompt has a text called the description.
A tutorial-prompt has a number called the tutorial-priority. The tutorial-priority of a tutorial-prompt is usually 2.
A tutorial-prompt has a number called the display-count.
A tutorial-prompt can be complete.
A tutorial-prompt can be active or inactive. A tutorial-prompt is usually active.

Prompt-displaying is an object-based rulebook.

Last prompt-displaying rule for a tutorial-prompt (called TP):
	if the description of TP is not empty:
		say "[tut-style][description of TP][/tut-style]";
	else:
		say "[tut-style](BUG) [TP] has no description.[run paragraph on][/tut-style]";

Selecting tutorial target is a rulebook. [### returning an object... action-based? can we do that off a stored action?]

Table of Tutorial Prompts
prompt (tutorial-prompt)	priority (number)
with 6 blank rows

When play begins:
	let N be the number of blank rows in the Table of Tutorial Prompts;
	let M be the number of tutorial-prompts;
	if M > N:
		say "(BUG) You have more tutorial prompts defined than can be listed in the internal table. Extend the table with a declaration like:[paragraph break]Table of Tutorial Prompts (continued)[line break]with [M - N] blank rows[paragraph break]";
		continue the action;
	now N is 1;
	repeat with TP running through tutorial-prompts:
		if TP is TP-null:
			next;
		choose row N from the Table of Tutorial Prompts;
		now the prompt entry is TP;
		now the priority entry is the tutorial-priority of TP;
		increment N;
	sort the Table of Tutorial Prompts in priority order;
	prepare the next tutorial-prompt;

Current-tutorial-prompt is a tutorial-prompt that varies.
Cued-tutorial-prompts is a list of tutorial-prompts that varies.

To prepare the next tutorial-prompt:
	now current-tutorial-prompt is TP-null;
	repeat through the Table of Tutorial Prompts:
		let TP be the prompt entry;
		if TP is inactive or TP is complete:
			next;
		now current-tutorial-prompt is TP;
		break;
	if current-tutorial-prompt is TP-null:
		say "### end of tutorial.";
		stop;
	cue current-tutorial-prompt;

To cue (TP - tutorial-prompt):
	add TP to cued-tutorial-prompts.

To mark (TP - tutorial-prompt) complete:
	if TP is not complete:
		now TP is complete;
		if TP is the current-tutorial-prompt:
			prepare the next tutorial-prompt;

Ever-read-command is initially false;

Before reading a command:
	now ever-read-command is true;
	if cued-tutorial-prompts is not empty:
		repeat with TP running through cued-tutorial-prompts:
			follow the prompt-displaying rules for TP;
	now cued-tutorial-prompts is {};

TP-null is an inactive tutorial-prompt.
The description is "(This prompt should never appear.)"

TP-look is a tutorial-prompt.
The description is "You interact with this game by typing commands. Type [cmd-style]LOOK[/cmd-style] to repeat the description of what you see."

After looking when ever-read-command is true:
	mark TP-look complete;

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

#Table of Tutorial Prompts (continued)
#with 5 blank rows

