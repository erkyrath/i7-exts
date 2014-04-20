Version 1 of Large Game Speedup by Andrew Plotkin begins here.

Use authorial modesty.

Chapter - Miscellaneous Definitions

[These adjectives are much faster than "if nothing is in..." or "if the number of things in ... is zero"]
Definition: a container is empty rather than non-empty if the first thing held by it is nothing.
Definition: a supporter is empty rather than non-empty if the first thing held by it is nothing.

Chapter - Table of Locale Priorities

Include (-
[ TableSortPartial tab rows col dir test_flag algorithm i j k f;
	for (i=1:i<=tab-->0:i++) {
		j = tab-->i; ! Address of column table
		if ((j-->1) & TB_COLUMN_DONTSORTME)
			return RunTimeProblem(RTP_TABLE_CANTSORT, tab);
	}
	if (col >= 100) col=TableFindCol(tab, col, false);
	k = rows; ! Not the entire table
	k = TableMoveBlanksToBack(tab, 1, k);
	if (test_flag) {
		print "After moving blanks to back:^"; TableColumnDebug(tab, col);
	}

	SetSortDomain(TableSwapRows, TableCompareRows);
	SortArray(tab, col, dir, k, test_flag, algorithm);

	if (test_flag) {
		print "Final state:^"; TableColumnDebug(tab, col);
	}
];
-) after "Sort" in "Tables.i6t".

To sort (T - table name) up to row (N - number) in (TC - table column) order
        (documented at ph_sortcolumn):
        (- TableSortPartial({T}, {N}, {TC}, 1); -).

[We never want to search (or sort) through the entire table, so we manually keep track of the number of "live" rows.]
The locale-table-count is a number that varies.

To set the/-- locale priority of (O - an object) to (N - a number):
	if O is a thing:
		[say "### setting priority of [shortname O] to [N]..."; [###]]
		if N <= 0:
			now O is mentioned;
		[search the active part of the table for O; also note the first null row]
		let rownum be 0;
		let blanknum be 0;
		repeat with I running from 1 to locale-table-count:
			let ent be the notable-object in row I of  the Table of Locale Priorities;
			if ent is nothing:
				if blanknum is 0:
					now blanknum is I;
				next;
			if ent is O:
				now rownum is I;
				break;
		if rownum > 0: [found it]
			if N > 0:
				[change the existing row]
				choose row rownum in the Table of Locale Priorities;
				now the locale description priority entry is N;
			else:
				[delete the existing row, by putting in "nothing".]
				choose row rownum in the Table of Locale Priorities;
				now the notable-object entry is nothing;
				now the locale description priority entry is 999;
		otherwise: [didn't find it]
			if N > 0:
				if blanknum is 0:
					[add a new row]
					increment locale-table-count;
					choose row locale-table-count in the Table of Locale Priorities;
					now the notable-object entry is O;
					now the locale description priority entry is N;
				else:
					[use the null row]
					choose row blanknum in the Table of Locale Priorities;
					now the notable-object entry is O;
					now the locale description priority entry is N;
		[dump the Table of Locale Priorities; [###]]

The optimized initialise locale description rule is listed instead of the initialise locale description rule in the before printing the locale description rulebook.
This is the optimized initialise locale description rule:
	now the locale paragraph count is 0;
	now all things are not mentioned; [loops through all things]
	[Mark the table as empty (without blanking every single row)]
	now the locale-table-count is zero.

The optimized interesting locale paragraphs rule is listed instead of the interesting locale paragraphs rule in the for printing the locale description rulebook.
This is the optimized interesting locale paragraphs rule:
	let the domain be the parameter-object;
	sort the Table of Locale Priorities up to row locale-table-count in locale description priority order;
	[say "### Sorted:[line break]";
	dump the Table of Locale Priorities; [###]]
	repeat with I running from 1 to locale-table-count:
		let O be the notable-object in row I of  the Table of Locale Priorities;
		if O is not nothing:
			carry out the printing a locale paragraph about activity with O;
	continue the activity.

The optimized you-can-also-see rule is listed instead of the you-can-also-see rule in the for printing the locale description rulebook.
This is the optimized you-can-also-see rule:
	let the domain be the parameter-object;
	let the mentionable count be 0;
	let the marked count be 0;
	now all things are not marked for listing;  [loops through all things]
	repeat with I running from 1 to locale-table-count:
		let O be the notable-object in row I of the Table of Locale Priorities;
		if O is not nothing:
			let N be the locale description priority in row I of the Table of Locale Priorities;
			[say "[O] - [N].";]
			if N is greater than 0 and O is not mentioned:
				now O is marked for listing;
				increment the marked count;
			increase the mentionable count by 1;
	if the mentionable count is greater than 0:
		[note that mentioned things have not been marked for listing]
		begin the listing nondescript items activity with the domain;
		if the marked count is 0:
			abandon the listing nondescript items activity with the domain;
		otherwise:
			if handling the listing nondescript items activity:
				if the domain is a room:
					if the domain is the location, say "You ";
					otherwise say "In [the domain] you ";
				otherwise if the domain is a supporter:
					say "On [the domain] you ";
				otherwise if the domain is an animal:
					say "On [the domain] you ";
				otherwise:
					say "In [the domain] you ";
				say "can [if the locale paragraph count is greater than 0]also [end if]see ";
				let the common holder be nothing;
				let contents form of list be true;
				repeat with I running from 1 to locale-table-count:
					let list item be the notable-object in row I of the Table of Locale Priorities;
					if list item is nothing or list item is not marked for listing:
						next;
					if the holder of the list item is not the common holder:
						if the common holder is nothing,
							now the common holder is the holder of the list item;
						otherwise now contents form of list is false;
					if the list item is mentioned, now the list item is not marked for listing;
				filter list recursion to unmentioned things;
				if contents form of list is true and the common holder is not nothing,
					list the contents of the common holder, as a sentence, including contents,
						giving brief inventory information, tersely, not listing
						concealed items, listing marked items only;
				otherwise say "[a list of marked for listing things including contents]"; [loops through all things]
				if the domain is the location, say " here";
				say ".[paragraph break]";
				unfilter list recursion;
			end the listing nondescript items activity with the domain;
	continue the activity.


To say shortname (O - object): (- @push parameter_object; parameter_object = {O}; STANDARD_NAME_PRINTING_R(); @pull parameter_object; -).

To dump the Table of Locale Priorities:
	[say "### locale-table-count is [locale-table-count][line break]";]
	repeat with I running from 1 to locale-table-count:
		choose row I in the Table of Locale Priorities;
		say "### ... [shortname notable-object entry] : [locale description priority entry][line break]";



Chapter - Faster Listing Phrases

Include (-
[ WriteListOfMarkedContentsObjects style common_parent
	obj first length;

	objectloop (obj in common_parent && obj has workflag2) {
		length++;
		if (first == nothing) { first = obj; }
	}

	if (length == 0) {
    	if (style & ISARE_BIT ~= 0) print (string) IS3__TX, " ", (string) NOTHING__TX;
    	else if (style & CFIRSTART_BIT ~= 0) print (string) NOTHING2__TX;
		else print (string) NOTHING__TX;
	} else {
		@push MarkedObjectArray; @push MarkedObjectLength;
		MarkedObjectArray = RequisitionStack(length);
		MarkedObjectLength = length;
		if (MarkedObjectArray == 0) return RunTimeProblem(RTP_LISTWRITERMEMORY); 

		! common_parent is always set
		ObjectTreeCoalesce(child(common_parent));
		length = 0;
		objectloop (obj in common_parent) ! object tree order
			if (obj has workflag2) MarkedObjectArray-->length++ = obj;

		WriteListFrom(first, style, 0, false, MarkedListIterator);

		FreeStack(MarkedObjectArray);
		@pull MarkedObjectLength; @pull MarkedObjectArray;
	}
	return;
];
-) after "WriteListOfMarkedObjects" in "ListWriter.i6t".

To say a list of (OS - description of objects) *in (parent - object):
	(- @push subst__v;
		objectloop (subst__v in {parent}) if ({-bind-variable:OS})
		give subst__v workflag2; else give subst__v ~workflag2;
		WriteListOfMarkedContentsObjects(ENGLISH_BIT, {parent});
		@pull subst__v; -).
To say A list of (OS - description of objects) *in (parent - object):
	(- @push subst__v;
		objectloop (subst__v in {parent}) if ({-bind-variable:OS})
		give subst__v workflag2; else give subst__v ~workflag2;
		WriteListOfMarkedContentsObjects(ENGLISH_BIT+CFIRSTART_BIT, {parent});
		@pull subst__v; -).
To say list of (OS - description of objects) *in (parent - object):
	(- @push subst__v;
		objectloop (subst__v in {parent}) if ({-bind-variable:OS})
		give subst__v workflag2; else give subst__v ~workflag2;
		WriteListOfMarkedContentsObjects(ENGLISH_BIT+NOARTICLE_BIT, {parent});
		@pull subst__v; -).
To say the list of (OS - description of objects) *in (parent - object):
	(- @push subst__v;
		objectloop (subst__v in {parent}) if ({-bind-variable:OS})
		give subst__v workflag2; else give subst__v ~workflag2;
		WriteListOfMarkedContentsObjects(ENGLISH_BIT+DEFART_BIT, {parent});
		@pull subst__v; -).
To say The list of (OS - description of objects) *in (parent - object):
	(- @push subst__v;
		objectloop (subst__v in {parent}) if ({-bind-variable:OS})
		give subst__v workflag2; else give subst__v ~workflag2;
		WriteListOfMarkedContentsObjects(ENGLISH_BIT+DEFART_BIT+CFIRSTART_BIT, {parent});
		@pull subst__v; -).
To say is-are a list of (OS - description of objects) *in (parent - object):
	(- @push subst__v;
		objectloop (subst__v in {parent}) if ({-bind-variable:OS})
		give subst__v workflag2; else give subst__v ~workflag2;
		WriteListOfMarkedContentsObjects(ENGLISH_BIT+ISARE_BIT, {parent});
		@pull subst__v; -).
To say is-are list of (OS - description of objects) *in (parent - object):
	(- @push subst__v;
		objectloop (subst__v in {parent}) if ({-bind-variable:OS})
		give subst__v workflag2; else give subst__v ~workflag2;
		WriteListOfMarkedContentsObjects(ENGLISH_BIT+ISARE_BIT+NOARTICLE_BIT, {parent});
		@pull subst__v; -).
To say is-are the list of (OS - description of objects) *in (parent - object):
	(- @push subst__v;
		objectloop (subst__v in {parent}) if ({-bind-variable:OS})
		give subst__v workflag2; else give subst__v ~workflag2;
		WriteListOfMarkedContentsObjects(ENGLISH_BIT+DEFART_BIT+ISARE_BIT, {parent});
		@pull subst__v; -).


Large Game Speedup ends here.
