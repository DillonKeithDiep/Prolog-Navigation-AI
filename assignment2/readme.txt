
-------------------------
- Setting up
-------------------------

1. You need the latest version of this ailp zip file.

2. You need a fairly up to date version of swi-prolog.

3. You need to be connected to the internet.

-------------------------
- Running assignment2 (a.k.a. oscar)
-------------------------

1. Change directory to the folder containing this readme.txt

2. Run the following command (no command line argument needed):

swipl -s ailp.pl

3. 

* To start the web server type command 'start.'

This will give you an option to open in a browser. Press Enter or type 'Y'. 

* To start with a fresh board type 'ailp_reset.'

* To stop the web server type command 'stop.'

* Don't forget to use 'make.'  and 'ailp_reset.' when you have made changes to the code.

////////////////////////////////////////////////////////////

EXTRA TIPS:
here are some useful things that could help ourselves:
UPDATING CODE:
	You don't need ailp_reset, if you're in shell enter '.' to exit with ERROR: Stream blabla
	Then do 'make.'
	Open 'shell.' and 'reset.', changes in oscar.pl are now applied
LISTS:
	c = [a|b] means c is a list, with head A and tail B
	lists can have different types, e.g.
	c = [a(P,F)|b] means a list of c = [a(P,F), b]
	be careful with recursive lists, so
	b = [1,2]
	c = [0, b]
	it is NOT [0,1,2] but instead [0,[1,2]]
	good shit

DIFFERENCE BETWEEN GO AND FIND
	go(p(7,7)) location is known
	find(o(1)) location is unknown