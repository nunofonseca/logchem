Last Updated: November 10, 2006

What is t-unit?
t-unit is a package that was developed for the tcl language.  The t-unit package provides an automated unit testing framework modeled after the "JUnit" Java test suite.  All calls are "asserts"; such as "assertTrue" checks for result boolean TRUE.  

Features:
- assertEquals
- assertEqualsArray
- assertEqualsList
- assertEqualsReal
- assertFalse
- assertNotEquals
- assertTrue
- unitTestHarness

Related Sites:
- TUnit Homepage (http://tunitplugin.googlepages.com/)
- TUnit Repository (http://code.google.com/p/t-unit/)
- TUnit Blog (http://callmeduder.blogspot.com/)

Using t-unit:
A t-unit script is organized as follows: Any line that starts with a "hash" symbol (one or more) is considered a comment and will not be executed as a test.  Comments MUST take the entire line; no "in-line" comment syntax is provided.  All other lines are considered to be commands.  

Command lines are structured as follows.  Everything up to the first occurrance of the vertical bar symbol ("|" or "pipe" as it is known in UNIX/Solaris), is considered a Tcl command which may include arguments to that command.  There should be two, and only two, occurrances of the pipe.  Everything between the two pipes is considered to be the expected result of the execution of that line's command.  Everything occurring after the second occurrance of the pipe is considered to be the result returned by the command; this can include either "OK", or some other phrase indicating an expected "forced error condition" which occurs when the command's error handling functionality is being tested.

Note that blank lines are considered comments.

Note also that the "PASS" or "FAIL" is with respect to the t-unit procedure result, NOT to the TESTED procedure result.  This can be confusing at first but it all works out when you see the test results on the stdout display.  

Note that string variables containing spaces must be surrounded by double quotes AND BY CURLY BRACES to make the Tcl interpreter treat them as a single string entity.  Otherwise, the string will break at the first space, causing an unexpected result.  An annotated example is as follows, with ">" indicating the file margin:

>## This is a comment line
>## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ another comment
>
>## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
>##  This is an example of a block comment
>## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
>  t-unit::assertEquals 12.34 12.34       |  PASS  |     OK
>  t-unit::assertFalse  TRUE              |  FAIL  |     NOT_FALSE

Credits:
Thanks to BJ Johnson for the motivation and help in the design and implementation of TUnit.  Without your idea, none of this would have been possible.  Special thanks to Mark James for the icons used in the plugin.  Visit http://www.famfamfam.com for more information.  

Copyright Notice:
(c) Copyright 2006 Loyola Marymount University Computer Science Department.

License:
This program is covered under the Lesser GNU Public License. For details on this license, please refer to: http://www.gnu.org/licenses/lgpl.html

Version History:
1.0.3
Changes:
- Added a one second delay to the unit test harness