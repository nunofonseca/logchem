## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##
##  File Name:  assertEqualsReal.tcl
##  @short      Check if the arguments passed in are indeed equivalent
##  @author     Original author ~ Joe Boyle
##  @date       Original date   ~ 15-Feb-2006
##  @version    1.0   Initial release
##
##  @comment      This module file provides functionality to evaluate two Tcl
##  @c            expressions and decide if the results of the evaluations
##  @c            are 100% equal.  The arguments are both assumed to be
##  @c            evaluable expressions.  To be considered equal, the results
##  @c            must be of equal type and value; i.e., "2" is not considered
##  @c            equal to "2.0".
##
##  @note       See procedure definition header below for more details.
##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##
##  Revision history
##  ----------------
##
##          Rev    Rev. Date   Released by:  Revision Description
##         -----  -----------  ------------  --------------------------------
## version 1.0--  15-Feb-2006  Joe Boyle     Initial version written
##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  NO MODULE GLOBAL VARIABLES
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

proc t-unit::assertEqualsReal { expression1 expression2 range {errorString ""} }  {
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  Proc Name:  assertEqualsReal
##  @short      Check if the arguments passed in are indeed equal to each other
##  @argument   expression1   First expression to evaluate
##  @a          expression2   Second expression to evaluate
##  @a          range         Range within which values are considered equal
##  @a          errorString   Optional string literal specifying error message
##  @result     returns "OK" on success, error string on failure
##  @example    set errVal [t-unit::assertEqualsReal this that 0.001 "Error: Not equal"]
##
##  @comment      This module file provides functionality to evaluate two Tcl
##  @c            expressions and decide if the results of the evaluations
##  @c            compare nearly.  Results must be the same data type as well
##  @c            as the same value, within the bounds of Tcl/Tk semantics.&p&p
##  @c
##  @c            Note that this procedure is not intended to replace the normal
##  @c            comparison of, for example, two integers.  If you want to
##  @c            check if 2 == 2, you'd still do it that way.  This procedure
##  @c            is intended to check two PROCEDURES, or EXPRESSIONS, for
##  @c            equality, as part of a unit test on the procedure(s).  In
##  @c            addition, this procedure is tailored to comparisons for the
##  @c            "REAL" and "DOUBLE" data types.  Representations of these&p&p
##  @c            data types in computers are notoriously difficult when two
##  @c            values are compared; for this reason, in this procedure, the
##  @c            two resulting values are subtracted, and the difference is
##  @c            compared to the range value passed in the third argument.
##  @c            If the difference is greater than the range, the numbers are
##  @c            considered not equal.  An example of this can be seen using
##  @c            the tclsh or wish84 shells, using the following commands:&p&p
##  @c
##  @c                  tclsh> set blah 123.45
##  @c                  tclsh> set woof 123.45001
##  @c                  tclsh> if { $blah == $woof } { puts "equal" }
##  @c                    =>equal
##  @c
##  @c            As shown, the two numbers are considered equal, even when they
##  @c            are obviously not.
##  @c
##  @c            The normal usage of this procedure is to have the first argument
##  @c            be the expected value, with the second argument containing the
##  @c            name of the procedure to test.  The third argument is expected
##  @c            to contain the "equality range" as defined above.
##
##  @note       None
##
##  @danger     None
##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   variable result1
   variable result2

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##   Check that the arguments are not empty
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   if { ("" == $expression1)  ||  ("" == $expression2)  ||  ("" == $range) } {
      set returnValue "EMPTY_ARG: Empty arguments not allowed"
      return $returnValue
   }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##   Check optional argument to properly format error return string
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   if { "" == $errorString } {
      set errorReturn "NOT_EQUAL: Compare failure -- not equal: "
   } else {
      set errorReturn $errorString
   }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##   Initialize the value to be returned; anticipates success
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   set returnValue "OK"

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##   Evaluate expressions, then check if result are equal
  ##
  ##    The "catch" block will catch, parse, and remove the error string from
  ##    'invalid command name "<command>"' which is the result of a command
  ##    that cannot be evaluated.  The resulting parse will leave only the
  ##    <command> phrase inside the double quotation marks.
  ##
  ##    The same could be done more easily with the statement
  ##
  ##              set result [eval $expression]
  ##
  ##    but it would fail if the expression to be evaluated contains a value
  ##    like "1", and will return a Tcl error rather than the result we want.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   if { [catch {[eval $expression1]} result1] } {
      set index [string first \" $result1]
      set  result1 [string range $result1 [expr $index + 1] end-1]
   }

   if { [catch {[eval $expression2]} result2] } {
      set index [string first \" $result2]
      set  result2 [string range $result2 [expr $index + 1] end-1]
   }

   if { ![string is integer -strict $result1] && ![string is double -strict $result1] } {
      set returnValue "ARG_MISMATCH: $result1 is not a number"
      return $returnValue
   }

   if { ![string is integer -strict $result2] && ![string is double -strict $result2] } {
      set returnValue "ARG_MISMATCH: $result2 is not a number"
      return $returnValue
   }

   if { $range < [expr abs([expr $result1 - $result2])] } {
      set returnValue "$errorReturn $result1 $result2"
   }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##   Send back the result to the calling procedure
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   return $returnValue
}
