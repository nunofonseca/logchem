## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##
##  File Name:  assertNotEquals.tcl
##  @short      Check if the arguments passed in are indeed not equivalent
##  @author     Original author ~ Joe Boyle
##  @date       Original date   ~ 15-Feb-2006
##  @version    1.0   Initial release
##
##  @comment      This module file provides functionality to evaluate two Tcl
##  @c            expressions and decide if the results of the evaluations
##  @c            are not equal.  The arguments are both assumed to be
##  @c            evaluable expressions.  
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

proc t-unit::assertNotEquals { expression1 expression2 {errorString ""} }  {
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  Proc Name:  assertNotEquals
##  @short      Check if the arguments passed in are indeed not equal to each other
##  @argument   expression1   First expression to evaluate
##  @a          expression2   Second expression to evaluate
##  @a          errorString   Optional string literal specifying error message
##  @result     returns "OK" on success, error string on failure
##  @example    set errVal [t-unit::assertNotEquals this that "Error: Not equal"]
##
##  @comment      This module file provides functionality to evaluate two Tcl
##  @c            expressions and decide if the results of the evaluations
##  @c            are not equal.&p&p
##  @c
##  @c            Note that this procedure is not intended to replace the normal
##  @c            comparison of, for example, two integers.  If you want to
##  @c            check if 2 != 3, you'd still do it that way.  This procedure
##  @c            is intended to check two PROCEDURES, or EXPRESSIONS, for
##  @c            equality, as part of a unit test on the procedure(s).&p&p
##  @c
##  @c            The normal usage would be to have the first argument be the
##  @c            expected value, with the second argument containing the name
##  @c            of the procedure to test.  
##
##  @note       None.
##
##  @danger     None
##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   variable result1
   variable result2

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##   Check that the arguments are not empty
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   if { ("" == $expression1)  ||  ("" == $expression2) } {
      set returnValue "EMPTY_ARG: Empty arguments not allowed"
      return $returnValue
   }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##   Check if either argument is of integer type; mustn't compare integers
  ##     with doubles
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   if { [string is integer -strict $expression1] } {
      if { ![string is integer $expression2] } {
         set returnValue "ARG_MISMATCH: Argument types must match"
         return $returnValue
      }
   }
   if { [string is integer -strict $expression2] } {
      if { ![string is integer $expression1] } {
         set returnValue "ARG_MISMATCH: Argument types must match"
         return $returnValue
      }
   }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##   Check optional argument to properly format error return string
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   if { "" == $errorString } {
      set errorReturn "EQUAL: Divergence failure -- equal: "
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

   if { $result1 == $result2 } {
      set returnValue "$errorReturn $result1 $result2"
   }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##   Send back the result to the calling procedure
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   return $returnValue
}
