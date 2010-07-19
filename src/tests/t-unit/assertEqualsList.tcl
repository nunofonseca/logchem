## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##
##  File Name:  assertEqualsList.tcl
##  @short      Check if the arguments passed in are indeed equivalent
##  @author     Original author ~ Joe Boyle
##  @date       Original date   ~ 15-Feb-2006
##  @version    1.0   Initial release
##
##  @comment      This module file provides functionality to evaluate two Tcl
##  @c            lists and decide if the results of the evaluations will
##  @c            compare exactly.  Lists must contain the same exact data
##  @c            elements to be equal.  For example, comparing the results of
##  @c            expressions evaluating to the list containing "1 2 3" and the
##  @c            list containing "{1 2} 3" are not considered equal.
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

proc t-unit::assertEqualsList { expression1 expression2 {errorString ""} }  {
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  Proc Name:  assertEqualsList
##  @short      Check if the arguments passed in are indeed equal to each other
##  @argument   expression1   First expression to evaluate
##  @a          expression2   Second expression to evaluate
##  @a          errorString   Optional string literal specifying error message
##  @result     returns "OK" on success, error string on failure
##  @example    set errVal [t-unit::assertEqualsList {1 2} {1 3} "Error: Not equal"]
##
##  @comment      This module file provides functionality to evaluate two Tcl
##  @c            lists and decide if the results of the evaluations will
##  @c            compare exactly.  Lists must contain the same exact data
##  @c            elements to be equal.  For example, comparing the results of
##  @c            expressions evaluating to the list containing "1 2 3" and the
##  @c            list containing "{1 2} 3" are not considered equal.&p&p
##  @c
##  @c            Note that this procedure is not intended to replace the normal
##  @c            comparison of, for example, two integers.  If you want to
##  @c            check if 2 == 2, you'd still do it that way.  This procedure
##  @c            is intended to check two PROCEDURES, or EXPRESSIONS, for
##  @c            equality, as part of a unit test on the procedure(s).&p&p
##  @c
##  @c            The normal usage would be to have the first argument be the
##  @c            expected value, with the second argument containing the name
##  @c            of the procedure to test.  
##
##  @note         The list elements must not contain procedures or expressions
##  @n            to be evaluated.  Evaluation on the individual list items
##  @n            is not supported.
##
##  @danger     None
##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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
  ##   Compare the lists
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   foreach value1 $expression1 value2 $expression2 {
      if { $value1 ne $value2 } {
         set returnValue "$errorReturn $expression1/$expression2"
         break
      }
   }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##   Send back the result to the calling procedure
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   return $returnValue
}
