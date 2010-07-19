## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##
##  File Name:  assertFalse.tcl
##  @short      Check if the argument passed in evaluates to boolean FALSE
##  @author     Original author ~ Joe Boyle
##  @date       Original date   ~ 14-Feb-2006
##  @version    1.0   Initial release
##
##  @comment      This module file provides functionality to evaluate a Tcl
##  @c            expression and decide if the result of that evaluation is
##  @c            boolean FALSE.  The first argument passed is the expression
##  @c            to be evaluated.  An optional second argument, if not an
##  @c            empty string, constitutes a string literal that is used if
##  @c            the evaluated expression is NOT FALSE (i.e., boolean TRUE).
##  @c            In the case for which the expression DOES evaluate to FALSE
##  @c            the procedure returns "OK".  Otherwise, the procedure will
##  @c            return a standard error string, unless the optional second
##  @c            argument contains a string; in the second case the argument
##  @c            value is used as the error return.
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
## version 1.0--  14-Feb-2006  Joe Boyle     Initial version written
##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  NO MODULE GLOBAL VARIABLES
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

proc t-unit::assertFalse { expression { errorString "" } } {
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  Proc Name:  assertFalse
##  @short      Check if the argument passed in evaluates to boolean FALSE
##  @argument   expression   Expression to evaluate for boolean result
##  @a          errorString  Optional string literal specifying error message
##  @result     returns "OK" on success, error string on failure
##  @example    set values [t-unit::assertFalse this "Error: Not FALSE"]
##
##  @comment      This module file provides functionality to evaluate a Tcl
##  @c            expression and decide if the result of that evaluation is
##  @c            boolean FALSE.  The first argument passed is the expression
##  @c            to be evaluated.  An optional second argument, if not an
##  @c            empty string, constitutes a string literal that is used if
##  @c            the evaluated expression is NOT FALSE (i.e., boolean TRUE).
##  @c            In the case for which the expression DOES evaluate to FALSE
##  @c            the procedure returns "OK".  Otherwise, the procedure will
##  @c            return a standard error string, unless the optional second
##  @c            argument contains a string; in the second case the argument
##  @c            value is used as the error return.
##
##  @note         Note that this procedure only checks for the expression in the
##  @n            argument to evaluate to boolean FALSE.  All other values are
##  @n            considered a failure.&p&p
##  @n
##  @n            The Tcl language defines booleans as one of "0, 1, true, or
##  @n            false (in any case)" (Welch book p. 54).  For this reason,
##  @n            the procedure will convert the result of the evaluation of
##  @n            the expression to UPPERCASE, and will check that the result
##  @n            is not an integer.  This should disambiguate between those
##  @n            expressions which return integer zero or one as (possible)
##  @n            evaluations and those which are truly boolean.
##
##  @danger     None
##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##   Check that the arguments are not empty
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   if { "" == $expression } {
      set returnValue "EMPTY_ARG: Empty arguments not allowed"
      return $returnValue
   }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##   Check optional argument to properly format error return string
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   if { "" == $errorString } {
      set errorReturn "NOT_FALSE: Expected expression evaluating to boolean FALSE; got"
   } else {
      set errorReturn $errorString
   }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##   Initialize the value to be returned; anticipates success
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   set returnValue "OK"

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##   Evaluate expression, then check if result is "FALSE"
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
   if { [catch {[eval $expression]} result] } {
      set index [string first "FALSE" $result]
      if { -1 != $index } {
         set result [string range $result $index end-1]
      } else {
         set index [string first \" $result]
         set  result [string range $result [expr $index + 1] end-1]
      }
   }

   if { "FALSE" != [string toupper $result] } {
      set returnValue "$errorReturn $result"
   }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##   Send back the result to the calling procedure
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   return $returnValue
}
