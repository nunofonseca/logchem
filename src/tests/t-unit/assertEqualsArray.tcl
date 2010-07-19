## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##
##  File Name:  assertEqualsArray.tcl
##  @short      Check if the arguments passed in are indeed equivalent.  
##  @author     Original author - Joe Boyle
##  @date       Original date - 07-Jun-2006
##  @version    1.0   Initial release
##
##  @comment      This module file provides functionality to evaluate two Tcl
##  @c            arrays and decide if the results of the evaluations will
##  @c            compare exactly.  Arrays must contain the same exact data
##  @c            elements to be equal.  For example, comparing the results of
##  @c            expressions evaluating to the array containing A(1) = 4 and the
##  @c            array containing B(2) = 4 are not considered equal.
##
##  @note       See procedure definition header below for more details.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  Revision history
##  ----------------
##
##          Rev    Rev. Date   Released by:  Revision Description
##         -----  -----------  ------------  --------------------------------
## version 1.0--  07-Jun-2006  Joe Boyle     Initial version written
##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  NO MODULE GLOBAL VARIABLES
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

proc t-unit::assertEqualsArray { expression1 expression2 {errorString ""} } {
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  Proc Name:  assertEqualsArray
##  @short      Check if the arguments passed in are indeed equal to each other
##  @argument   expression1   First expression to evaluate
##  @a          expression2   Second expression to evaluate
##  @a          errorString   Optional string literal specifying error message
##  @result     returns "OK" on success, error string on failure
##  @example    set errVal [t-unit::assertEqualsArray this that "Error: Not equal"]
##
##  @comment      This module file provides functionality to evaluate two Tcl
##  @c            arrays and decide if the results of the evaluations will
##  @c            compare exactly.  Arrays must contain the same exact data
##  @c            elements to be equal.  For example, comparing the results of
##  @c            expressions evaluating to the array containing A(1) = 4 and the
##  @c            array containing B(2) = 4 are not considered equal.
##  @c
##  @c            Note that this procedure is not intended to replace the normal
##  @c            comparison of, for example, two integers.  If you want to
##  @c            check if 2 == 2, you'd still do it that way.  This procedure
##  @c            is intended to check two arrays for equality, as part of a unit 
##  @c            test on the procedure(s).&p&p
##  @c
##  @c            The normal usage would be to have the first argument be the
##  @c            expected value, with the second argument containing the name
##  @c            of the array to test.  The third argument is expected to
##  @c            contain one of the data types allowed.
##
##  @note         The array elements must not contain procedures or expressions
##  @n            to be evaluated.  Evaluation on the individual array items
##  @n            is not supported.
##
##  @danger     None
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   upvar #0 $expression1 array1 $expression2 array2

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##   Check if the arrays exist
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if { (0 == [array exists array1]) ||  (0 == [array exists array2]) } {
     set returnValue "NOT_ARRAY: Array must exist"
     return $returnValue
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##   Check if either argument is an empty array; if both are, it's OK
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   if { (0 == [array size array1])  &&  (0 == [array size array2]) } {
      return "OK"
   } else {
      if { [array size array1]  !=  [array size array2] } {
         set returnValue "SIZE_MISMATCH: Array sizes must match"
         return $returnValue
      }
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
  ##   Compare the arrays
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   set indicies1 [array names array1]
   set indicies2 [array names array2]
   
   foreach value $indicies1 {
      if { [lsearch $indicies2 $value] == -1 } {
         set returnValue "INDEX_NONEXISTENT: Array index $value does not exist"
         return $returnValue
      }
   }
   
   foreach value $indicies1 {
      if { $array1($value) ne $array2($value) } {
         set returnValue "$errorReturn $array1($value)/$array2($value)"
         break
      }
   }
     
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##   Send back the result to the calling procedure
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   return $returnValue
}
