module interface_generic_procedure_same_name

   implicit none

   interface frexp
      function frexp(x,n) result(r)
         real r
         real, intent(in), value :: x
         integer, intent(out) :: n
      end function frexp
   end interface frexp

end module interface_generic_procedure_same_name
