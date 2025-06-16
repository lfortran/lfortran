module procedure_20_mod1
   implicit none

   public cauchy

   interface cauchy
      module procedure cauchy_sngl
   end interface cauchy

   contains

    subroutine cauchy_sngl(f, i)
        interface
            function f(z) result(r)
                integer r
                complex, intent(in) :: z
            end function f
        end interface

        integer, intent(out) :: i
        integer, parameter :: n = 2
        i = n * f((1, 2))
    end subroutine cauchy_sngl
end module procedure_20_mod1

module procedure_20_mod2
    use procedure_20_mod1
    implicit none
    contains
    subroutine wind0_sngl(i, f)
        interface
            function f(z) result(r)
                integer r
                complex, intent(in) :: z
            end function f
        end interface

        integer, intent(out) :: i

        call cauchy(f, i)
    end subroutine wind0_sngl
end module procedure_20_mod2

program procedure_20
   use procedure_20_mod2
   implicit none
   integer :: i

   call wind0_sngl(i, test_function)
   print *, "i: ", i
   if (i /= 10) error stop

contains
   function test_function(z) result(r)
      complex, intent(in) :: z
      integer :: r
      r = 5
   end function test_function
end program procedure_20
