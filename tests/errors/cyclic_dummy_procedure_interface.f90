! The interface of `g` depends on the characteristics of `g` through those
! of `k` (Fortran 2023 15.4.3.6): cpi_f -> g -> cpi_h -> k -> cpi_f.
module cyclic_dummy_procedure_interface_m
implicit none
type :: holder
    procedure(cpi_f), pointer, nopass :: pf => null()
end type
contains
    recursive integer function cpi_f(g, n) result(r)
        procedure(cpi_h) :: g
        integer, intent(in) :: n
        r = n
        if (n > 0) r = g(cpi_f, n - 1)
    end function

    recursive integer function cpi_h(k, n) result(r)
        procedure(cpi_f) :: k
        integer, intent(in) :: n
        r = k(cpi_h, n)
    end function
end module

program cyclic_dummy_procedure_interface
use cyclic_dummy_procedure_interface_m
implicit none
type(holder) :: p
p%pf => cpi_f
print *, p%pf(cpi_h, 1)
end program
