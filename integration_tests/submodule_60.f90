! A separate module procedure (`module procedure`) starts from the implicit
! typing rules of its host, the submodule, and its own IMPLICIT statement
! overrides only the letters it names (F2018 8.7 p4).
module submodule_60_mod
interface
    module subroutine own_implicit(k)
        integer, intent(out) :: k
    end subroutine
    module function no_implicit() result(r)
        integer :: r
    end function
end interface
end module

submodule (submodule_60_mod) submodule_60_sub
implicit double precision (a-h)
contains
    module procedure own_implicit
        implicit integer (a-c)
        a = 1
        d = 2
        k = kind(d) + 10 * kind(a)
        call inner()
    contains
        subroutine inner()
            b = 1
            if (kind(b) /= 4) error stop 1
            e = 1
            if (kind(e) /= 8) error stop 2
        end subroutine
    end procedure

    module procedure no_implicit
        a = 1
        r = kind(a)
    end procedure
end submodule

program submodule_60
use submodule_60_mod
integer :: k
call own_implicit(k)
print *, k
if (k /= 48) error stop 3
print *, no_implicit()
if (no_implicit() /= 8) error stop 4
end program
