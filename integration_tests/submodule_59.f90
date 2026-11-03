! The implicit typing rules of a submodule do not apply to the program
! units that follow it in the same file (F2018 8.7 p4).
module submodule_59_mod
interface
    module subroutine set(k)
        integer, intent(out) :: k
    end subroutine
end interface
end module

submodule (submodule_59_mod) submodule_59_sub
implicit double precision (a-h)
contains
    module subroutine set(k)
        integer, intent(out) :: k
        b = 1
        k = kind(b)
    end subroutine
end submodule

module submodule_59_mod2
interface
    module subroutine set2(k)
        integer, intent(out) :: k
    end subroutine
end interface
end module

submodule (submodule_59_mod2) submodule_59_sub2
implicit none
contains
    module subroutine set2(k)
        integer, intent(out) :: k
        k = 4
    end subroutine
end submodule

subroutine submodule_59_ext(k)
    integer, intent(out) :: k
    i = 3
    k = kind(i)
end subroutine

program submodule_59
use submodule_59_mod
use submodule_59_mod2
implicit integer (i-n)
integer :: k
call set(k)
if (k /= 8) error stop 1
call set2(k)
if (k /= 4) error stop 2
b = 1
if (kind(b) /= 4) error stop 3
call submodule_59_ext(k)
if (k /= 4) error stop 4
print *, b, k
end program
