! Which companions of a saved coarray are exported from the object file
! (#13413). Only those of a coarray a module owns are, whether the module or
! one of its procedures declares it: another translation unit that uses the
! module refers to them by name. Those of a coarray an external procedure or
! a program declares, or a procedure either contains, and the translation
! unit's own initializer, are private to it, so that another translation unit
! declaring a coarray of the same name does not clash with them at link time.
! The LLVM reference test registered for this file pins the linkage of each.
module coarrays_52_m
    implicit none
    integer :: m[*] = 1
contains
    subroutine coarrays_52_mod_sub(v)
        integer, intent(out) :: v
        integer, save :: m(2)[*] = 2
        m = m + 1
        v = m(1) + m(2)
    end subroutine
end module

subroutine coarrays_52_sub(v)
    implicit none
    integer, intent(out) :: v
    integer, save :: m[*] = 3
    m = m + 1
    v = m
end subroutine

program coarrays_52
    use coarrays_52_m, only: module_m => m, coarrays_52_mod_sub
    implicit none
    interface
        subroutine coarrays_52_sub(v)
            integer, intent(out) :: v
        end subroutine
    end interface
    integer, save :: m[*] = 5
    integer :: v

    if (module_m /= 1) error stop 1
    call coarrays_52_mod_sub(v)
    if (v /= 6) error stop 2
    call coarrays_52_sub(v)
    if (v /= 4) error stop 3
    call coarrays_52_int_sub(v)
    if (v /= 5) error stop 4
    if (m /= 5) error stop 5

    sync all
    if (module_m[1] /= 1) error stop 6
    if (m[1] /= 5) error stop 7
    if (this_image() == 1) print *, "ok"

contains

    subroutine coarrays_52_int_sub(v)
        integer, intent(out) :: v
        integer, save :: m[*] = 4
        m = m + 1
        v = m
    end subroutine

end program
