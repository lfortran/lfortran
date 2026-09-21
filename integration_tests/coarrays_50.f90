! A saved coarray declared inside a procedure. `coarrays_48` covers the ones
! declared in a module or a program directly; these are allocated by the same
! unit initializers, because the coarray pass walks out of the procedure to
! the module or program that encloses it:
!
!   * `mod_bump`'s `mc` is allocated by the module's initializer,
!   * `int_bump`'s `ic` by the program's,
!
! while a saved coarray of an *external* procedure, which `coarrays_16` and
! `coarrays_26` cover, belongs to no program unit and stays on the translation
! unit. The allocation is collective either way, so it has to happen in a
! startup initializer rather than on first entry to the procedure.
!
! Reported as a coverage gap by @bonachea in review of #13221.
module coarrays_50_m
    implicit none
contains
    ! Saved coarray of a MODULE procedure.
    subroutine mod_bump(v)
        integer, intent(out) :: v
        integer, save :: mc[*] = 100
        mc = mc + 1
        v = mc
    end subroutine

    ! Read another image's copy, which only works if every image allocated it.
    subroutine mod_remote(v)
        integer, intent(out) :: v
        integer, save :: mr[*] = 500
        mr = 500 + this_image()
        sync all
        v = mr[1]
    end subroutine
end module

program coarrays_50
    use coarrays_50_m
    implicit none
    integer :: a, b

    ! The save attribute holds across calls: each is initialized once, by the
    ! initializer, not on entry.
    call mod_bump(a)
    if (a /= 101) error stop 1
    call mod_bump(b)
    if (b /= 102) error stop 2

    call int_bump(a)
    if (a /= 201) error stop 3
    call int_bump(b)
    if (b /= 202) error stop 4

    call mod_remote(a)
    if (a /= 501) error stop 5
    call int_remote(b)
    if (b /= 601) error stop 6

    if (this_image() == 1) print *, "ok"

contains

    ! Saved coarray of an INTERNAL procedure.
    subroutine int_bump(v)
        integer, intent(out) :: v
        integer, save :: ic[*] = 200
        ic = ic + 1
        v = ic
    end subroutine

    subroutine int_remote(v)
        integer, intent(out) :: v
        integer, save :: ir[*] = 600
        ir = 600 + this_image()
        sync all
        v = ir[1]
    end subroutine

end program
