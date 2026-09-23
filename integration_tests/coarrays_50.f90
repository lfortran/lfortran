! A saved coarray declared inside a procedure. `coarrays_48` covers the ones
! declared in a module or a program directly; these are allocated by the same
! unit initializers, because the coarray pass walks out of the procedure to
! the module or program that encloses it:
!
!   * `mod_bump`'s `i` is allocated by the module's initializer,
!   * `int_bump`'s `i` by the program's,
!
! while a saved coarray of an *external* procedure, which `coarrays_16` and
! `coarrays_26` cover, belongs to no program unit and stays on the translation
! unit. The allocation is collective either way, so it has to happen in a
! startup initializer rather than on first entry to the procedure.
!
! Image 2 never calls the procedures that declare these coarrays. Allocating
! one is collective, so an implementation that waited until first entry to the
! procedure would have three images allocating while the fourth never does,
! and would hang or mismatch here rather than pass.
!
! Reported as a coverage gap, and this test strengthened, by @bonachea in
! review of #13221.
!
! Every one of these coarrays is named `i`, so the names the coarray pass
! derives for them collide: the reference test registered for this file
! pins that each procedure is left referring to its own coarray (#13413).
module coarrays_50_m
    implicit none
contains
    ! Saved coarray of a MODULE procedure.
    subroutine mod_bump(v)
        integer, intent(out) :: v
        integer, save :: i[*] = 100
        i = i + 1
        v = i
    end subroutine

    ! Read another image's copy, which only works if every image allocated it.
    ! Adding to the initial value rather than overwriting it also checks that
    ! the initializer ran, not merely that the storage exists.
    subroutine mod_remote(v)
        integer, intent(out) :: v
        integer, save :: i[*] = 500
        i = i + this_image()
        sync all
        v = i[1]
    end subroutine
end module

program coarrays_50
    use coarrays_50_m
    implicit none
    integer :: a, b

    ! The save attribute holds across calls: each is initialized once, by the
    ! initializer, not on entry. Image 2 sits these out, so the allocation
    ! cannot have been driven by the call.
    if (this_image() /= 2) then
        call mod_bump(a)
        if (a /= 101) error stop 1
        call mod_bump(b)
        if (b /= 102) error stop 2

        call int_bump(a)
        if (a /= 201) error stop 3
        call int_bump(b)
        if (b /= 202) error stop 4
    end if

    ! Every image takes part in these: they are collective.
    call mod_remote(a)
    if (a /= 501) error stop 5
    call int_remote(b)
    if (b /= 601) error stop 6

    if (this_image() == 1) print *, "ok"

contains

    ! Saved coarray of an INTERNAL procedure.
    subroutine int_bump(v)
        integer, intent(out) :: v
        integer, save :: i[*] = 200
        i = i + 1
        v = i
    end subroutine

    subroutine int_remote(v)
        integer, intent(out) :: v
        integer, save :: i[*] = 600
        i = i + this_image()
        sync all
        v = i[1]
    end subroutine

end program
