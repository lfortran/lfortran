! Coarrays that share a name across scopes, and user variables named like
! the companions the coarray pass derives for a coarray. Every coarray here is
! named `x`, so each of those derived names collides with another; the
! reference test registered for this file pins that every scope is left
! referring to its own coarray, and that no user variable is shadowed by a
! companion (#13413).
module coarrays_26_m
    implicit none
    integer :: x[*]
end module

module coarrays_26_m2
    implicit none
    integer :: x[*]
end module

! Saved coarrays of a module procedure and of its internal procedures: one
! declares its own `x`, the other reaches its host's by host association.
module coarrays_26_m3
    implicit none
    integer :: x[*]
contains
    subroutine coarrays_26_mod_sub()
        integer, save :: x[*] = 10
        x = x + 1
        call coarrays_26_mod_inner()
        if (x /= 11) error stop "Incorrect module procedure SAVE coarray value"
        call coarrays_26_mod_host()
        if (x /= 12) error stop "Incorrect module procedure SAVE coarray value"
    contains
        subroutine coarrays_26_mod_inner()
            integer, save :: x[*] = 20
            x = x + 1
            if (x /= 21) error stop "Incorrect module internal SAVE coarray value"
        end subroutine

        subroutine coarrays_26_mod_host()
            x = x + 1
        end subroutine
    end subroutine
end module

subroutine coarrays_26_sub()
    implicit none
    integer, save :: x[*]
    integer :: x__coarray_ptr, x__coarray_handle, x__coarray_data

    x__coarray_ptr = -1
    x__coarray_handle = -2
    x__coarray_data = -3
    x = this_image() + 100

    call coarrays_26_sub2()
    call coarrays_26_inner()
    call coarrays_26_host()

    if (x /= this_image() + 101) then
        error stop "Incorrect SAVE coarray value"
    end if
    if (x__coarray_ptr /= -1 .or. x__coarray_handle /= -2 &
            .or. x__coarray_data /= -3) then
        error stop "Incorrect local variable value"
    end if
contains
    ! Saved coarray of an internal procedure of an external procedure.
    subroutine coarrays_26_inner()
        integer, save :: x[*] = 30
        x = x + 1
        if (x /= 31) error stop "Incorrect external internal SAVE coarray value"
    end subroutine

    subroutine coarrays_26_host()
        x = x + 1
    end subroutine
end subroutine

subroutine coarrays_26_sub2()
    implicit none
    integer, save :: x[*]

    x = this_image() + 1000

    if (x /= this_image() + 1000) then
        error stop "Incorrect SAVE coarray value"
    end if
end subroutine

program coarrays_26
    use coarrays_26_m, only: module_x => x
    use coarrays_26_m2, only: module_x2 => x
    use coarrays_26_m3, only: module_x3 => x, coarrays_26_mod_sub
    implicit none

    integer :: x[*]
    integer :: x__coarray_ptr = 7

    module_x = this_image()
    module_x2 = this_image() + 1
    module_x3 = this_image() + 2
    x = this_image() * 10

    call coarrays_26_sub()
    call coarrays_26_mod_sub()
    call coarrays_26_prog_inner()

    sync all

    if (x /= this_image() * 10) then
        error stop "Incorrect program coarray value"
    end if

    if (x__coarray_ptr /= 7) then
        error stop "Incorrect program variable value"
    end if

    if (module_x /= this_image()) then
        error stop "Incorrect module coarray value in module m"
    end if

    if (module_x2 /= this_image() + 1) then
        error stop "Incorrect module coarray value in module m2"
    end if

    if (module_x3 /= this_image() + 2) then
        error stop "Incorrect module coarray value in module m3"
    end if
contains
    ! Saved coarray of an internal procedure of the program.
    subroutine coarrays_26_prog_inner()
        integer, save :: x[*] = 40
        x = x + 1
        if (x /= 41) error stop "Incorrect program internal SAVE coarray value"
    end subroutine
end program coarrays_26
