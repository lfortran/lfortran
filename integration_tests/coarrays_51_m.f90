! Modules with saved coarrays, compiled into an object file of their own.
! That object file allocates them and binds them to their companions, so a
! translation unit that only uses these modules must name that initializer
! rather than define a second one, and must not rebind the coarrays to
! companions of its own.
module coarrays_51_a
    implicit none
    integer, save :: ca[*] = 11
end module

module coarrays_51_b
    implicit none
    type :: point
        integer :: x = 0
    end type
    ! A saved coarray together with something the global-init pass owns in
    ! its own right: the initializer this module needs already exists as a
    ! declaration by the time the coarray pass reaches it.
    integer, save :: cb[*] = 22
    type(point), save :: pts(2) = point(5)
    integer, pointer :: p => null()
end module
