! A saved coarray of a module and of a program is allocated by that unit's
! startup initializer, which the program calls before its first statement,
! rather than by a constructor the target runs before main.
module coarrays_48_m
    implicit none
    integer, save :: mc[*] = 7
end module

program coarrays_48
    use coarrays_48_m
    implicit none
    integer, save :: pc[*] = 13
    integer :: me

    me = this_image()

    ! Both are allocated and hold their initial value before anything here
    ! runs, whichever unit declares them.
    if (mc /= 7) error stop 1
    if (pc /= 13) error stop 2

    mc = 7 + me
    pc = 13 + me
    sync all

    if (mc /= 7 + me) error stop 3
    if (pc /= 13 + me) error stop 4

    ! Every image allocated both coarrays, so they can be read remotely.
    if (mc[1] /= 8) error stop 5
    if (pc[1] /= 14) error stop 6

    if (me == 1) print *, "ok"
end program
