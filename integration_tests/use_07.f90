module use_07_left
    implicit none
    integer, save :: value = 13
end module

module use_07_right
    implicit none
    integer, save :: value = 29
end module

module use_07_alias
    use use_07_left, only: left => value
    use use_07_right, only: right => value
    implicit none
contains
    subroutine change_left()
        left = 41
    end subroutine
end module

program use_07
    use use_07_alias, only: left, right, change_left
    implicit none
    if (left /= 13 .or. right /= 29) error stop 1
    call change_left()
    if (left /= 41 .or. right /= 29) error stop 2
end program
