module allocate_30_mod
    implicit none
contains

    subroutine fill_strings(strings)
        character(len=*), allocatable, intent(inout) :: strings(:)
        character(len=:), allocatable :: tmp(:)

        tmp = [character(len=2) :: "aa", "bb"]

        if (len(tmp) <= len(strings)) then
            strings = tmp
        else
            strings = [character(len=len(strings)) ::]
        end if
    end subroutine fill_strings

end module allocate_30_mod

program allocate_30
    use allocate_30_mod, only: fill_strings
    implicit none

    character(len=4), allocatable :: strings(:)

    allocate(strings(1))
    strings = "    "

    call fill_strings(strings)

    if (.not. allocated(strings)) error stop
    if (size(strings) /= 2) error stop
    if (strings(1) /= "aa  ") error stop
    if (strings(2) /= "bb  ") error stop

    print *, "ok"
end program allocate_30
