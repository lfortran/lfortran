module m
    implicit none
    integer :: x[*]
end module

module m2
    implicit none
    integer :: x[*]
end module

subroutine foo()
    implicit none
    integer, save :: x[*]

    x = this_image() + 100
    sync all

    if (x /= this_image() + 100) then
        error stop "Incorrect SAVE coarray value"
    end if
end subroutine

program coarrays_26
    use m, only: module_x => x
    use m2, only: module_x2 => x
    implicit none

    integer :: x[*]

    module_x = this_image()
    module_x2 = this_image() + 1
    x = this_image() * 10

    call foo()

    sync all

    if (x /= this_image() * 10) then
        error stop "Incorrect program coarray value"
    end if

    if (module_x /= this_image()) then
        error stop "Incorrect module coarray value in module m"
    end if

    if (module_x2 /= this_image() + 1) then
        error stop "Incorrect module coarray value in module m2"
    end if
end program coarrays_26