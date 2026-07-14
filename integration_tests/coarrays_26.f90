module coarrays_26_m
    implicit none
    integer :: x[*]
end module

module coarrays_26_m2
    implicit none
    integer :: x[*]
end module

subroutine coarrays_26_sub()
    implicit none
    integer, save :: x[*]

    x = this_image() + 100

    call coarrays_26_sub2()
    
    if (x /= this_image() + 100) then
        error stop "Incorrect SAVE coarray value"
    end if
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
    implicit none

    integer :: x[*]

    module_x = this_image()
    module_x2 = this_image() + 1
    x = this_image() * 10

    call coarrays_26_sub()

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