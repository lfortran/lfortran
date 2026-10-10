module module_directory_01_m
    implicit none
contains
    subroutine increment(value)
        integer, intent(inout) :: value
        value = value + 2
    end subroutine
end module

program module_directory_01
    use module_directory_01_m, only: increment
    implicit none
    integer :: value
    value = 40
    call increment(value)
    if (value /= 42) error stop 1
end program
