module pass_array_extname_mod_a
implicit none
contains
    subroutine process(arr)
        real, intent(inout) :: arr(:)
        arr = arr + 1.0
    end subroutine
end module

module pass_array_extname_mod_b
implicit none
contains
    subroutine process(arr)
        real, intent(inout) :: arr(:)
        arr = arr * 2.0
    end subroutine
end module

program pass_array_by_data_extname_01
    use pass_array_extname_mod_a, only: process_a => process
    use pass_array_extname_mod_b, only: process_b => process
    implicit none
    real :: x(4)
    x = 1.0
    call process_a(x)
    call process_b(x)
    if (abs(x(1) - 4.0) > 1.0e-6) error stop
    if (abs(x(2) - 4.0) > 1.0e-6) error stop
    if (abs(x(3) - 4.0) > 1.0e-6) error stop
    if (abs(x(4) - 4.0) > 1.0e-6) error stop
    print *, "PASS"
end program
