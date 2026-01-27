program test_string_read_arrays
    implicit none
    call test_f32()
    call test_f64()
    call test_i32()
    call test_i64()
    print *, "ALL TESTS PASSED"
contains
    subroutine test_f32()
        implicit none
        real :: list(3)
        character(len=12) :: clist
        clist = "1.0 42. -42."
        read(clist, *) list
        if (abs(list(1) - 1.0)  > 1e-6) error stop
        if (abs(list(2) - 42.0) > 1e-6) error stop 
        if (abs(list(3) + 42.0) > 1e-6) error stop 
    end subroutine

    subroutine test_f64()
        implicit none
        real(8) :: list(3)
        character(len=15) :: clist
        clist = "1.5 2.5 -3.5"
        read(clist, *) list
        if (abs(list(1) - 1.5_8) > 1d-12) error stop 
        if (abs(list(2) - 2.5_8) > 1d-12) error stop 
        if (abs(list(3) + 3.5_8) > 1d-12) error stop 
    end subroutine

    subroutine test_i32()
        implicit none
        integer :: list(3)
        character(len=15) :: clist
        clist = "10 -20 30"
        read(clist, *) list
        if (list(1) /= 10)  error stop 
        if (list(2) /= -20) error stop 
        if (list(3) /= 30)  error stop 
    end subroutine

    subroutine test_i64()
        implicit none
        integer(8) :: list(3)
        character(len=33) :: clist
        clist = "1000000000 -2000000000 2000000000"
        read(clist, *) list
        if (list(1) /= 1000000000_8)  error stop 
        if (list(2) /= -2000000000_8) error stop 
        if (list(3) /= 2000000000_8)  error stop 
    end subroutine

end program test_string_read_arrays