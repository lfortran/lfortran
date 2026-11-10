! Equality and inequality between a complex(8) variable and default real
! constants, which are converted to complex(8) (#3417).
program complex_50
    implicit none
    complex(8) :: x
    logical :: printed

    x = 1.0
    printed = .false.
    if (x /= 2.0) then
        print *, "Valar Morghulis"
        printed = .true.
    end if
    if (.not. printed) error stop 1

    if (x == 2.0) error stop 2
    if (.not. (x == 1.0)) error stop 3
    if (x /= 1.0) error stop 4
    if (.not. (2.0 /= x)) error stop 5

    x = (1.0d0, 3.0d0)
    if (.not. (x /= 1.0)) error stop 6
    if (x == 1.0) error stop 7
end program complex_50
