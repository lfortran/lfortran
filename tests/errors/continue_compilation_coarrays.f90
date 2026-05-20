program continue_compilation_coarrays

    ! Test: coindex notation on non-coarray variable should produce error
    integer :: x
    x = x[1]

    ! Test: corank mismatch should produce error
    integer :: B[3,*]
    B[1] = 5

    type :: t
        integer :: x
        integer :: y[3,*]
    end type t
    type(t) :: s
    ! Should error: x is not a coarray
    s%x = s%x[1]
    
    ! Corank = 2, but only 1 coindex provided → ERROR
    s%y[1] = 5

    integer :: z[*], a
    a = z[1:5:2]

    real :: cod[4]
    print *,cod[5]

end program continue_compilation_coarrays