program transfer_25
    implicit none
    type :: t_src
        integer :: x
    end type
    type :: t_dst
        integer :: y
    end type
    type(t_src) :: a
    type(t_dst) :: b
    a%x = 42
    b = transfer(a, b)
    if (b%y /= 42) error stop
end program transfer_25
