! The parent component of an extended type is a component whose name is the
! name of the parent type, so a structure constructor may give it by keyword:
! `e_t(base_t=base_t(...), z=...)`.
module structure_constructor_args_12_m
    implicit none
    type :: a_t
        integer :: x = 1
        real :: r = 1.5
    end type
    type, extends(a_t) :: b_t
        integer :: y = 2
    end type
    type, extends(b_t) :: c_t
        integer :: z = 3
    end type
    type(b_t), parameter :: p_parent = b_t(a_t=a_t(11, 2.5), y=12)
    type(c_t), parameter :: p_nested = c_t(b_t=b_t(a_t=a_t(21, 3.5), y=22), z=23)
end module

program structure_constructor_args_12
    use structure_constructor_args_12_m
    implicit none
    type(a_t) :: a
    type(b_t) :: b
    type(c_t) :: c

    if (p_parent%x /= 11 .or. p_parent%r /= 2.5 .or. p_parent%y /= 12) error stop
    if (p_nested%x /= 21 .or. p_nested%r /= 3.5) error stop
    if (p_nested%y /= 22 .or. p_nested%z /= 23) error stop

    b = b_t(a_t=a_t(31, 4.5), y=32)
    if (b%x /= 31 .or. b%r /= 4.5 .or. b%y /= 32) error stop

    ! the parent component keyword may come in any order
    b = b_t(y=42, a_t=a_t(41, 5.5))
    if (b%x /= 41 .or. b%r /= 5.5 .or. b%y /= 42) error stop

    ! components not given take their default initialization
    b = b_t(a_t=a_t(51, 6.5))
    if (b%x /= 51 .or. b%r /= 6.5 .or. b%y /= 2) error stop
    b = b_t(y=62)
    if (b%x /= 1 .or. b%r /= 1.5 .or. b%y /= 62) error stop

    ! the value of the parent component may be any expression of the parent type
    a = a_t(71, 7.5)
    b = b_t(a_t=a, y=72)
    if (b%x /= 71 .or. b%r /= 7.5 .or. b%y /= 72) error stop

    c = c_t(b_t=b, z=82)
    if (c%x /= 71 .or. c%r /= 7.5 .or. c%y /= 72 .or. c%z /= 82) error stop

    c = c_t(b_t=b_t(a_t=a_t(91, 8.5), y=92), z=93)
    if (c%x /= 91 .or. c%r /= 8.5 .or. c%y /= 92 .or. c%z /= 93) error stop

    call check_local()
    print *, "ok"

contains

    subroutine check_local()
        type(b_t) :: l = b_t(a_t=a_t(101, 9.5), y=102)
        if (l%x /= 101 .or. l%r /= 9.5 .or. l%y /= 102) error stop
    end subroutine

end program
