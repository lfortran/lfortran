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
    ! a `parameter` of the parent type is itself a valid parent component value
    type(a_t), parameter :: p_a = a_t(111, 10.5)
    type(b_t), parameter :: p_from_param = b_t(a_t=p_a, y=112)
    ! counts the calls to `make_a()`, to check the parent component value is
    ! evaluated exactly once however many components the parent has
    integer :: n_calls = 0
    ! counts the calls to `next_idx()`, to check a subscript of the parent
    ! component value is evaluated exactly once as well
    integer :: n_idx_calls = 0
contains
    function make_a() result(res)
        type(a_t) :: res
        n_calls = n_calls + 1
        res = a_t(121, 11.5)
    end function
    function next_idx() result(res)
        integer :: res
        n_idx_calls = n_idx_calls + 1
        res = 2
    end function
end module

program structure_constructor_args_12
    use structure_constructor_args_12_m
    implicit none
    type(a_t) :: a
    type(a_t) :: a_src(3)
    type(b_t) :: b
    type(b_t) :: b_arr(3)
    type(b_t) :: b_one(1)
    type(b_t) :: b_two(2)
    type(b_t) :: b_mat(4)
    type(c_t) :: c
    integer :: i, j, msk(3)

    do i = 1, 3
        a_src(i) = a_t(i * 10, real(i))
    end do

    if (p_parent%x /= 11 .or. p_parent%r /= 2.5 .or. p_parent%y /= 12) error stop
    if (p_nested%x /= 21 .or. p_nested%r /= 3.5) error stop
    if (p_nested%y /= 22 .or. p_nested%z /= 23) error stop
    if (p_from_param%x /= 111 .or. p_from_param%r /= 10.5) error stop
    if (p_from_param%y /= 112) error stop

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

    ! the parent component value is evaluated exactly once, even though it
    ! supplies every component the parent owns
    b = b_t(a_t=make_a(), y=122)
    if (b%x /= 121 .or. b%r /= 11.5 .or. b%y /= 122) error stop
    if (n_calls /= 1) error stop

    ! an array constructor evaluates the parent component value of each of its
    ! elements exactly once
    n_calls = 0
    b_one = [ b_t(a_t=make_a(), y=142) ]
    if (b_one(1)%x /= 121 .or. b_one(1)%r /= 11.5 .or. b_one(1)%y /= 142) error stop
    if (n_calls /= 1) error stop

    n_calls = 0
    b_two = [ b_t(a_t=make_a(), y=143), b_t(a_t=make_a(), y=144) ]
    if (b_two(1)%x /= 121 .or. b_two(1)%r /= 11.5 .or. b_two(1)%y /= 143) error stop
    if (b_two(2)%x /= 121 .or. b_two(2)%r /= 11.5 .or. b_two(2)%y /= 144) error stop
    if (n_calls /= 2) error stop

    ! a real loop evaluates it once per iteration
    n_calls = 0
    do i = 1, 3
        b_arr(i) = b_t(a_t=make_a(), y=i)
    end do
    if (n_calls /= 3) error stop

    ! inside an array constructor the parent component value belongs to each
    ! iteration of the implied do loop
    b_arr = [ (b_t(a_t=a_src(i), y=i), i = 1, 3) ]
    do i = 1, 3
        if (b_arr(i)%x /= i * 10 .or. b_arr(i)%r /= real(i)) error stop
        if (b_arr(i)%y /= i) error stop
    end do

    ! and the same in a nested implied do loop
    b_mat = [ ((b_t(a_t=a_src(i), y=10 * j + i), i = 1, 2), j = 1, 2) ]
    do j = 1, 2
        do i = 1, 2
            if (b_mat(2 * (j - 1) + i)%x /= i * 10) error stop
            if (b_mat(2 * (j - 1) + i)%r /= real(i)) error stop
            if (b_mat(2 * (j - 1) + i)%y /= 10 * j + i) error stop
        end do
    end do

    ! a subscript of the parent component value is evaluated exactly once too
    n_idx_calls = 0
    b = b_t(a_t=a_src(next_idx()), y=152)
    if (b%x /= 20 .or. b%r /= 2.0 .or. b%y /= 152) error stop
    if (n_idx_calls /= 1) error stop

    ! a masked assignment evaluates its value once, whatever the mask selects
    n_calls = 0
    msk = [1, 0, 1]
    b_arr = b_t(a_t=a_t(0, 0.0), y=0)
    where (msk == 1)
        b_arr = b_t(a_t=make_a(), y=162)
    end where
    if (n_calls /= 1) error stop
    if (b_arr(1)%x /= 121 .or. b_arr(1)%y /= 162) error stop
    if (b_arr(2)%x /= 0 .or. b_arr(2)%y /= 0) error stop
    if (b_arr(3)%x /= 121 .or. b_arr(3)%y /= 162) error stop

    n_calls = 0
    msk = [0, 0, 0]
    where (msk == 1)
        b_arr = b_t(a_t=make_a(), y=172)
    end where
    if (n_calls /= 1) error stop

    call check_local()
    call check_param_in_procedure()
    print *, "ok"

contains

    subroutine check_local()
        type(b_t) :: l = b_t(a_t=a_t(101, 9.5), y=102)
        if (l%x /= 101 .or. l%r /= 9.5 .or. l%y /= 102) error stop
    end subroutine

    subroutine check_param_in_procedure()
        type(a_t), parameter :: la = a_t(131, 12.5)
        type(b_t), parameter :: lb = b_t(a_t=la, y=132)
        if (lb%x /= 131 .or. lb%r /= 12.5 .or. lb%y /= 132) error stop
    end subroutine

end program
