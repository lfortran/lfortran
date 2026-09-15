! null() for a procedure pointer component whose interface is a module
! procedure defined later, in a constructor argument and in a component
! default filled into a constructor
module structure_constructor_args_10_mod
    implicit none
    type :: o_t
        integer :: x = 5
        procedure(g), pointer, nopass :: fp => null()
    end type
    type :: w_t
        type(o_t) :: inner = o_t()
        type(o_t) :: inner2 = o_t(6, null())
    end type
    type(o_t), parameter :: z_copy = o_t(3, null())
    type(o_t), parameter :: z_default = o_t()
    type(o_t) :: v_copy = o_t(4, null())
    type(o_t) :: v_default = o_t()
contains
    subroutine check_locals()
        type(o_t) :: l_copy = o_t(7, null())
        type(o_t) :: l_default = o_t()
        type(w_t) :: w
        if (l_copy%x /= 7 .or. associated(l_copy%fp)) error stop 1
        if (l_default%x /= 5 .or. associated(l_default%fp)) error stop 2
        if (w%inner%x /= 5 .or. associated(w%inner%fp)) error stop 3
        if (w%inner2%x /= 6 .or. associated(w%inner2%fp)) error stop 4
        l_copy%fp => g
        if (.not. associated(l_copy%fp, g)) error stop 5
    end subroutine

    integer function g(i)
        integer, intent(in) :: i
        g = 2*i
    end function
end module

program structure_constructor_args_10
    use structure_constructor_args_10_mod
    implicit none
    type :: p_t
        integer :: y = 1
        procedure(h), pointer, nopass :: hp => null()
    end type
    type(o_t) :: a = z_copy
    type(o_t) :: b = z_default
    type(p_t) :: c = p_t(2, null())
    type(p_t) :: d = p_t()
    if (a%x /= 3 .or. associated(a%fp)) error stop 6
    if (b%x /= 5 .or. associated(b%fp)) error stop 7
    if (v_copy%x /= 4 .or. associated(v_copy%fp)) error stop 8
    if (v_default%x /= 5 .or. associated(v_default%fp)) error stop 9
    if (c%y /= 2 .or. associated(c%hp)) error stop 10
    if (d%y /= 1 .or. associated(d%hp)) error stop 11
    call check_locals()
    d%hp => h
    if (.not. associated(d%hp, h)) error stop 12
    a%fp => g
    b = a
    if (b%fp(5) /= 10) error stop 13
    print *, "ok"
contains
    integer function h(i)
        integer, intent(in) :: i
        h = i + 3
    end function
end program
