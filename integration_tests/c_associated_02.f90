module c_associated_02_mod
    use iso_c_binding, only: c_ptr, c_loc, c_funptr, c_funloc
    implicit none
    real, target :: g
contains
    function get_ptr() result(r)
        type(c_ptr) :: r
        r = c_loc(g)
    end function

    subroutine cb() bind(c)
    end subroutine

    subroutine cb2() bind(c)
    end subroutine

    function get_fptr() result(r)
        type(c_funptr) :: r
        r = c_funloc(cb)
    end function
end module

program c_associated_02
    ! c_associated with a c_ptr value (c_loc or a function result), not a
    ! c_ptr variable, as its first argument
    use iso_c_binding, only: c_ptr, c_loc, c_associated, c_int64_t, &
        c_funptr, c_funloc, c_intptr_t, c_null_ptr
    use c_associated_02_mod, only: g, get_ptr, cb, cb2, get_fptr
    implicit none
    real, target :: s, t, a(4)
    integer(c_int64_t), target :: k
    integer(c_intptr_t) :: addr
    type(c_ptr) :: p, q
    type(c_funptr) :: fp

    s = 1.5
    t = 2.5
    a = 0.0
    k = 0
    p = c_loc(s)
    q = c_loc(a(2))

    if (.not. c_associated(p, c_loc(s))) error stop 1
    if (.not. c_associated(c_loc(s), p)) error stop 2
    if (.not. c_associated(c_loc(s), c_loc(s))) error stop 3
    if (c_associated(c_loc(t), p)) error stop 4

    if (.not. c_associated(c_loc(a), c_loc(a(1)))) error stop 5
    if (.not. c_associated(c_loc(a(2)), q)) error stop 6
    if (c_associated(c_loc(a(3)), q)) error stop 7

    if (.not. c_associated(get_ptr(), c_loc(g))) error stop 8
    if (c_associated(get_ptr(), p)) error stop 9
    if (.not. c_associated(get_ptr())) error stop 10
    if (.not. c_associated(c_loc(g), get_ptr())) error stop 13
    if (c_associated(c_loc(t), get_ptr())) error stop 14

    ! c_funptr values: c_funloc and a c_funptr function result
    fp = c_funloc(cb)
    if (.not. c_associated(c_funloc(cb), fp)) error stop 15
    if (c_associated(c_funloc(cb2), fp)) error stop 16
    if (.not. c_associated(get_fptr(), fp)) error stop 17
    if (.not. c_associated(get_fptr(), c_funloc(cb))) error stop 18
    if (c_associated(get_fptr(), c_funloc(cb2))) error stop 19

    ! a c_ptr produced by transfer
    addr = transfer(p, addr)
    if (.not. c_associated(transfer(addr, c_null_ptr), p)) error stop 20
    if (c_associated(transfer(addr, c_null_ptr), q)) error stop 21

    ! the address of a zero-valued target is still not a null pointer
    if (.not. c_associated(c_loc(k))) error stop 11
    if (.not. c_associated(c_loc(a(1)))) error stop 12

    print *, "ok"
end program
