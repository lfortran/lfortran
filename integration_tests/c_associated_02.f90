module c_associated_02_mod
    use iso_c_binding, only: c_ptr, c_loc
    implicit none
    real, target :: g
contains
    function get_ptr() result(r)
        type(c_ptr) :: r
        r = c_loc(g)
    end function
end module

program c_associated_02
    ! c_associated with a c_ptr value (c_loc or a function result), not a
    ! c_ptr variable, as its first argument
    use iso_c_binding, only: c_ptr, c_loc, c_associated, c_int64_t
    use c_associated_02_mod, only: g, get_ptr
    implicit none
    real, target :: s, t, a(4)
    integer(c_int64_t), target :: k
    type(c_ptr) :: p, q

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

    ! the address of a zero-valued target is still not a null pointer
    if (.not. c_associated(c_loc(k))) error stop 11
    if (.not. c_associated(c_loc(a(1)))) error stop 12

    print *, "ok"
end program
