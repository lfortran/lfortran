! Initial targets that a derived type gives its pointer components, for
! module variables declared in a module other than the one that defines the
! type, each module compiled on its own. The variables' module initializer
! associates each pointer, so it names every target through a symbol of its
! own: an array element, also at a named constant's index, a component, an
! element's component, a substring, a variable and a procedure that the
! type's module itself uses from another module, one of them renamed, and
! external procedures through interfaces, one of them bind(c). The variables'
! module declares its own `arr` and `c_twice`, which the targets must not
! resolve to, and two components name the same target.
program global_init_13
    use global_init_13_a, only: t, arr, str, dt, dts
    use global_init_13_c, only: c_tgt, c_arr
    use global_init_13_b, only: obj, objs, b_arr => arr, b_twice => c_twice
    implicit none
    integer :: i
    call check(obj, 0)
    do i = 1, 2
        call check(objs(i), i)
    end do
    ! Each pointer aliases its target.
    obj%p_elem = 120
    if (arr(2) /= 120 .or. objs(1)%p_again /= 120) error stop 30
    obj%p_elem_comp = 210
    if (dts(2)%v /= 210) error stop 31
    obj%p_sub = "XYZ"
    if (str /= "aXYZe" .or. objs(2)%p_sub /= "XYZ") error stop 32
    obj%p_ren_elem = 330
    if (c_arr(3) /= 330) error stop 33
    ! The variables' module keeps its own entities of the same names.
    if (any(b_arr /= [91, 92, 93])) error stop 34
    if (b_twice(5) /= -5) error stop 35
    print *, "ok"
contains
    subroutine check(x, n)
        type(t), intent(in) :: x
        integer, intent(in) :: n
        if (.not. associated(x%p_elem, arr(2))) call fail(10, n)
        if (.not. associated(x%p_elem_k, arr(3))) call fail(11, n)
        if (.not. associated(x%p_again, arr(2))) call fail(12, n)
        if (.not. associated(x%p_comp, dt%v)) call fail(13, n)
        if (.not. associated(x%p_comp_elem, dt%w(2))) call fail(14, n)
        if (.not. associated(x%p_elem_comp, dts(2)%v)) call fail(15, n)
        if (.not. associated(x%p_sub)) call fail(16, n)
        if (len(x%p_sub) /= 3 .or. x%p_sub /= str(2:4)) call fail(17, n)
        if (.not. associated(x%p_c, c_tgt)) call fail(18, n)
        if (.not. associated(x%p_ren_elem, c_arr(3))) call fail(19, n)
        if (x%fp_c(5) /= 10) call fail(20, n)
        if (x%fp_ext(5) /= 15) call fail(21, n)
        if (x%fp_bindc(5) /= 20) call fail(22, n)
    end subroutine check

    subroutine fail(code, n)
        integer, intent(in) :: code, n
        print *, "check", code, "failed for variable", n
        error stop 1
    end subroutine fail
end program global_init_13
