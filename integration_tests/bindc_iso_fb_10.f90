program bindc_iso_fb_10
    use iso_c_binding, only: c_int
    implicit none
    interface
        function c_fill(p) bind(c) result(was_null)
            import :: c_int
            character(len=:), pointer, intent(inout) :: p(:)
            integer(c_int) :: was_null
        end function
    end interface
    character(len=:), pointer :: p(:)
    character(len=:), pointer :: q(:) => null()

    nullify(p)
    if (c_fill(p) /= 1) error stop 1
    call check(p)
    deallocate(p)

    p => null()
    if (c_fill(p) /= 1) error stop 2
    call check(p)
    deallocate(p)

    if (c_fill(q) /= 1) error stop 3
    call check(q)
    deallocate(q)
    print *, "ok"
contains
    subroutine check(x)
        character(len=:), pointer, intent(in) :: x(:)
        if (.not. associated(x)) error stop 10
        if (size(x) /= 3) error stop 11
        if (len(x) /= 2) error stop 12
        if (x(1) /= "ab" .or. x(2) /= "cd" .or. x(3) /= "ef") error stop 13
    end subroutine
end program
