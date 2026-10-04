module c_f_pointer_02_mod
    use, intrinsic :: iso_c_binding, only: c_int
    implicit none
contains
    integer(c_int) function add_one(x) bind(c)
        integer(c_int), value :: x
        add_one = x + 1
    end function
end module

program c_f_pointer_02
    use, intrinsic :: iso_c_binding, only: c_ptr, c_funptr, c_int, &
        cloc => c_loc, cfunloc => c_funloc, cassoc => c_associated, &
        cfptr => c_f_pointer, cfprocptr => c_f_procpointer
    use c_f_pointer_02_mod, only: add_one
    implicit none
    abstract interface
        integer(c_int) function int_fn(x) bind(c)
            import :: c_int
            integer(c_int), value :: x
        end function
    end interface
    integer(c_int), target :: x = 42
    integer(c_int), target :: arr(2, 3)
    type(c_ptr) :: cp
    type(c_funptr) :: fp
    integer(c_int), pointer :: p
    integer(c_int), pointer :: parr(:, :)
    procedure(int_fn), pointer :: fn

    cp = cloc(x)
    if (.not. cassoc(cp)) error stop
    call cfptr(cp, p)
    if (p /= 42) error stop
    p = 7
    if (x /= 7) error stop

    arr = reshape([1, 2, 3, 4, 5, 6], [2, 3])
    cp = cloc(arr)
    call cfptr(cp, parr, [2, 3])
    if (size(parr, 1) /= 2 .or. size(parr, 2) /= 3) error stop
    if (parr(2, 3) /= 6) error stop
    call cfptr(fptr=parr, cptr=cp, shape=[3, 2])
    if (size(parr, 1) /= 3 .or. parr(3, 2) /= 6) error stop

    fp = cfunloc(add_one)
    call cfprocptr(fp, fn)
    if (fn(41) /= 42) error stop
    print *, "Ok"
end program
