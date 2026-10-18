! A procedure declaration statement without an interface, `procedure() :: f`,
! does not give a type, so a separate type declaration statement may give the
! type of `f`, before or after it, for a local or a dummy procedure.
program implicit_typing_15
    real :: y
    external sq
    real :: sq
    call apply_type_first(sq, 3.0, y)
    if (abs(y - 9.0) > 1e-6) error stop
    call apply_proc_first(sq, 2.0, y)
    if (abs(y - 4.0) > 1e-6) error stop
    call local_type_first()
    call local_proc_first(y)
    if (abs(y - 25.0) > 1e-6) error stop
    print *, y
end program

subroutine apply_type_first(f, x, y)
    real :: f
    procedure() :: f
    real :: x, y
    y = f(x)
end subroutine

subroutine apply_proc_first(f, x, y)
    procedure() :: f
    real :: f
    real :: x, y
    y = f(x)
end subroutine

subroutine local_type_first()
    real :: sq
    procedure() :: sq
end subroutine

subroutine local_proc_first(y)
    procedure() :: sq
    real :: sq
    real :: y
    y = sq(5.0)
end subroutine

real function sq(x)
    real :: x
    sq = x * x
end function
