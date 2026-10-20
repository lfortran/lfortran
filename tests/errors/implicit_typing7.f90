! A procedure declaration statement without an interface, `procedure() :: f`,
! gives no type, so one type declaration statement may give the type of `f`,
! before or after it. A second type declaration statement must still be
! rejected, for a dummy or a local procedure.
subroutine dummy_proc_first(f)
    procedure() :: f
    real :: f
    integer :: f
end subroutine

subroutine dummy_type_first(f)
    real :: f
    procedure() :: f
    integer :: f
end subroutine

subroutine local_proc_first()
    procedure() :: f
    real :: f
    integer :: f
end subroutine

subroutine local_type_first()
    real :: f
    procedure() :: f
    integer :: f
end subroutine
