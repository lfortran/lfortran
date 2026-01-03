program arrays_op_26
    implicit none
    type t
        real, pointer :: x(:)
        real, pointer :: y(:)
    end type t
    real, target :: X(5), Y(5)
    type(t) :: t_1
    t_1%x => X
    t_1%y => Y
    call sub(t_1)
    print '(f10.2)', X
    if( any(X /= 2679.00) ) error stop

contains

subroutine sub(t_1)
    type(t), intent(inout) :: t_1
    real, dimension(size(t_1%x)) :: S
    integer :: i
    S = 5
    t_1%x = 54
    t_1%y = 21
    do i = 1, size(t_1%y)
        t_1%x = t_1%x + t_1%y(i) * S**2
    end do
end subroutine sub

end program arrays_op_26
