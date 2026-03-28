module module_array_bound_6
    implicit none
    contains
    function make_bordered(n) result(a)
        integer, intent(in) :: n
        character(len=1), allocatable :: a(:,:)
        integer :: i, j
        allocate(a(0:n+1, 0:n+1))  ! border at 0 and n+1; interior: 1..n
        a = '.'
        print *, "Inside Function (Lbound, RBound, Size)", lbound(a), ubound(a), size(a)
        if ((lbound(a,1) /= 0) .and. ((lbound(a,2) /= 0))) error stop 
        if ((ubound(a,1) /= 6) .and. ((ubound(a,2) /= 6))) error stop
        if ((size(a,1) /= 7) .and. ((size(a,2) /= 7))) error stop
    end function
end module module_array_bound_6

program array_bound_6
    use module_array_bound_6
    implicit none
    integer, parameter :: N = 5
    character(len=1), allocatable :: a(:,:), b(:,:), c(:, :)
    allocate(b(0:N+1, 0:N+1))
     

    ! Test bounds for direct assignments
    c = b

    ! Test bounds for function call results
    a = make_bordered(N)


    print *, "Array a (Lbound, RBound, Size)", lbound(a), ubound(a), shape(a)
    if ((lbound(a,1) /= 1) .and. ((lbound(a,2) /= 1))) error stop 
    if ((ubound(a,1) /= 7) .and. ((ubound(a,2) /= 7))) error stop
    if ((size(a,1) /= 7) .and. ((size(a,2) /= 7))) error stop


    print *, "Array b (Lbound, RBound, Size)", lbound(b), ubound(b), shape(b)
    if ((lbound(b,1) /= 0) .and. ((lbound(b,2) /= 0))) error stop 
    if ((ubound(b,1) /= 6) .and. ((ubound(b,2) /= 6))) error stop
    if ((size(b,1) /= 7) .and. ((size(b,2) /= 7))) error stop

    print *, "Array c (Lbound, RBound, Size)", lbound(c), ubound(c), shape(c)
    if ((lbound(c,1) /= 0) .and. ((lbound(c,2) /= 0))) error stop 
    if ((ubound(c,1) /= 6) .and. ((ubound(c,2) /= 6))) error stop
    if ((size(c,1) /= 7) .and. ((size(c,2) /= 7))) error stop

end program array_bound_6

