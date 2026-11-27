program array_bound_expr_01
    implicit none
    integer :: b(0:1, 4:6), i(-1:1, -1:1)
    if (any(lbound(2*b) /= 1)) error stop 1
    if (any(ubound(2*b) /= [2, 3])) error stop 2
    if (any(lbound(merge(i, i, .true.)) /= 1)) error stop 3
    if (any(ubound(merge(i, i, .true.)) /= [3, 3])) error stop 4
    if (lbound(2*b, 1) /= 1 .or. ubound(2*b, 2) /= 3) error stop 5
    if (any(lbound(b) /= [0, 4])) error stop 6
    if (any(ubound(b) /= [1, 6])) error stop 7
end program array_bound_expr_01
