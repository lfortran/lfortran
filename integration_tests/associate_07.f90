program associate_07
    integer, pointer :: p1 => null()
    real(8), pointer :: p2(:, :) => null()
    complex(4), pointer :: p3(:, :) => null()
    integer, target :: t1 = 2
    real(8), target :: t2(2, 3)
    complex(4), target :: t3(2, 4)

    p1 => t1
    p2 => t2

    if( .not. associated(p1) ) error stop
    if( .not. associated(p2) ) error stop
    if( .not. associated(p1, t1) ) error stop
    if( .not. associated(p2, t2) ) error stop
    if( associated(p3) ) error stop
    if( associated(p3, t3) ) error stop

end program
