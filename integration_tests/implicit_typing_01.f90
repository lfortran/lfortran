program implicit_typing_01
    implicit double precision (a-h,o-z)
    dimension :: dv(0:100)
    integer :: i

    do i = 0,100
        dv(i) = i
    end do

    print *, dv(0:100)

    do i = 0,100
        if (abs(dv(i)-i) > 1e-10) error stop
    end do

end program
