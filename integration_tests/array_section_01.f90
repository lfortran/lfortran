subroutine vecfcn(l, m, n, o)
    implicit none
    integer, intent(in) :: l, m, n, o
    integer :: i, j, k, a, b, c, d
    real :: fvec(l), fvec2d(l, m), fvec3d(l, m, n), fvec4d(l, m, n, o)
    i = 5
    j = 5
    k = 5

    fvec(1) = 1
    fvec(2:l) = 0.0
    print *, fvec
    if (fvec(1) /= 1.0) error stop
    if (fvec(2) /= 0.0) error stop
    if (fvec(10) /= 0.0) error stop

    fvec2d = 1.0
    fvec2d(:, k) = 2.0
    print *, fvec2d
    do a = 1, l
        do b = 1, m
            if( b == k ) then
                if( fvec2d(a, b) /= 2.0 ) error stop
            else
                if( fvec2d(a, b) /= 1.0 ) error stop
            end if
        end do
    end do

    fvec3d = 1.0
    fvec3d(:, k, :i) = 2.0
    print *, fvec3d
    do a = 1, l
        do b = 1, m
            do c = 1, n
                if( b == 5 .and. c <= i ) then
                    if( fvec3d(a, b, c) /= 2.0 ) error stop
                else
                    if( fvec3d(a, b, c) /= 1.0 ) error stop
                end if
            end do
        end do
    end do

    fvec4d = 1.0
    fvec4d(:, k, :i, j) = 2.0
    print *, fvec4d
    do a = 1, l
        do b = 1, m
            do c = 1, n
                do d = 1, o
                    if( b == 5 .and. c <= i .and. d == j ) then
                        if( fvec4d(a, b, c, d) /= 2.0 ) error stop
                    else
                        if( fvec4d(a, b, c, d) /= 1.0 ) error stop
                    end if
                end do
            end do
        end do
    end do

end subroutine vecfcn

program main
    call vecfcn(10, 10, 10, 10)
end program main
