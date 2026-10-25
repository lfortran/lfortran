module do_concurrent_22_values
    implicit none
    integer :: i__dc3 = 40
end module

program do_concurrent_22
    use do_concurrent_22_values
    implicit none
    integer :: i, j, a(3), b(2, 3), i__dc4
    i = 100
    j = 200
    i__dc4 = 50
    a = -1
    do concurrent (i = 1:3)
        block
            integer :: i__dc, i__dc1
            i__dc = 1
            i__dc1 = 2
            block
                integer :: i__dc2
                i__dc2 = 3
                a(i) = 10*i + i__dc + i__dc1 + i__dc2 + i__dc3 + i__dc4
            end block
        end block
    end do
    if (any(a /= [106, 116, 126])) error stop
    if (i /= 100 .or. i__dc3 /= 40 .or. i__dc4 /= 50) error stop
    call nested()
    if (i /= 100 .or. j /= 200) error stop
    if (any(b(1, :) /= [11, 12, 13])) error stop
    if (any(b(2, :) /= [21, 22, 23])) error stop
contains
    subroutine nested()
        do concurrent (i = 1:2)
            block
                integer :: j__dc
                j__dc = 1
                do concurrent (j = 1:3)
                    b(i, j) = 10*i + j*j__dc
                end do
            end block
        end do
    end subroutine
end program
