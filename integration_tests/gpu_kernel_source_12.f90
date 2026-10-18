program gpu_kernel_source_12
    implicit none
    integer :: a(-2:4,3:7), b(3:7,-2:4), c(2,3,4), i, j, k, value

    do concurrent (j = 3:7, i = -2:4)
        a(i,j) = i + 10*j
    end do
    do j = 3, 7
        do i = -2, 4
            if (a(i,j) /= i + 10*j) error stop 1
        end do
    end do

    do concurrent (j = 3:7, i = -2:4)
        a(i,j) = i - j
        b(j,i) = i + j
    end do
    do j = 3, 7
        do i = -2, 4
            if (a(i,j) /= i - j) error stop 2
            if (b(j,i) /= i + j) error stop 3
        end do
    end do

    do concurrent (k = 1:4, j = 1:3, i = 1:2)
        c(i,j,k) = i + 2*j + 3*k
    end do
    do k = 1, 4
        do j = 1, 3
            do i = 1, 2
                if (c(i,j,k) /= i + 2*j + 3*k) error stop 4
            end do
        end do
    end do

    a = -99
    do concurrent (j = 7:3, i = -2:4)
        a(i,j) = 0
    end do
    do j = 3, 7
        do i = -2, 4
            if (a(i,j) /= -99) error stop 5
        end do
    end do

    do concurrent (j = 3:7, i = -2:4) local(value)
        value = 2*i - j
        a(i,j) = value
    end do
    do j = 3, 7
        do i = -2, 4
            if (a(i,j) /= 2*i - j) error stop 6
        end do
    end do

    do concurrent (j = 3:7, i = -2:4)
        a(i,j) = i + 2*j
        b(10-j,i) = i - j
    end do
    do j = 3, 7
        do i = -2, 4
            if (a(i,j) /= i + 2*j) error stop 7
            if (b(10-j,i) /= i - j) error stop 8
        end do
    end do
    print *, "ok"
end program
