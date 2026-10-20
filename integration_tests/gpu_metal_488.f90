program gpu_metal_488
    ! More kernels than the runtime used to have room for: every
    ! `do concurrent` below is a kernel of its own, and each one has to be
    ! found when it is launched.
    implicit none
    integer, parameter :: n = 100
    integer :: a(n), i
    do i = 1, n
        a(i) = i
    end do
    do concurrent (i = 1:n)
        a(i) = a(i) + 1
    end do
    do concurrent (i = 1:n)
        a(i) = a(i) + 2
    end do
    do concurrent (i = 1:n)
        a(i) = a(i) + 3
    end do
    do concurrent (i = 1:n)
        a(i) = a(i) + 4
    end do
    do concurrent (i = 1:n)
        a(i) = a(i) + 5
    end do
    do concurrent (i = 1:n)
        a(i) = a(i) + 6
    end do
    do concurrent (i = 1:n)
        a(i) = a(i) + 7
    end do
    do concurrent (i = 1:n)
        a(i) = a(i) + 8
    end do
    do concurrent (i = 1:n)
        a(i) = a(i) + 9
    end do
    do concurrent (i = 1:n)
        a(i) = a(i) + 10
    end do
    do concurrent (i = 1:n)
        a(i) = a(i) + 11
    end do
    do concurrent (i = 1:n)
        a(i) = a(i) + 12
    end do
    do concurrent (i = 1:n)
        a(i) = a(i) + 13
    end do
    do concurrent (i = 1:n)
        a(i) = a(i) + 14
    end do
    do concurrent (i = 1:n)
        a(i) = a(i) + 15
    end do
    do concurrent (i = 1:n)
        a(i) = a(i) + 16
    end do
    do concurrent (i = 1:n)
        a(i) = a(i) + 17
    end do
    do concurrent (i = 1:n)
        a(i) = a(i) + 18
    end do
    do concurrent (i = 1:n)
        a(i) = a(i) + 19
    end do
    do concurrent (i = 1:n)
        a(i) = a(i) + 20
    end do
    do concurrent (i = 1:n)
        a(i) = a(i) + 21
    end do
    do concurrent (i = 1:n)
        a(i) = a(i) + 22
    end do
    do concurrent (i = 1:n)
        a(i) = a(i) + 23
    end do
    do concurrent (i = 1:n)
        a(i) = a(i) + 24
    end do
    do concurrent (i = 1:n)
        a(i) = a(i) + 25
    end do
    do concurrent (i = 1:n)
        a(i) = a(i) + 26
    end do
    do concurrent (i = 1:n)
        a(i) = a(i) + 27
    end do
    do concurrent (i = 1:n)
        a(i) = a(i) + 28
    end do
    do concurrent (i = 1:n)
        a(i) = a(i) + 29
    end do
    do concurrent (i = 1:n)
        a(i) = a(i) + 30
    end do
    do concurrent (i = 1:n)
        a(i) = a(i) + 31
    end do
    do concurrent (i = 1:n)
        a(i) = a(i) + 32
    end do
    do concurrent (i = 1:n)
        a(i) = a(i) + 33
    end do
    do concurrent (i = 1:n)
        a(i) = a(i) + 34
    end do
    do concurrent (i = 1:n)
        a(i) = a(i) + 35
    end do
    do concurrent (i = 1:n)
        a(i) = a(i) + 36
    end do
    do concurrent (i = 1:n)
        a(i) = a(i) + 37
    end do
    do concurrent (i = 1:n)
        a(i) = a(i) + 38
    end do
    do concurrent (i = 1:n)
        a(i) = a(i) + 39
    end do
    do concurrent (i = 1:n)
        a(i) = a(i) + 40
    end do
    do concurrent (i = 1:n)
        a(i) = a(i) + 41
    end do
    do concurrent (i = 1:n)
        a(i) = a(i) + 42
    end do
    do concurrent (i = 1:n)
        a(i) = a(i) + 43
    end do
    do concurrent (i = 1:n)
        a(i) = a(i) + 44
    end do
    do concurrent (i = 1:n)
        a(i) = a(i) + 45
    end do
    do concurrent (i = 1:n)
        a(i) = a(i) + 46
    end do
    do concurrent (i = 1:n)
        a(i) = a(i) + 47
    end do
    do concurrent (i = 1:n)
        a(i) = a(i) + 48
    end do
    do concurrent (i = 1:n)
        a(i) = a(i) + 49
    end do
    do concurrent (i = 1:n)
        a(i) = a(i) + 50
    end do
    do concurrent (i = 1:n)
        a(i) = a(i) + 51
    end do
    do concurrent (i = 1:n)
        a(i) = a(i) + 52
    end do
    do concurrent (i = 1:n)
        a(i) = a(i) + 53
    end do
    do concurrent (i = 1:n)
        a(i) = a(i) + 54
    end do
    do concurrent (i = 1:n)
        a(i) = a(i) + 55
    end do
    do concurrent (i = 1:n)
        a(i) = a(i) + 56
    end do
    do concurrent (i = 1:n)
        a(i) = a(i) + 57
    end do
    do concurrent (i = 1:n)
        a(i) = a(i) + 58
    end do
    do concurrent (i = 1:n)
        a(i) = a(i) + 59
    end do
    do concurrent (i = 1:n)
        a(i) = a(i) + 60
    end do
    do concurrent (i = 1:n)
        a(i) = a(i) + 61
    end do
    do concurrent (i = 1:n)
        a(i) = a(i) + 62
    end do
    do concurrent (i = 1:n)
        a(i) = a(i) + 63
    end do
    do concurrent (i = 1:n)
        a(i) = a(i) + 64
    end do
    do concurrent (i = 1:n)
        a(i) = a(i) + 65
    end do
    do concurrent (i = 1:n)
        a(i) = a(i) + 66
    end do
    do i = 1, n
        if (a(i) /= i + 66 * 67 / 2) error stop 1
    end do
    print *, "ok"
end program
