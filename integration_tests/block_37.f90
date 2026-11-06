program block_37
    ! Implied-do array constructors in the initializer of BLOCK-local
    ! named constants are compile-time folded (#16160).
    implicit none
    integer :: k
    block
        integer :: j
        integer, parameter :: v(3) = [(j, j=1,3)]
        print *, v
        if (any(v /= [1, 2, 3])) error stop
        if (size(v) /= 3) error stop
    end block
    block
        integer, parameter :: w(3) = [(10*k, k=1,3)]
        integer :: a(w(3))
        print *, w
        if (any(w /= [10, 20, 30])) error stop
        if (size(a) /= 30) error stop
    end block
    block
        integer :: j
        real, parameter :: r(2) = [(real(j)/2, j=1,2)]
        print *, r
        if (any(abs(r - [0.5, 1.0]) > 1e-6)) error stop
    end block
end program
