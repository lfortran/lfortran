module lincoa_mod
    implicit none

    contains

    subroutine get_lincon(amat)
        implicit none
        real, intent(out), allocatable :: amat(:, :)
        integer :: m, n
        m = 3
        n = 2
        allocate(amat(m, n))
        amat = reshape([1., 2., 3., 4., 5., 6.], shape(amat))
    end subroutine get_lincon

    subroutine get_lincon1(amat)
        implicit none
        real, intent(out), allocatable :: amat(:, :)
        integer :: m, n
        m = 3
        n = 2
        amat = reshape([4., 5., 6., 1., 2., 3.], [m, n])
    end subroutine get_lincon1
end module lincoa_mod

program arrays_reshape_23
    use lincoa_mod
    implicit none
    real, allocatable :: a(:, :), b(:, :)

    call get_lincon(a)
    print *, a
    if( any(a(:, 1) /= [1.0, 2.0, 3.0]) ) error stop
    if( any(a(:, 2) /= [4.0, 5.0, 6.0]) ) error stop

    call get_lincon1(b)
    print *, b
    if( any(b(:, 1) /= [4.0, 5.0, 6.0]) ) error stop
    if( any(b(:, 2) /= [1.0, 2.0, 3.0]) ) error stop
end program arrays_reshape_23
