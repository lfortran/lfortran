program implied_do_loops57
    ! An implied DO in an output list leaves its variable, and those of the
    ! implied DOs nested in it, at their final values, however the list is
    ! written: element by element, expanded at compile time (values of
    ! different types, trimmed strings) or through a temporary array.
    implicit none
    type :: ta
        integer :: i = 1
    end type
    type :: tb
        integer :: j = 2
    end type
    type(ta) :: x(3)
    type(tb) :: y(3)
    integer :: i, j, k(3), n, u
    real :: r(3)
    character(len=5) :: s(3) = ['ab   ', 'cde  ', 'f    ']
    character(len=200) :: line
    k = 7; r = 1.5; n = 3
    open(newunit=u, status='scratch')
    i = 99; print *, (k(i), i=1,3); call check(i, 4)
    i = 99; print '(3i2)', (k(i), i=1,3); call check(i, 4)
    i = 99; print *, (k(i), r(i), i=1,3); call check(i, 4)
    i = 99; print '(*(i2,f4.1))', (k(i), r(i), i=1,3); call check(i, 4)
    i = 99; print '(*(a,1x))', (trim(s(i)), i=1,3); call check(i, 4)
    i = 99; print *, (trim(s(i)), i=1,3); call check(i, 4)
    i = 99; write(line, '(*(a,1x))') (trim(s(i)), i=1,3); call check(i, 4)
    i = 99; write(line, *) (k(i), r(i), i=1,3); call check(i, 4)
    i = 99; write(u, *) (k(i), r(i), i=1,3); call check(i, 4)
    i = 99; write(u, '(*(i2,f4.1))') (k(i), r(i), i=1,3); call check(i, 4)
    i = 99; print '(*(i0,1x))', (x(i), y(i), i=1,3); call check(i, 4)
    i = 99; print *, (k(i), i=3,1,-1); call check(i, 0)
    i = 99; print *, (k(i), i=1,0); call check(i, 1)
    i = 99; print *, (k(i), i=1,n); call check(i, 4)
    i = 99; print '(*(a,1x))', (trim(s(i)), i=1,n); call check(i, 4)
    i = 99; j = 99; print *, ((k(i)+j, j=1,2), i=1,3)
    call check(i, 4); call check(j, 3)
    i = 99; j = 99; print '(*(a,1x))', ((trim(s(j)), j=1,i), i=1,3)
    call check(i, 4); call check(j, 4)
    i = 99; j = 99; print *, ((k(j), r(j), j=1,i), i=1,n)
    call check(i, 4); call check(j, 4)
    i = 99; j = 99; print *, ((k(j), j=1,0), i=1,2)
    call check(i, 3); call check(j, 1)
    i = 99; print *, (k(i), r(i), s(i), i=3,1,-2); call check(i, -1)
    i = 4; print *, (k(i), i=i-1,1,-2); call check(i, -1)
    i = 4; print *, (k(i), r(i), i=i-1,1,-2); call check(i, -1)
    i = 4; print '(*(a,1x))', (trim(s(i)), i=i-1,1,-1); call check(i, 0)
    close(u)
contains
    subroutine check(v, expected)
        integer, intent(in) :: v, expected
        if (v /= expected) error stop
    end subroutine
end program
