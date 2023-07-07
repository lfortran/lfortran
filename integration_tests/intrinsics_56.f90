program intrinsics_56
implicit none

logical :: m1(5, 10), m2(5, 10), m

integer :: a1(5, 10), b1(5, 10)
integer, allocatable :: a2(:, :)

real :: a4(5, 10), b2(5, 10)
real, allocatable :: a3(:, :)

complex, allocatable :: a5(:, :), a6(:, :), b3(:, :)

character(len=5) :: a7(5, 10)
character(len=5), allocatable :: a8(:, :)
character(len=5) :: b4(5, 10)
integer :: i, j

m = .true.
do i = 1, 5
    do j = 1, 10
        m1(i, j) = m
        m2(i, j) = .not. m
        m = .not. m
    end do
end do

allocate(a2(5, 10))
a1 = 1
a2 = 2
b1 = merge(a1, a2, m1)
m = .true.
do i = 1, 5
    do j = 1, 10
        print *, b1(i, j)
        if( m ) then
            if( b1(i, j) /= a1(i, j) ) error stop
        else
            if( b1(i, j) /= a2(i, j) ) error stop
        end if
        m = .not. m
    end do
end do

allocate(a3(5, 10))
a3 = 3.0
a4 = 4.0
b2 = merge(a3, a4, m2)
m = .false.
do i = 1, 5
    do j = 1, 10
        print *, b2(i, j)
        if( m ) then
            if( b2(i, j) /= a3(i, j) ) error stop
        else
            if( b2(i, j) /= a4(i, j) ) error stop
        end if
        m = .not. m
    end do
end do

allocate(a5(5, 10), a6(5, 10), b3(5, 10))
a5 = (5.0, 6.0)
a6 = (7.0, 8.0)
b3 = merge(a5, a6, m1)
m = .true.
do i = 1, 5
    do j = 1, 10
        print *, b3(i, j)
        if( m ) then
            if( b3(i, j) /= a5(i, j) ) error stop
        else
            if( b3(i, j) /= a6(i, j) ) error stop
        end if
        m = .not. m
    end do
end do

allocate(a8(5, 10))
a7 = "a7"
a8 = "a8"
b4 = merge(a7, a8, m2)
m = .false.
do i = 1, 5
    do j = 1, 10
        print *, b4(i, j)
        if( m ) then
            if( b4(i, j) /= a7(i, j) ) error stop
        else
            if( b4(i, j) /= a8(i, j) ) error stop
        end if
        m = .not. m
    end do
end do

end program
