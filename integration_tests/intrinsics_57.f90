program intrinsics_57
implicit none

character(len=:), allocatable :: shorts(:)
character(len=:), allocatable :: long
integer :: a(10), i, res(10)
logical :: m
do i = 1, 10
    a(i) = i
end do

allocate(character(len=6) :: shorts(10))
allocate(character(len=5) :: long)
shorts(1:10:2) = "shorts"
shorts(2:10:2) = "long"
long = "long"
print *, shorts
print *, long

print *, merge(1, a, shorts == long)
res = merge(2, a, shorts == long)
m = .false.
do i = 1, 10
    if( m ) then
        if( res(i) /= 2 ) error stop
    else
        if( res(i) /= a(i) ) error stop
    end if
    m = .not. m
end do

print *, merge(a, 0, shorts == long)
res = merge(a, 3, shorts == long)
m = .true.
do i = 1, 10
    if( m ) then
        if( res(i) /= 3 ) error stop
    else
        if( res(i) /= a(i) ) error stop
    end if
    m = .not. m
end do

print *, merge(1, 0, shorts == long)
res = merge(1, 0, shorts == long)
m = .false.
do i = 1, 10
    if( m ) then
        if( res(i) /= 1 ) error stop
    else
        if( res(i) /= 0 ) error stop
    end if
    m = .not. m
end do


end program
