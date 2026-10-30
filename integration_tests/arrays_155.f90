module constructor_controls_once
    implicit none
    integer :: starts, ends, steps
contains
    integer(8) function first()
        starts = starts + 1
        first = 9_8
    end function
    integer(2) function last()
        ends = ends + 1
        last = 1_2
    end function
    integer(8) function stride()
        steps = steps + 1
        stride = -2_8
    end function
end module

program arrays_155
    use constructor_controls_once
    implicit none
    integer :: i, a(5), k, n
    real(8) :: r(5)
    complex(8) :: z(5)
    logical :: flags(5)
    starts = 0
    ends = 0
    steps = 0
    do k = 1, 3
        a = [(i, i=first(),last(),stride())]
        if (starts /= k .or. ends /= k .or. steps /= k) error stop 1
        if (any(a /= [9,7,5,3,1])) error stop 2
    end do
    if (any([(i,i=first(),last(),stride())] /= [9,7,5,3,1])) then
        error stop 3
    end if
    if (starts /= 4 .or. ends /= 4 .or. steps /= 4) error stop 4
    n = 5
    r = [(real(i,8),i=1,n)]
    z = [(cmplx(i,-i,8),i=1,n)]
    flags = [(mod(i,2) == 0,i=1,n)]
    if (r(1) /= 1.0_8 .or. r(5) /= 5.0_8) error stop 5
    if (z(1) /= (1.0_8,-1.0_8) .or. z(5) /= (5.0_8,-5.0_8)) error stop 6
    if (any(flags .neqv. [.false.,.true.,.false.,.true.,.false.])) error stop 7
end program
