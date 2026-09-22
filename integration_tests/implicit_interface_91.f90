! Function references and calls through implicit interfaces whose actual
! arguments are an ASSOCIATE name, a pointer and an allocatable scalar: the
! procedure receives the target.
program implicit_interface_91
    implicit none
    integer, external :: ii91_f
    character(len=3), external :: ii91_c
    external :: ii91_s
    integer, target :: k
    integer, pointer :: p
    integer, allocatable :: a
    integer :: r
    k = 2
    p => k
    allocate(a)
    a = 3
    associate (j => k)
        if (ii91_f(j) /= 20) error stop 1
        if (ii91_c(j) /= "x2 ") error stop 2
        call ii91_s(j, r)
        if (r /= 20) error stop 3
    end associate
    if (ii91_f(p) /= 20) error stop 4
    if (ii91_f(a) /= 30) error stop 5
    call ii91_s(a, r)
    if (r /= 30) error stop 6
    print *, ii91_f(k), ii91_c(k)
end program

integer function ii91_f(i)
    integer, intent(in) :: i
    ii91_f = 10*i
end function

character(len=3) function ii91_c(i)
    integer, intent(in) :: i
    write(ii91_c, '(a,i1)') "x", i
end function

subroutine ii91_s(i, r)
    integer, intent(in) :: i
    integer, intent(out) :: r
    r = 10*i
end subroutine
