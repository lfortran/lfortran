! SIZE with a DIM other than the last of an assumed-size array
module array_size_assumed_size_01_m
    implicit none
    type kpt_t
        integer :: ngwk
    end type
    type(kpt_t) :: nkpt
    type pt_t
        integer :: a
    end type
contains
    subroutine rank3(nstate, c2, r1, r2)
        integer, intent(in) :: nstate
        complex(8), intent(in) :: c2(nkpt%ngwk, nstate, *)
        integer, intent(out) :: r1, r2
        r1 = size(c2, 1)
        r2 = size(c2, 2)
    end subroutine

    subroutine chr(n, c, u, r1, r2, r3)
        integer, intent(in) :: n
        character(len=*), intent(in) :: c(n,*)
        character(len=3), intent(in) :: u(2:n+1,*)
        integer, intent(out) :: r1, r2
        integer(8), intent(out) :: r3
        r1 = size(c, 1)
        r2 = size(u, 1)
        r3 = size(u, 1, kind=8)
    end subroutine

    integer function dt(n, x) result(r)
        integer, intent(in) :: n
        type(pt_t), intent(in) :: x(n,*)
        r = size(x, 1)
    end function

    function fres(n, c) result(r)
        integer, intent(in) :: n
        real, intent(in) :: c(n,*)
        real :: r(size(c, 1))
        r = 2
    end function

    subroutine bc(n, m, c, r1, r2) bind(c)
        use iso_c_binding, only: c_int, c_float
        integer(c_int), value :: n, m
        real(c_float), intent(in) :: c(n,m,*)
        integer(c_int), intent(out) :: r1, r2
        integer :: k
        k = 2
        r1 = size(c, 1)
        r2 = size(c, k)
    end subroutine
end module

program array_size_assumed_size_01
    use array_size_assumed_size_01_m, only: nkpt, rank3, chr, dt, fres, pt_t, bc
    implicit none
    real :: a(3,4), b(3), a3(3,4,2)
    complex(8) :: z(4,3,2)
    character(len=3) :: ch(3,4)
    type(pt_t) :: x(3,2)
    integer :: r1, r2
    integer(8) :: r3
    a = 1
    call s(3, a, 1)
    call t(3_8, a)
    call lb(3, a)
    z = 0
    nkpt%ngwk = 4
    call rank3(3, z, r1, r2)
    print *, r1, r2
    if (r1 /= 4 .or. r2 /= 3) error stop
    ch = 'abc'
    call chr(3, ch, ch, r1, r2, r3)
    print *, r1, r2, r3
    if (r1 /= 3 .or. r2 /= 3 .or. r3 /= 3_8) error stop
    x%a = 1
    print *, dt(3, x)
    if (dt(3, x) /= 3) error stop
    print *, size(fres(3, a)), sum(fres(3, a))
    if (size(fres(3, a)) /= 3) error stop
    b = fres(3, a)
    if (any(b /= 2)) error stop
    a3 = 0
    call bc(3, 4, a3, r1, r2)
    print *, r1, r2
    if (r1 /= 3 .or. r2 /= 4) error stop
contains
    subroutine s(n, c, k)
        integer, intent(in) :: n, k
        real, intent(in) :: c(n,*)
        real :: tmp(size(c, 1))
        print *, size(c, 1), size(c, dim=1), size(c, k), size(tmp)
        if (size(c, 1) /= 3) error stop
        if (size(c, dim=1) /= 3) error stop
        if (size(c, k) /= 3) error stop
        if (size(tmp) /= 3) error stop
        if (kind(size(c, 1, kind=8)) /= 8) error stop
        if (size(c, 1, kind=8) /= 3_8) error stop
        if (2*size(c, 1) /= 6) error stop
    end subroutine

    subroutine t(n, c)
        integer(8), intent(in) :: n
        real, intent(in) :: c(n,*)
        print *, size(c, 1)
        if (size(c, 1) /= 3) error stop
    end subroutine

    subroutine lb(n, c)
        integer, intent(in) :: n
        real, intent(in) :: c(0:n-1,*)
        print *, size(c, 1)
        if (size(c, 1) /= 3) error stop
    end subroutine
end program
