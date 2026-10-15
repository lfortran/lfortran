program data_28
    ! Large implied-DO DATA tables of procedure and BLOCK locals, and arrays
    ! of each kind initialized by several DATA statements
    implicit none
    integer :: i
    do i = 1, 3
        call tables(i)
        call kinds(i)
    end do
contains
    subroutine tables(call_number)
        integer, intent(in) :: call_number
        integer :: a(20000), b(100, 200), c(3), i, j
        real :: x(20000)
        data (a(i), i=1,20000) /10000*1, 10000*2/
        data ((b(i, j), i=1,100), j=1,200) /20000*3/
        data (c(i), x(i), i=1,3) /4, 1.5, 5, 2.5, 6, 3.5/
        if (a(1) /= call_number .or. sum(a(2:)) /= 29999) error stop "a"
        if (b(1, 1) /= 2 + call_number .or. sum(b) /= 59999 + call_number) &
            error stop "b"
        if (any(c /= [4, 5, 6])) error stop "c"
        if (any(x(1:4) /= [1.5, 2.5, 3.5, 0.0])) error stop "x"
        a(1) = a(1) + 1
        b(1, 1) = b(1, 1) + 1
        block
            integer :: d(5000), k
            data (d(k), k=1,5000) /2500*7, 2500*8/
            if (d(1) /= 6 + call_number .or. sum(d(2:)) /= 37493) &
                error stop "d"
            d(1) = d(1) + 1
        end block
    end subroutine

    ! Elements of arrays of each kind set by several DATA statements
    subroutine kinds(n)
        integer, intent(in) :: n
        integer :: k
        integer(2) :: i2(5)
        integer(8) :: i8(2, 3)
        real :: r4(4)
        real(8) :: r8(4)
        complex :: c4(3)
        complex(8) :: c8(3)
        logical :: l(4)
        logical(1) :: l1(3)
        data i2(1) /1/, i2(2) /2/
        data (i2(k), k=4,5) /4, 5/
        data i8(1, 1) /11_8/, i8(2, 3) /23_8/
        data (i8(k, 1), k=2,2) /21_8/
        data r4(2) /2.5/, (r4(k), k=3,4) /3.5, 4.5/
        data r8(1) /1.25d0/, r8(4) /4.25d0/
        data c4(1) /(1.0, 1.0)/, c4(3) /(3.0, 3.0)/
        data c8(2) /(2.0d0, -2.0d0)/, (c8(k), k=3,3) /(3.0d0, -3.0d0)/
        data l(1) /.true./, l(3) /.true./
        data l1(2) /.true./
        if (any(i2 /= [1, 2, 0, 4, 5] + n - 1)) error stop "i2"
        if (any(i8(:, 1) /= [11, 21] + n - 1) .or. i8(2, 3) /= 22 + n &
            .or. i8(1, 3) /= n - 1) error stop "i8"
        if (any(r4 /= [0.0, 2.5, 3.5, 4.5] + n - 1)) error stop "r4"
        if (any(r8 /= [1.25d0, 0.0d0, 0.0d0, 4.25d0] + n - 1)) error stop "r8"
        if (any(c4 /= [cmplx(n, 1), cmplx(n - 1, 0), cmplx(2 + n, 3)])) &
            error stop "c4"
        if (any(c8 /= [cmplx(n - 1, 0, 8), cmplx(1 + n, -2, 8), &
            cmplx(2 + n, -3, 8)])) error stop "c8"
        if (any(l .neqv. [.true., .false., .true., .false.] .neqv. &
            mod(n, 2) == 0)) error stop "l"
        if (any(l1 .neqv. [.false., .true., .false.] .neqv. mod(n, 2) == 0)) &
            error stop "l1"
        i2 = i2 + 1; i8 = i8 + 1; r4 = r4 + 1; r8 = r8 + 1
        c4 = c4 + 1; c8 = c8 + 1; l = .not. l; l1 = .not. l1
    end subroutine
end program
