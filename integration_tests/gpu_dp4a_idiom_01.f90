module gpu_dp4a_idiom_01_mod
    use iso_fortran_env, only: int32
    implicit none
contains

    ! The portable form of a packed dot product of four signed bytes. GPU
    ! instruction/vector lowering must give the same result as the byte loop.
    pure elemental function dp4a(a, b, c) result(r)
        integer(int32), intent(in) :: a, b, c
        integer(int32) :: r
        r = c + shifta(shiftl(a, 24), 24) * shifta(shiftl(b, 24), 24) &
              + shifta(shiftl(a, 16), 24) * shifta(shiftl(b, 16), 24) &
              + shifta(shiftl(a,  8), 24) * shifta(shiftl(b,  8), 24) &
              + shifta(a, 24) * shifta(b, 24)
    end function

    ! Byte j (0 = least significant) of x, as a signed integer.
    pure function sbyte(x, j) result(r)
        integer(int32), intent(in) :: x
        integer, intent(in) :: j
        integer(int32) :: r
        r = ibits(x, 8*j, 8)
        if (r > 127) r = r - 256
    end function

    ! The byte loop the idiom stands for.
    pure function dp4a_ref(a, b, c) result(r)
        integer(int32), intent(in) :: a, b, c
        integer(int32) :: r
        integer :: j
        r = c
        do j = 0, 3
            r = r + sbyte(a, j) * sbyte(b, j)
        end do
    end function

end module

program gpu_dp4a_idiom_01
    use iso_fortran_env, only: int32
    use gpu_dp4a_idiom_01_mod
    implicit none
    integer, parameter :: n = 8
    integer(int32) :: a(n), b(n), c(n), ref(n), ref2(n)
    integer(int32) :: r1(n), r2(n), r3(n), r4(n), r5(n), r6(n), r7(n), r8(n)
    integer :: i

    ! Negative bytes, -128, 127, zero, and a mix.
    a = [int(z'01020304', int32), int(z'80FF017F', int32), int(z'80808080', int32), &
         int(z'7F7F7F7F', int32), int(z'FFFFFFFF', int32), 0_int32, &
         int(z'FE03FC05', int32), int(z'0080FF7F', int32)]
    b = [int(z'04030201', int32), int(z'7F0180FF', int32), int(z'80808080', int32), &
         int(z'7F7F7F7F', int32), int(z'01010101', int32), int(z'12345678', int32), &
         int(z'0201FF80', int32), int(z'80000001', int32)]
    c = [7_int32, -17_int32, -1_int32, 0_int32, 1000000_int32, -1000000_int32, &
         123_int32, -2_int32]
    do i = 1, n
        ref(i) = dp4a_ref(a(i), b(i), c(i))
        ref2(i) = dp4a_ref(a(i), b(i), dp4a_ref(a(i), b(i), 0_int32))
    end do

    do concurrent (i = 1:n)
        ! The function.
        r1(i) = dp4a(a(i), b(i), c(i))
        ! Nested: the accumulator is another dot product.
        r2(i) = dp4a(a(i), b(i), dp4a(a(i), b(i), 0_int32))
        ! The idiom written out in the loop, operands and additions in
        ! another order.
        r3(i) = shifta(a(i), 24) * shifta(b(i), 24) &
              + shifta(shiftl(b(i), 8), 24) * shifta(shiftl(a(i), 8), 24) &
              + c(i) &
              + shifta(shiftl(a(i), 16), 24) * shifta(shiftl(b(i), 16), 24) &
              + shifta(shiftl(a(i), 24), 24) * shifta(shiftl(b(i), 24), 24)
        ! The operands are expressions, the accumulator a sum, the last
        ! shift is by zero.
        r4(i) = (c(i) + 1) + &
              shifta(shiftl(ieor(a(i), 5), 24), 24) * shifta(shiftl(b(i) + 0, 24), 24) + &
              shifta(shiftl(ieor(a(i), 5), 16), 24) * shifta(shiftl(b(i) + 0, 16), 24) + &
              shifta(shiftl(ieor(a(i), 5),  8), 24) * shifta(shiftl(b(i) + 0,  8), 24) + &
              shifta(shiftl(ieor(a(i), 5),  0), 24) * shifta(shiftl(b(i) + 0,  0), 24) + (i - i)
        ! Not the idiom: byte 2 multiplies a by a, not a by b (a plain
        ! computation, still exact).
        r5(i) = c(i) + shifta(shiftl(a(i), 24), 24) * shifta(shiftl(b(i), 24), 24) &
              + shifta(shiftl(b(i), 16), 24) * shifta(shiftl(a(i), 16), 24) &
              + shifta(shiftl(a(i),  8), 24) * shifta(shiftl(a(i),  8), 24) &
              + shifta(a(i), 24) * shifta(b(i), 24)
        ! Not the idiom: byte 2 is missing and byte 1 twice.
        r6(i) = c(i) + shifta(shiftl(a(i), 24), 24) * shifta(shiftl(b(i), 24), 24) &
              + shifta(shiftl(a(i), 16), 24) * shifta(shiftl(b(i), 16), 24) &
              + shifta(shiftl(a(i), 16), 24) * shifta(shiftl(b(i), 16), 24) &
              + shifta(a(i), 24) * shifta(b(i), 24)
        r7(i) = dp4a(a(i), a(i), c(i))
        r8(i) = dp4a(b(i), a(i), 0_int32) - dp4a(a(i), b(i), 0_int32)
    end do

    do i = 1, n
        if (r1(i) /= ref(i)) error stop "dp4a function"
        if (r2(i) /= ref2(i)) error stop "dp4a nested"
        if (r3(i) /= ref(i)) error stop "dp4a inline, reordered"
        if (r4(i) /= dp4a_ref(ieor(a(i), 5_int32), b(i), c(i) + 1)) &
            error stop "dp4a inline, expressions"
        if (r5(i) /= c(i) + sbyte(a(i), 0) * sbyte(b(i), 0) &
                + sbyte(a(i), 1) * sbyte(b(i), 1) &
                + sbyte(a(i), 2) * sbyte(a(i), 2) &
                + sbyte(a(i), 3) * sbyte(b(i), 3)) error stop "mixed operands"
        if (r6(i) /= c(i) + sbyte(a(i), 0) * sbyte(b(i), 0) &
                + 2 * sbyte(a(i), 1) * sbyte(b(i), 1) &
                + sbyte(a(i), 3) * sbyte(b(i), 3)) error stop "repeated byte"
        if (r7(i) /= dp4a_ref(a(i), a(i), c(i))) error stop "dp4a a*a"
        if (r8(i) /= 0) error stop "dp4a commutes"
    end do
    print *, r1
end program
