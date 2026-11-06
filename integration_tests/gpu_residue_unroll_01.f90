module gpu_residue_unroll_01_m
    use iso_fortran_env, only: int16, int32, real32
    implicit none
contains
    pure elemental function packed_byte(word, byte_index) result(value)
        integer(int32), intent(in) :: word
        integer, intent(in) :: byte_index
        integer(int32) :: value
        if (byte_index < 0 .or. byte_index > 3) &
            error stop "packed_byte requires byte_index in 0..3"
        value = ibits(word, 8*byte_index, 8)
    end function

    ! The decode of a q8 matrix-vector product: a block-private map of
    ! codes, four per packed word, read by the sums of groups of 64.
    pure subroutine project(w, x, y)
        integer(int32), contiguous, intent(in) :: w(:,:)
        real(real32), contiguous, intent(in) :: x(:)
        real(real32), contiguous, intent(out) :: y(:)
        integer :: k, groups
        k = size(x)
        groups = k/64
        do concurrent (integer :: n = 1:size(y))
            block
                integer(int16) :: codes(k)
                real(real32) :: terms(groups)
                do concurrent (integer :: d = 1:k)
                    codes(d) = int(packed_byte(w((d-1)/4+1, n), &
                        modulo(d-1, 4)), int16)
                end do
                do concurrent (integer :: g = 1:groups)
                    terms(g) = sum(real(codes((g-1)*64+1:g*64), real32) &
                        *x((g-1)*64+1:g*64))
                end do
                y(n) = sum(terms)
            end block
        end do
    end subroutine

    ! The same codes summed in groups of 8, with the loop variable read
    ! after each group's loop.
    pure subroutine groups_of_eight(w, y, last)
        integer(int32), contiguous, intent(in) :: w(:,:)
        integer, contiguous, intent(out) :: y(:), last(:)
        do concurrent (integer :: n = 1:size(y))
            block
                integer :: d, g, acc, final
                acc = 0
                final = 0
                do g = 1, size(w, 1)/2
                    do d = (g-1)*8+1, g*8
                        acc = acc + packed_byte(w((d-1)/4+1, n), &
                            modulo(d-1, 4))*g
                    end do
                    final = final + d
                end do
                y(n) = acc
                last(n) = final
            end block
        end do
    end subroutine

    subroutine check(k, outputs)
        integer, intent(in) :: k, outputs
        integer(int32) :: w(k/4, outputs)
        real(real32) :: x(k), y(outputs), expected
        integer :: z(outputs), last(outputs), i, n, d, b0, b1, b2, b3, &
            ez, el, g
        do n = 1, outputs
            do i = 1, k/4
                b0 = mod(i*7 + n*3, 256)
                b1 = mod(i*11 + n*5 + 1, 256)
                b2 = mod(i*13 + n, 256)
                b3 = mod(i*3 + n*17, 128)
                w(i, n) = b0 + 256*b1 + 65536*b2 + 16777216*b3
            end do
        end do
        do d = 1, k
            x(d) = real(mod(d*5, 7) - 3, real32)
        end do
        call project(w, x, y)
        do n = 1, outputs
            expected = 0
            do d = 1, k
                expected = expected + real(ibits(w((d-1)/4+1, n), &
                    8*mod(d-1, 4), 8), real32)*x(d)
            end do
            if (y(n) /= expected) error stop "project"
        end do
        call groups_of_eight(w, z, last)
        do n = 1, outputs
            ez = 0
            el = 0
            do g = 1, k/8
                do d = (g-1)*8+1, g*8
                    ez = ez + ibits(w((d-1)/4+1, n), 8*mod(d-1, 4), 8)*g
                end do
                el = el + g*8 + 1
            end do
            if (z(n) /= ez) error stop "groups_of_eight"
            if (last(n) /= el) error stop "loop variable after the groups"
        end do
    end subroutine
end module

program gpu_residue_unroll_01
    use gpu_residue_unroll_01_m, only: check
    implicit none
    call check(64, 3)
    call check(256, 37)
    call check(1024, 300)
    print *, "ok"
end program
