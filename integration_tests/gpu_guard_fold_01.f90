module gpu_guard_fold_01_m
    implicit none
contains
    ! The check holds for every call below, so no kernel can stop.
    pure elemental integer function byte_of(word, part)
        integer, intent(in) :: word, part
        if (part < 0 .or. part > 3) error stop "byte_of requires part in 0..3"
        byte_of = ibits(word, 8*part, 8)
    end function

    ! Its argument, and so the one it passes on, is known from its calls.
    pure integer function nibble_sum(word, half)
        integer, intent(in) :: word, half
        if (half /= 0 .and. half /= 1) error stop "nibble_sum requires half in 0..1"
        nibble_sum = byte_of(word, 2*half) + byte_of(word, 2*half + 1)
    end function

    pure subroutine decode(w, y)
        integer, intent(in) :: w(:)
        integer, intent(out) :: y(:)
        integer :: k
        do concurrent (k = 1:size(y))
            y(k) = byte_of(w((k - 1)/4 + 1), modulo(k - 1, 4))
        end do
    end subroutine

    pure subroutine halves(w, y)
        integer, intent(in) :: w(:)
        integer, intent(out) :: y(:)
        integer :: k
        do concurrent (k = 1:size(y))
            y(k) = nibble_sum(w((k - 1)/2 + 1), iand(k - 1, 1))
        end do
    end subroutine

    pure subroutine words(w, y)
        integer, intent(in) :: w(:)
        integer, intent(out) :: y(:)
        integer :: k, j, acc
        do concurrent (k = 1:size(y))
            acc = 0
            do j = 0, 3
                acc = acc + byte_of(w(k), j)
            end do
            y(k) = acc + byte_of(w(k), mod(iand(k, 1023), 4)) + byte_of(w(k), ibits(k, 3, 2))
        end do
    end subroutine
end module

program gpu_guard_fold_01
    use gpu_guard_fold_01_m, only: decode, halves, words
    implicit none
    integer, parameter :: n = 64
    integer :: w(n/4), y(n), z(n/2), v(n/4), k, j, expected
    do k = 1, n/4
        ! The bytes of word k are 4k-3, 4k-2, 4k-1 and 4k.
        w(k) = (4*k - 3) + 256*((4*k - 2) + 256*((4*k - 1) + 256*(4*k)))
    end do
    call decode(w, y)
    do k = 1, n
        if (y(k) /= k) error stop "decode"
    end do
    call halves(w, z)
    do k = 1, n/2
        if (z(k) /= 4*k - 1) error stop "halves"
    end do
    call words(w, v)
    do k = 1, n/4
        expected = 0
        do j = 0, 3
            expected = expected + ibits(w(k), 8*j, 8)
        end do
        expected = expected + ibits(w(k), 8*mod(k, 4), 8) &
            + ibits(w(k), 8*ibits(k, 3, 2), 8)
        if (v(k) /= expected) error stop "words"
    end do
    print *, "ok", sum(y), sum(z), sum(v)
end program
