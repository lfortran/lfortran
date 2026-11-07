module gpu_guard_fold_02_m
    implicit none
contains
    pure elemental integer function byte_of(word, part)
        integer, intent(in) :: word, part
        if (part < 0 .or. part > 3) error stop "byte_of requires part in 0..3"
        byte_of = ibits(word, 8*part, 8)
    end function

    ! modulo(k - 1, 5) reaches 4, so the check of byte_of is kept and
    ! stops the program.
    pure subroutine decode(w, y)
        integer, intent(in) :: w(:)
        integer, intent(out) :: y(:)
        integer :: k
        do concurrent (k = 1:size(y))
            y(k) = byte_of(w((k - 1)/5 + 1), modulo(k - 1, 5))
        end do
    end subroutine
end module

program gpu_guard_fold_02
    use gpu_guard_fold_02_m, only: decode
    implicit none
    integer :: w(8), y(40)
    w = 16909060
    call decode(w, y)
    print *, "a part of 4 was decoded:", sum(y)
end program
