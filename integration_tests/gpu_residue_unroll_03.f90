module gpu_residue_unroll_03_m
    implicit none
contains
    pure elemental integer function low_part(word, idx)
        integer, intent(in) :: word, idx
        if (idx > 2) error stop "low_part requires idx in 0..2"
        low_part = ibits(word, 8*idx, 8)
    end function

    ! The check of the function still stops the program once the
    ! residue it is called with is the constant 3.
    pure subroutine decode(w, y)
        integer, intent(in) :: w(:,:)
        integer, intent(out) :: y(:)
        do concurrent (integer :: n = 1:size(y))
            block
                integer :: d, acc
                acc = 0
                do d = 1, 4*size(w, 1)
                    acc = acc + low_part(w((d-1)/4+1, n), modulo(d-1, 4))
                end do
                y(n) = acc
            end block
        end do
    end subroutine
end module

program gpu_residue_unroll_03
    use gpu_residue_unroll_03_m, only: decode
    implicit none
    integer :: w(8, 16), y(16)
    w = 65793
    call decode(w, y)
    print *, "a residue of 3 was decoded:", y(1)
end program
