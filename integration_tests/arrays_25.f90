program arrays_25
    implicit none
    type :: model_t
        integer, allocatable :: decoder_idx(:)
    end type

    type(model_t) :: m

    allocate(m%decoder_idx(10))

    m%decoder_idx(:) = 0
    m%decoder_idx(5) = 5

    print *, m%decoder_idx
    call decode(m%decoder_idx)

    contains

        subroutine decode(idx)
        integer, intent(in) :: idx(:)
        if (idx(4) /= 0) error stop
        if (idx(5) /= 5) error stop
        end subroutine

end program
