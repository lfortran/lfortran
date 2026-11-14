program transfer_pointer_01
    use iso_c_binding
    implicit none

    ! f90wrap recovers a derived-type pointer by transferring it
    ! through an integer(c_int) pair. Both words of the pointer have
    ! to be copied, and the recovered pointer has to be usable.
    type :: payload
        real(8), allocatable :: y(:)
    end type payload
    type :: wrap
        type(payload), pointer :: p
    end type wrap

    type(wrap) :: w, w2
    integer(c_int) :: bits(2)

    allocate(w%p)
    allocate(w%p%y(2))
    w%p%y(1) = 1.5d0
    w%p%y(2) = 2.5d0
    bits = transfer(w, bits)
    w2 = transfer(bits, w2)
    if (.not. associated(w2%p)) error stop "pointer lost"
    if (.not. allocated(w2%p%y)) error stop "allocatable lost"
    if (size(w2%p%y) /= 2) error stop "allocatable size"
    if (w2%p%y(1) /= 1.5d0) error stop "allocatable value"
    if (w2%p%y(2) /= 2.5d0) error stop "allocatable value 2"

    ! `w2%p` is the same target as `w%p`. Free that one target.
    deallocate(w%p%y)
    deallocate(w%p)

    print *, "ok"
end program transfer_pointer_01
