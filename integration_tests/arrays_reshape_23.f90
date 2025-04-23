program arrays_reshape_23
    integer, parameter :: a_half_len = 2
    integer :: a(a_half_len*2)
    a = [2, 6, 8, 10]

    call f(a, a_half_len)

    contains

    subroutine f(a, half_len)
        implicit none
        integer, dimension(:) :: a
        integer, intent(in) :: half_len
        integer, dimension(:), allocatable :: sbuf

        allocate(sbuf(half_len))

        sbuf = reshape(a, [half_len*2])
        print *, "sbuf: ", sbuf
        if (any(a /= sbuf)) error stop

        deallocate(sbuf)
    end subroutine
end program arrays_reshape_23
