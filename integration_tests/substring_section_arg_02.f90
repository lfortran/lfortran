program substring_section_arg_02
    implicit none
    character(len=4) :: e(6)
    integer :: n, k

    e = ['abcd', 'efgh', 'ijkl', 'mnop', 'qrst', 'uvwx']
    n = 5
    k = 3
    ! The copy back uses n and k as they were before the call
    call set(e(2:n:2)(2:k), n, k)
    print *, e
    if (any(e /= ['abcd', 'e##h', 'ijkl', 'm##p', 'qrst', 'uvwx'])) error stop 1
contains
    subroutine set(c, n, k)
        character(*), intent(inout) :: c(:)
        integer, intent(inout) :: n, k
        c = '##'
        n = 6
        k = 4
    end subroutine
end program
