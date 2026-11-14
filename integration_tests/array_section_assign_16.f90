! A scalar assigned to a substring of each element of a character array
! section, `names(:)(2:3) = names(1)(1:2)`, is evaluated before the target
! is defined, as it may be a part of the target.
program array_section_assign_16
    implicit none
    character(len=4) :: names(3)
    character(len=2) :: c
    integer :: n

    names = ['abcd', 'efgh', 'ijkl']
    names(:)(2:3) = names(1)(1:2)
    if (any(names /= ['aabd', 'eabh', 'iabl'])) error stop 1

    names = ['abcd', 'efgh', 'ijkl']
    names(:)(1:2) = names(1)(2:3)
    if (any(names /= ['bccd', 'bcgh', 'bckl'])) error stop 2

    ! The last element is assigned first
    names = ['abcd', 'efgh', 'ijkl']
    names(3:1:-1)(1:3) = names(3)(2:4)
    if (any(names /= ['jkld', 'jklh', 'jkll'])) error stop 3

    ! The value is an expression, and the bounds are not constants
    n = 2
    names = ['abcd', 'efgh', 'ijkl']
    names(:n)(n:n + 1) = names(1)(n + 1:) // names(1)(:1)
    if (any(names /= ['acdd', 'ecdh', 'ijkl'])) error stop 4

    ! The value is padded or truncated to the length of the substring
    names = ['abcd', 'efgh', 'ijkl']
    names(:)(1:3) = names(1)(2:2)
    if (any(names /= ['b  d', 'b  h', 'b  l'])) error stop 5

    names = ['abcd', 'efgh', 'ijkl']
    names(:)(1:1) = names(1)
    if (any(names /= ['abcd', 'afgh', 'ajkl'])) error stop 6

    ! A value that is not a part of the target
    c = 'xy'
    names = ['abcd', 'efgh', 'ijkl']
    names(:)(3:4) = c
    if (any(names /= ['abxy', 'efxy', 'ijxy'])) error stop 7

    names(2:)(1:2) = 'pq'
    if (any(names /= ['abxy', 'pqxy', 'pqxy'])) error stop 8

    ! An empty section
    names(3:2)(1:2) = names(1)(3:4)
    if (any(names /= ['abxy', 'pqxy', 'pqxy'])) error stop 9

    call set(names, 2)
    if (any(names /= ['aaby', 'paby', 'paby'])) error stop 10

    print *, names
contains

    subroutine set(x, lo)
        character(len=*), intent(inout) :: x(:)
        integer, intent(in) :: lo
        x(:)(lo:lo + 1) = x(1)(:2)
    end subroutine

end program
