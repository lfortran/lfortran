! Test for https://github.com/lfortran/lfortran/issues/5901
! Allocatable character with internal write in pure function, merge intrinsic
module integertostring_100
    implicit none
    integer :: n(2) = [42, -1]
contains
    pure function i0(n)
        character(:), allocatable :: i0
        integer, intent(in) :: n
        character(range(n)+2) :: result
        write(result, "(I0)") n
        i0 = trim(adjustl(result))
    end function i0
end module integertostring_100

program string_100
    use integertostring_100, only: i0, n
    implicit none
    character(:), allocatable :: string
    string = 'The answer is '//merge(i0(n(1)), i0(n(2)), n(1) > 0)//'.'
    print *, '"'//string//'"'
    if (string /= 'The answer is 42.') error stop
end program
