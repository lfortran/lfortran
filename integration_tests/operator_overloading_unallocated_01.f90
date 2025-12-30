module m_mod

    implicit none

    interface operator(.in.)

        module procedure in_array

    end interface

contains

    logical function in_array(str, arr) result(res)

        character(len=*), intent(in) :: str

        character(len=*), intent(in) :: arr(:)

        res = .false.

    end function in_array

end module m_mod

program demo_in_operator

    use m_mod

    implicit none

    character(len=:), allocatable :: s

    character(len=100), allocatable :: modules_provided(:)

    s = 'mod_a'

    if (s .in. modules_provided) error stop

    print *, "Test passed: unallocated array handled gracefully in operator overload"

end program demo_in_operator


