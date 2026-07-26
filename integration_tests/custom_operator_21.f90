module m_custom_operator_21_concat
    implicit none

    private

    public :: operator(//)

    interface operator(//)
        module procedure string_concat_i4
    end interface

contains

    function string_concat_i4(str1, i2) result(string)
        character(len=*), intent(in) :: str1
        integer,          intent(in) :: i2
        character(len=:), allocatable :: string

        character(len=32) :: buffer
        write(buffer, '(I0)') i2
        string = str1 // trim(buffer)
    end function
end module

! Re-export the operator through an intermediate module. Only the operator
! (not the specific procedure string_concat_i4) is made public here.
module m_custom_operator_21
    use m_custom_operator_21_concat
    implicit none

    private

    public :: operator(//)
end module

program custom_operator_21
    use m_custom_operator_21
    implicit none

    character(len=:), allocatable :: string

    string = 'coucou' // 1

    print *, string
    if (string /= 'coucou1') error stop
end program
