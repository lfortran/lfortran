module string_21_fpm_string

integer, public, parameter :: tfc = selected_char_kind('DEFAULT')

contains

function f_string(c_string)

    use iso_c_binding
    character(len=1), intent(in) :: c_string(:)
    character(:), allocatable :: f_string
    integer :: i, n

    i = 0
    do while(c_string(i + 1) /= C_NULL_CHAR)
      i = i + 1
    end do
    n = i

    allocate(character(kind=tfc, len=n) :: f_string)
    do i = 1, n
      f_string(i:i) = c_string(i)
    end do

    print *, f_string

end function f_string

end module

program string_21
use string_21_fpm_string
use iso_c_binding
implicit none

character(len=1) :: c_string(6)
character(:), allocatable :: f_string_result

c_string(1) = 'H'
c_string(2) = 'e'
c_string(3) = 'l'
c_string(4) = 'l'
c_string(5) = 'o'
c_string(6) = C_NULL_CHAR

f_string_result = f_string(c_string)
print *, f_string_result
if( f_string_result /= "Hello" ) error stop

end program
