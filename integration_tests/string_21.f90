module fpm_string

integer, public, parameter :: tfc = selected_char_kind('DEFAULT')

contains

function f_string(c_string)
    use iso_c_binding
    character(len=1), intent(in) :: c_string(:)
    character(:), allocatable :: f_string

    integer :: i, n

    i = 0
    do while(c_string(i+1) /= C_NULL_CHAR)
      i = i + 1
    end do
    n = i

    allocate(character(kind=tfc, len=n) :: f_string)
    do i=1,n
      f_string(i:i) = c_string(i)
    end do

end function f_string

end module
