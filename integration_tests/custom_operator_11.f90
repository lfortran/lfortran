module custom_operator_11_mod1
  
  interface operator(.in.)
     module procedure string_contains ! string in an integer array ??
  end interface operator(.in.)
  
contains

  logical function string_contains(search_string,array)
    character(*), intent(in) :: search_string
    integer,intent(in) :: array(:)
    character(len=10) :: string_buffer
    integer :: i

    string_contains = .false.
    do i = 1, size(array)
      write(string_buffer, '(i0)') array(i)
      string_contains = string_contains .or. (trim(string_buffer) == search_string)
    end do
  end function string_contains

end module custom_operator_11_mod1

module custom_operator_11_mod2
  use custom_operator_11_mod1, only: operator(.in.)
  implicit none
  interface operator(.in.)
     procedure target_in 
  end interface operator(.in.)
contains
  subroutine temp()
    integer, allocatable :: arr(:)
    character(len=:), allocatable :: str
    arr = [42,666]
    str = '42'
    if (.not. (str .in. arr)) error stop
  end subroutine temp

  function target_in(needle, arr) result(x)
    integer, intent(in) :: needle
    integer, intent(in) :: arr(:)
    logical :: x
    x = any(needle==arr)
  end function target_in
end module custom_operator_11_mod2
    
program custom_operator_11
  use custom_operator_11_mod2, only: operator(.in.),temp
  if (.not. (42 .in. [42,666])) error stop
  if ('22' .in. [42,666]) error stop
  if (0 .in. [42,666]) error stop
  call temp()
  
end program custom_operator_11