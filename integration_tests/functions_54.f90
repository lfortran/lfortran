module mod_functions_54
    implicit none

    type,public :: string
        character(len=:),allocatable :: str
    end type string

    contains 
    elemental function string_to_int(me) result(i)
        class(string),intent(in) :: me
        integer :: i
        i = len(me%str)
    end function string_to_int

    function split(str,token) result(vals)
        implicit none
        character(len=*),intent(in)  :: str
        character(len=*),intent(in)  :: token
        type(string),dimension(:),allocatable :: vals
        allocate(vals(1)) 
        vals(1)%str = str
    end function split

    function parse_nums64(line) result(ints)
        character(len=*),intent(in) :: line
        integer(4),dimension(:), allocatable :: ints 
        type(string),dimension(:),allocatable :: vals
        ints = string_to_int(split(line, ' '))
    end function parse_nums64
end module mod_functions_54

program functions_54 
  use mod_functions_54
  character(len=100) :: single_line
  integer(4), allocatable :: results(:)
  single_line = "10 20 30"
  results = parse_nums64(single_line)
  print *, results
  if (results(1) /= 100) error stop
end program functions_54