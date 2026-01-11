program functions_53
    implicit none
    type :: temp
      integer, allocatable :: arr(:)
    end type 
    type(temp), allocatable :: x
    x = parse_temp()
    if (.not. allocated(x)) error stop
contains
    function parse_temp() result(res)
          type(temp) :: res
          allocate(res%arr(2))
    end function
end program