program type_name_01
    implicit none
    integer :: i
    real :: r
    real(8) :: d
    complex :: c
    logical :: l
    character(len=20) :: s
    integer, allocatable :: alloc_arr(:)
    integer :: fixed_arr(10)

    if (type_name(i) /= "integer(4)") error stop
    if (type_name(r) /= "real(4)") error stop
    if (type_name(d) /= "real(8)") error stop
    if (type_name(c) /= "complex(4)") error stop
    if (type_name(l) /= "logical(4)") error stop
    if (type_name(s) /= "character") error stop
    if (type_name(alloc_arr) /= "integer(4)") error stop
    if (type_name(fixed_arr) /= "integer(4)") error stop

    print *, type_name(i)
    print *, type_name(d)
end program
