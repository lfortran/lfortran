! Test labels on type declaration statements (Fortran 90+ allows labels on declarations)
program label_on_declaration_01
    implicit none
    
10  integer :: i
20  real :: x
30  character(len=10) :: str
40  logical :: flag
50  real, dimension(5) :: arr
    
    i = 42
    x = 3.14
    str = "test"
    flag = .true.
    arr = [1.0, 2.0, 3.0, 4.0, 5.0]
    
    if (i /= 42) error stop
    if (abs(x - 3.14) > 1e-6) error stop
    if (str /= "test") error stop
    if (.not. flag) error stop
    if (abs(arr(3) - 3.0) > 1e-6) error stop
    
end program label_on_declaration_01
