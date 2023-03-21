module module_1
    implicit none
    type t_1
        integer :: i
    end type t_1
contains
    function f_1(a) result(x)
        integer :: a
        type(t_1), allocatable :: x(:)
    end function f_1
end module module_1

program name
    use module_1
    implicit none
    type(t_1) :: z(2)
    z = f_1(1)
end program name
