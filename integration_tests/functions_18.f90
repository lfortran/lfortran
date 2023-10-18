program functions_18
    implicit none
    type t
        integer, pointer :: x(:)
    end type t
    type(t) :: type_1
    integer, target :: x_global(3)
    x_global = 124
    allocate(type_1%x(3))
    type_1%x => x_global
    print*, sub(type_1%x)
contains
    function sub(x)
        integer, intent(in) :: x(:)
        integer :: sub(size(x)), i
        sub = x
        do i = 1, size(sub)
            if (sub(i) /= 124) error stop
        end do
    end function sub
end program
