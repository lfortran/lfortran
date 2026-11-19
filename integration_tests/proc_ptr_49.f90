! Calls through a procedure pointer component of an object that is itself a
! component, as `call o%in%p(x)`: the callee is `p`, not the first component of
! `in`, and a passed-object dummy receives `o%in`, not `o`.
module proc_ptr_49_m
    implicit none

    abstract interface
        subroutine iface(x)
            real, intent(inout) :: x
        end subroutine
        real function fface(x)
            real, intent(in) :: x
        end function
    end interface

    type :: inner_t
        integer :: n = 7
        procedure(fface), pointer, nopass :: f => null()
        procedure(iface), pointer, nopass :: p => null()
        procedure(add_n), pointer, pass :: ps => null()
        procedure(sub_n), pointer, pass(self) :: pf => null()
    end type inner_t

    type :: outer_t
        integer :: k = 100
        type(inner_t) :: in
        type(inner_t) :: arr(2)
        type(inner_t), allocatable :: al
        type(inner_t), pointer :: pt => null()
    end type outer_t

    type :: top_t
        type(outer_t) :: o
    end type top_t

contains

    subroutine double_it(x)
        real, intent(inout) :: x
        x = 2.0*x
    end subroutine double_it

    real function triple(x)
        real, intent(in) :: x
        triple = 3.0*x
    end function triple

    subroutine add_n(self, x)
        class(inner_t), intent(inout) :: self
        real, intent(inout) :: x
        x = x + self%n
    end subroutine add_n

    real function sub_n(y, self)
        real, intent(in) :: y
        class(inner_t), intent(in) :: self
        sub_n = y - self%n
    end function sub_n

end module proc_ptr_49_m

program proc_ptr_49
    use proc_ptr_49_m
    implicit none
    type(outer_t), target :: o
    type(top_t) :: t
    type(inner_t), target :: tg
    type(inner_t) :: i
    real :: x

    i%p => double_it
    x = 1.5
    call i%p(x)
    if (x /= 3.0) error stop

    ! The first component of `in` is data
    o%in%p => double_it
    x = 1.5
    call o%in%p(x)
    if (x /= 3.0) error stop

    ! The component before `p` is another procedure pointer
    o%in%f => triple
    x = 1.5
    call o%in%p(x)
    if (x /= 3.0) error stop
    if (o%in%f(2.0) /= 6.0) error stop

    ! Passed-object dummy
    o%in%ps => add_n
    o%in%pf => sub_n
    o%in%n = 5
    x = 1.0
    call o%in%ps(x)
    if (x /= 6.0) error stop
    if (o%in%pf(8.0) /= 3.0) error stop

    ! Three levels
    t%o%in%p => double_it
    t%o%in%ps => add_n
    t%o%in%n = 4
    x = 1.5
    call t%o%in%p(x)
    if (x /= 3.0) error stop
    call t%o%in%ps(x)
    if (x /= 7.0) error stop

    ! Array element, allocatable and pointer components
    o%arr(2)%p => double_it
    o%arr(2)%ps => add_n
    o%arr(2)%n = 2
    x = 1.5
    call o%arr(2)%p(x)
    if (x /= 3.0) error stop
    call o%arr(2)%ps(x)
    if (x /= 5.0) error stop

    allocate(o%al)
    o%al%p => double_it
    o%al%ps => add_n
    o%al%n = 3
    x = 1.5
    call o%al%p(x)
    if (x /= 3.0) error stop
    call o%al%ps(x)
    if (x /= 6.0) error stop

    tg%n = 10
    o%pt => tg
    o%pt%p => double_it
    o%pt%ps => add_n
    x = 1.5
    call o%pt%p(x)
    if (x /= 3.0) error stop
    call o%pt%ps(x)
    if (x /= 13.0) error stop

    print *, "ok"
end program proc_ptr_49
