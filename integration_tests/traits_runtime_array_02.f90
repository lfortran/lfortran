! The reported IClient message taking `class(ISomeTrait) :: arr(:)`, from the
! separately compiled traits_runtime_array_02_traits.f90 (TD-46): an
! implementation dispatches through the elements of the borrowed array, called
! statically and through a runtime view, with whole arrays and sections.
module traits_runtime_array_02_impl
    use traits_runtime_array_02_traits, only: ISomeTrait, IClient
    implicit none
    private
    public :: Adder, Client, acc

    real(8) :: acc = 0

    type, implements(ISomeTrait) :: Adder
        real(8) :: w = 1
    contains
        procedure :: method => adder_method
    end type Adder

    type, implements(IClient) :: Client
        integer :: calls = 0
    contains
        procedure :: user
    end type Client

contains

    subroutine adder_method(self, a, b)
        class(Adder), intent(in) :: self
        real(8), intent(in) :: a, b
        acc = acc + self%w * a + b
    end subroutine adder_method

    subroutine user(self, arr)
        class(Client), intent(inout) :: self
        class(ISomeTrait), intent(inout) :: arr(:)
        integer :: i
        do i = 1, size(arr)
            call arr(i)%method(real(i, 8), 1d0)
        end do
        self%calls = self%calls + 1
    end subroutine user
end module traits_runtime_array_02_impl

program traits_runtime_array_02
    use traits_runtime_array_02_traits, only: IClient
    use traits_runtime_array_02_impl, only: Adder, Client, acc
    implicit none
    type(Adder) :: xs(3)
    class(IClient), allocatable :: client_view

    xs(1)%w = 1
    xs(2)%w = 10
    xs(3)%w = 100
    client_view = Client()
    call client_view%user(xs)
    if (abs(acc - 324) > 1d-12) error stop 1
    acc = 0
    call client_view%user(xs(::2))
    if (abs(acc - 203) > 1d-12) error stop 2
    print *, "ok"
end program traits_runtime_array_02
