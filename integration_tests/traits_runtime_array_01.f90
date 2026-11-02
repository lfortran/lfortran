! Runtime trait arrays (TD-46): an assumed-shape `class(ISomeTrait) :: arr(:)`
! dummy borrows a nonpolymorphic derived-type array in place, with one witness
! of its element type, as the reported `IClient` message does. Elements are
! views that dispatch, also through a defining receiver, are passed to scalar
! view dummies and are inquired with `size`, `lbound` and `ubound`. The actual
! is a whole array, a strided or reversed section, an explicit-shape, an
! allocatable or a component array, or a forwarded trait array, also of rank
! two, and the message is called statically and through a runtime view.
module traits_runtime_array_01_traits
    implicit none
    private
    public :: ISomeTrait, ICounter, IClient

    abstract interface :: ISomeTrait
        subroutine method(self, a, b)
            type(itself), pass, intent(in) :: self
            real(8), intent(in) :: a, b
        end subroutine method
    end interface

    abstract interface :: ICounter
        subroutine bump(self, by)
            type(itself), pass, intent(inout) :: self
            integer, intent(in) :: by
        end subroutine bump
        integer function count(self)
            type(itself), pass, intent(in) :: self
        end function count
    end interface

    abstract interface :: IClient
        subroutine user(self, arr)
            type(itself), pass, intent(inout) :: self
            class(ISomeTrait), intent(inout) :: arr(:)
        end subroutine user
    end interface
end module traits_runtime_array_01_traits

module traits_runtime_array_01_impl
    use traits_runtime_array_01_traits, only: ISomeTrait, ICounter, IClient
    implicit none
    private
    public :: Adder, Tally, Holder, Client, acc, total, bump_all, bump_matrix, first_count

    real(8) :: acc = 0

    type, implements(ISomeTrait) :: Adder
        real(8) :: w = 1
    contains
        procedure :: method => adder_method
    end type Adder

    type, implements(ICounter) :: Tally
        integer :: n = 0
    contains
        procedure :: bump => tally_bump
        procedure :: count => tally_count
    end type Tally

    type :: Holder
        type(Tally) :: items(4)
    end type Holder

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

    subroutine tally_bump(self, by)
        class(Tally), intent(inout) :: self
        integer, intent(in) :: by
        self%n = self%n + by
    end subroutine tally_bump

    integer function tally_count(self)
        class(Tally), intent(in) :: self
        tally_count = self%n
    end function tally_count

    subroutine user(self, arr)
        class(Client), intent(inout) :: self
        class(ISomeTrait), intent(inout) :: arr(:)
        integer :: i
        do i = 1, size(arr)
            call arr(i)%method(real(i, 8), 1d0)
        end do
        self%calls = self%calls + 1
    end subroutine user

    integer function first_count(item)
        class(ICounter), intent(in) :: item
        first_count = item%count()
    end function first_count

    ! Forwards its trait array and passes elements to a scalar view dummy.
    integer function total(items) result(s)
        class(ICounter), intent(inout) :: items(:)
        integer :: i
        call bump_all(items, 1)
        s = 0
        do i = lbound(items, 1), ubound(items, 1)
            s = s + first_count(items(i))
        end do
    end function total

    subroutine bump_all(items, by)
        class(ICounter), intent(inout) :: items(:)
        integer, intent(in) :: by
        integer :: i
        do i = 1, size(items)
            call items(i)%bump(by * i)
        end do
    end subroutine bump_all

    subroutine bump_matrix(items)
        class(ICounter), intent(inout) :: items(:, :)
        integer :: i, j
        do j = 1, size(items, 2)
            do i = 1, size(items, 1)
                call items(i, j)%bump(10 * i + j)
            end do
        end do
    end subroutine bump_matrix
end module traits_runtime_array_01_impl

program traits_runtime_array_01
    use traits_runtime_array_01_traits, only: IClient
    use traits_runtime_array_01_impl
    implicit none
    type(Adder) :: xs(3)
    type(Client) :: c
    class(IClient), allocatable :: view
    type(Tally) :: t(5), m(2, 3)
    type(Tally), allocatable :: a(:)
    type(Holder) :: h
    integer :: i

    xs(1)%w = 1
    xs(2)%w = 10
    xs(3)%w = 100
    call c%user(xs)
    if (abs(acc - 324) > 1d-12 .or. c%calls /= 1) error stop 1
    acc = 0
    call c%user(xs(3:1:-2))
    if (abs(acc - 104) > 1d-12) error stop 2
    acc = 0
    view = Client()
    call view%user(xs(2:3))
    if (abs(acc - 212) > 1d-12) error stop 3

    call bump_all(t, 1)
    if (any(t%n /= [1, 2, 3, 4, 5])) error stop 4
    call bump_all(t(2::2), 10)
    if (any(t%n /= [1, 12, 3, 24, 5])) error stop 5
    if (total(t(5:1:-1)) /= 45 + 15) error stop 6
    if (any(t%n /= [6, 16, 6, 26, 6])) error stop 7

    allocate(a(3))
    call bump_all(a, 2)
    if (any(a%n /= [2, 4, 6])) error stop 8
    call bump_all(h%items, 3)
    if (any(h%items%n /= [3, 6, 9, 12])) error stop 9

    call bump_matrix(m)
    do i = 1, 2
        if (any(m(i, :)%n /= [10 * i + 1, 10 * i + 2, 10 * i + 3])) error stop 10
    end do
    call bump_matrix(m(2:1:-1, 1:3:2))
    if (m(2, 1)%n /= 32 .or. m(1, 1)%n /= 32 .or. m(2, 3)%n /= 35 .or. &
        m(1, 3)%n /= 35 .or. m(1, 2)%n /= 12) error stop 11
    print *, "ok"
end program traits_runtime_array_01
