! A user's module: a message returns an allocatable array whose size is an
! argument, and a derived type owns a conforming object through an
! allocatable trait component, through which the message is dispatched.
module traits_runtime_array_result_05_m
    implicit none
    abstract interface :: IMyTrait
        function method(self, n) result(a)
            type(itself), pass, intent(in) :: self
            integer, intent(in) :: n
            real(8), allocatable :: a(:)
        end function method
    end interface
    type :: MyType
        class(IMyTrait), allocatable :: p
    end type MyType

    type :: Linear
        real(8) :: slope = 1
    end type Linear
    implements IMyTrait :: Linear
        procedure, pass :: method => linear_method
    end implements

    type :: Constant
        real(8) :: c = 0
    end type Constant
    implements IMyTrait :: Constant
        procedure, pass :: method => constant_method
    end implements
contains
    function linear_method(self, n) result(a)
        class(Linear), intent(in) :: self
        integer, intent(in) :: n
        real(8), allocatable :: a(:)
        integer :: i
        allocate(a(n))
        do i = 1, n
            a(i) = self%slope * i
        end do
    end function linear_method

    function constant_method(self, n) result(a)
        class(Constant), intent(in) :: self
        integer, intent(in) :: n
        real(8), allocatable :: a(:)
        allocate(a(n))
        a = self%c
    end function constant_method

    real(8) function total(x)
        real(8), intent(in) :: x(:)
        total = sum(x)
    end function total

    subroutine check(x, expected)
        type(MyType), intent(in) :: x
        real(8), intent(in) :: expected(:)
        real(8), allocatable :: y(:)
        integer :: n
        n = size(expected)
        y = x%p%method(n)
        if (size(y) /= n .or. lbound(y, 1) /= 1) error stop 1
        if (any(y /= expected)) error stop 2
        if (size(x%p%method(n + 2)) /= n + 2) error stop 3
        if (any(x%p%method(n) * 2 - expected /= expected)) error stop 4
        if (total(x%p%method(n)) /= sum(expected)) error stop 5
        y = x%p%method(n) + y
        if (any(y /= 2 * expected)) error stop 6
        if (size(x%p%method(0)) /= 0) error stop 7
    end subroutine check
end module traits_runtime_array_result_05_m

program traits_runtime_array_result_05
    use traits_runtime_array_result_05_m
    implicit none
    type(MyType) :: x
    type(MyType), allocatable :: many(:)
    real(8), allocatable :: y(:)
    integer :: i

    allocate(x%p, source=Linear(2))
    call check(x, [2d0, 4d0, 6d0, 8d0, 10d0])
    y = x%p%method(5)
    if (size(y) /= 5 .or. y(5) /= 10) error stop 10
    deallocate(x%p)
    x%p = Constant(3)
    call check(x, [3d0, 3d0])
    allocate(many(3))
    do i = 1, 3
        allocate(many(i)%p, source=Linear(real(i, 8)))
    end do
    do i = 1, 3
        if (sum(many(i)%p%method(i)) /= i * (i * (i + 1) / 2)) error stop 11
    end do
    print '(a)', 'traits_runtime_array_result_05 ok'
end program traits_runtime_array_result_05
