! An instance with an intrinsic type argument, named explicitly where the
! unit that declares the argument's conformance has not completed its
! conformances yet: the declaration gets that conformance once they are
! complete, in a module procedure, in the main program and in a procedure
! the program contains, which share the program's instance.
module traits_generic_type_05_contracts
    implicit none
    private
    public :: INumeric, ISum
    abstract interface :: INumeric
        integer | real(8)
    end interface INumeric
    abstract interface :: ISum
        function sum{INumeric :: T}(x) result(s)
            type(T), intent(in) :: x(:)
            type(T) :: s
        end function sum
    end interface ISum
end module traits_generic_type_05_contracts

module traits_generic_type_05_averager
    use traits_generic_type_05_contracts, only: ISum, INumeric
    implicit none
    private
    public :: Averager
    type, sealed :: Averager{ISum :: U}
        type(U), allocatable :: drv
    contains
        procedure, pass :: average
    end type Averager
contains
    function average{INumeric :: T}(self, x) result(a)
        type(Averager{U}), intent(in) :: self
        type(T), intent(in) :: x(:)
        type(T) :: a
        a = self%drv%sum(x) / T(size(x))
    end function average
end module traits_generic_type_05_averager

module traits_generic_type_05_app
    use traits_generic_type_05_contracts, only: ISum, INumeric
    use traits_generic_type_05_averager, only: Averager
    implicit none
    private
    public :: run_module
    implements ISum :: integer
        procedure, nopass :: sum => module_sum
    end implements integer
contains
    function module_sum{INumeric :: T}(x) result(s)
        type(T), intent(in) :: x(:)
        type(T) :: s
        integer :: i
        s = T(1000)
        do i = 1, size(x)
            s = s + x(i)
        end do
    end function module_sum
    integer function run_module() result(r)
        type(Averager{integer}) :: a
        a = Averager(drv = 5)
        r = a%average([1, 2, 3, 4, 5])
    end function run_module
end module traits_generic_type_05_app

program traits_generic_type_05
    use traits_generic_type_05_contracts, only: ISum, INumeric
    use traits_generic_type_05_averager, only: Averager
    use traits_generic_type_05_app, only: run_module
    implicit none
    implements ISum :: integer
        procedure, nopass :: sum => program_sum
    end implements integer
    type(Averager{integer}) :: a
    integer :: checks
    checks = 0
    call check(run_module() == 203)
    a = Averager(drv = 5)
    call check(a%average([1, 2, 3, 4, 5]) == 403)
    call check(abs(a%average([1.d0, 2.d0, 3.d0, 4.d0]) - 502.5d0) < 1.d-12)
    b := Averager(drv = 7)
    a = b
    call check(a%average([5, 10]) == 1007)
    call check(contained(a) == 403)
    print '(a, i0)', "traits_generic_type_05 checks: ", checks
contains
    subroutine check(condition)
        logical, intent(in) :: condition
        if (.not. condition) error stop
        checks = checks + 1
    end subroutine check
    integer function contained(x) result(r)
        type(Averager{integer}), intent(in) :: x
        type(Averager{integer}) :: c
        c = x
        r = c%average([1, 2, 3, 4, 5])
    end function contained
    function program_sum{INumeric :: T}(x) result(s)
        type(T), intent(in) :: x(:)
        type(T) :: s
        integer :: i
        s = T(2000)
        do i = 1, size(x)
            s = s + x(i)
        end do
    end function program_sum
end program traits_generic_type_05
