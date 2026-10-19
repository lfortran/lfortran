! Generic resolution accepts a class(RhsBase) actual for a trait view dummy
! when the abstract declared type RhsBase adopts the trait nominally but
! leaves a method deferred, so that no implementation of its own exists, as
! a direct call of the specific does (TD-38); so does a class(Decay) actual
! of an extension that inherits the adoption. Through a function and a
! subroutine generic, an intent(inout) view, a generic named like a type
! (a user-defined constructor) and a type-bound generic.
module traits_runtime_polymorphic_source_05_m
    implicit none
    abstract interface :: IRhs
        subroutine eval(self, y, yp)
            type(itself), pass, intent(in) :: self
            real(8), intent(in) :: y(:)
            real(8), intent(out) :: yp(:)
        end subroutine
        real(8) function stiffness(self)
            type(itself), pass, intent(in) :: self
        end function
        subroutine tune(self, k)
            type(itself), pass, intent(inout) :: self
            real(8), intent(in) :: k
        end subroutine
    end interface
    type, abstract, implements(IRhs) :: RhsBase
        real(8) :: k = 1
    contains
        procedure(eval_i), deferred :: eval
        procedure :: stiffness => base_stiffness
        procedure :: tune => base_tune
    end type
    abstract interface
        subroutine eval_i(self, y, yp)
            import RhsBase
            class(RhsBase), intent(in) :: self
            real(8), intent(in) :: y(:)
            real(8), intent(out) :: yp(:)
        end subroutine
    end interface
    type, extends(RhsBase) :: Decay
    contains
        procedure :: eval => decay_eval
        procedure :: stiffness => decay_stiffness
    end type
    type :: Problem
        class(IRhs), allocatable :: f
        real(8) :: h = 0
    end type
    interface Problem
        module procedure make_problem
    end interface
    interface stiff
        module procedure stiff_view
    end interface
    interface retune
        module procedure retune_view
    end interface
    type :: Integrator
        real(8) :: h = 0.1d0
    contains
        procedure :: advance_view
        generic :: advance => advance_view
    end type
contains
    real(8) function base_stiffness(self)
        class(RhsBase), intent(in) :: self
        base_stiffness = self%k
    end function
    subroutine base_tune(self, k)
        class(RhsBase), intent(inout) :: self
        real(8), intent(in) :: k
        self%k = k
    end subroutine
    subroutine decay_eval(self, y, yp)
        class(Decay), intent(in) :: self
        real(8), intent(in) :: y(:)
        real(8), intent(out) :: yp(:)
        yp = -self%k*y
    end subroutine
    real(8) function decay_stiffness(self)
        class(Decay), intent(in) :: self
        decay_stiffness = 10*self%k
    end function

    function make_problem(f, h) result(p)
        class(IRhs), intent(in) :: f
        real(8), intent(in) :: h
        type(Problem) :: p
        allocate(p%f, source=f)
        p%h = min(h, 0.5d0/f%stiffness())
    end function
    real(8) function stiff_view(f)
        class(IRhs), intent(in) :: f
        stiff_view = f%stiffness()
    end function
    subroutine retune_view(f, k)
        class(IRhs), intent(inout) :: f
        real(8), intent(in) :: k
        call f%tune(k)
    end subroutine
    subroutine advance_view(self, f, y)
        class(Integrator), intent(in) :: self
        class(IRhs), intent(in) :: f
        real(8), intent(inout) :: y(:)
        real(8) :: yp(size(y))
        call f%eval(y, yp)
        y = y + self%h*yp
    end subroutine
end module

program traits_runtime_polymorphic_source_05
    use traits_runtime_polymorphic_source_05_m
    implicit none
    class(RhsBase), allocatable :: d
    class(Decay), allocatable :: e
    type(Problem) :: p
    type(Integrator) :: integ
    real(8) :: y(2)
    allocate(Decay :: d)
    allocate(e)
    d%k = 2
    e%k = 3

    if (stiff(d) /= 20) error stop 1
    if (stiff(e) /= 30) error stop 2
    call retune(d, 4d0)
    if (d%k /= 4 .or. stiff(d) /= 40) error stop 3
    call retune(e, 1d0)
    if (e%k /= 1) error stop 4

    p = Problem(d, 0.1d0)
    print *, p%h, p%f%stiffness()
    if (abs(p%h - 0.0125d0) > 1d-15) error stop 5
    if (p%f%stiffness() /= 40) error stop 6
    p = Problem(e, 0.01d0)
    if (p%h /= 0.01d0) error stop 7

    y = [1d0, 2d0]
    call integ%advance(d, y)
    print *, y
    if (any(abs(y - [0.6d0, 1.2d0]) > 1d-14)) error stop 8
    y = [1d0, 2d0]
    call integ%advance(e, y)
    print *, y
    if (any(abs(y - [0.9d0, 1.8d0]) > 1d-14)) error stop 9
    print '(a)', 'traits_runtime_polymorphic_source_05 ok'
end program
