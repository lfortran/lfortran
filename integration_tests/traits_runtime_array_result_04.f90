! Array results of runtime messages in the contexts of array-valued function
! references: actual arguments of other messages and intrinsics, elemental
! intrinsics, ASSOCIATE, DO WHILE and WHERE, a receiver in the second
! position, an intent(inout) receiver, a message inherited from a parent
! trait, and a view formed from a polymorphic object (TD-38).
module traits_runtime_array_result_04_m
    implicit none
    abstract interface :: ISeries
        function terms(self) result(x)
            type(itself), pass, intent(in) :: self
            real(8), allocatable :: x(:)
        end function
        function weighted(w, self) result(x)
            real(8), intent(in) :: w(:)
            type(itself), pass, intent(in) :: self
            real(8), allocatable :: x(:)
        end function
        function drain(self) result(x)
            type(itself), pass, intent(inout) :: self
            integer, allocatable :: x(:)
        end function
    end interface
    abstract interface, extends(ISeries) :: INamed
        function label(self) result(c)
            type(itself), pass, intent(in) :: self
            character(len=4) :: c
        end function
    end interface
    type :: Geo
        integer :: n = 3
        integer :: drained = 0
    end type
    implements INamed :: Geo
        procedure, pass :: terms => geo_terms
        procedure, pass :: weighted => geo_weighted
        procedure, pass :: drain => geo_drain
        procedure, pass :: label => geo_label
    end implements
contains
    function geo_terms(self) result(x)
        class(Geo), intent(in) :: self
        real(8), allocatable :: x(:)
        integer :: i
        allocate(x(self%n))
        do i = 1, self%n
            x(i) = 2d0**(i - 1)
        end do
    end function
    function geo_weighted(self, w) result(x)
        class(Geo), intent(in) :: self
        real(8), intent(in) :: w(:)
        real(8), allocatable :: x(:)
        allocate(x(min(size(w), self%n)))
        x = w(1:size(x)) * 2
    end function
    function geo_drain(self) result(x)
        class(Geo), intent(inout) :: self
        integer, allocatable :: x(:)
        integer :: i
        allocate(x(self%n))
        x = [(i, i = 1, self%n)]
        self%drained = self%drained + self%n
        self%n = self%n - 1
    end function
    function geo_label(self) result(c)
        class(Geo), intent(in) :: self
        character(len=4) :: c
        c = 'geo!'
    end function
    function relay(s) result(r)
        class(ISeries), intent(in) :: s
        real(8), allocatable :: r(:)
        r = s%terms()
    end function
    subroutine take(x, expected_size)
        real(8), intent(in) :: x(:)
        integer, intent(in) :: expected_size
        if (size(x) /= expected_size) error stop 40
    end subroutine
    subroutine via_parent(s)
        class(ISeries), intent(in) :: s
        if (sum(s%terms()) /= 7) error stop 41
    end subroutine
    subroutine check(s)
        class(INamed), intent(inout) :: s
        real(8), allocatable :: y(:)
        integer, allocatable :: k(:)
        integer :: i
        y = s%terms()
        if (any(y /= [1, 2, 4])) error stop 1
        y = s%weighted([3d0, 4d0, 5d0, 6d0])
        if (any(y /= [6, 8, 10])) error stop 2
        y = s%weighted(s%terms())
        if (any(y /= [2, 4, 8])) error stop 3
        y = sqrt(s%terms())
        if (abs(y(3) - 2) > 1d-12) error stop 4
        if (maxval(s%terms()) /= 4 .or. minval(s%terms()) /= 1) error stop 5
        call take(s%terms(), 3)
        call via_parent(s)
        y = relay(s)
        if (size(y) /= 3) error stop 6
        if (s%label() /= 'geo!') error stop 7
        associate (a => s%terms())
            if (size(a) /= 3 .or. a(2) /= 2) error stop 8
        end associate
        i = 0
        do while (size(s%terms()) > i)
            i = i + 1
        end do
        if (i /= 3) error stop 9
        where (s%terms() > 1.5d0)
            y = -1
        elsewhere
            y = 1
        end where
        if (any(y /= [1, -1, -1])) error stop 10
        k = s%drain()
        if (size(k) /= 3) error stop 11
        k = s%drain()
        if (size(k) /= 2 .or. k(2) /= 2) error stop 12
        if (sum(s%drain()) /= 1) error stop 13
    end subroutine
end module traits_runtime_array_result_04_m

module traits_runtime_array_result_04_shapes_m
    implicit none
    abstract interface :: ISamples
        function samples(self) result(x)
            type(itself), pass, intent(in) :: self
            real(8), allocatable :: x(:)
        end function
        function window(self, n) result(x)
            type(itself), pass, intent(in) :: self
            integer, intent(in) :: n
            integer :: x(n)
        end function
    end interface
    type, abstract, implements(ISamples) :: Shape
        integer :: n = 2
    contains
        procedure(samples_i), deferred :: samples
        procedure :: window => shape_window
    end type
    abstract interface
        function samples_i(self) result(x)
            import :: Shape
            class(Shape), intent(in) :: self
            real(8), allocatable :: x(:)
        end function
    end interface
    type, extends(Shape) :: Square
    contains
        procedure :: samples => square_samples
        procedure :: window => square_window
    end type
contains
    function shape_window(self, n) result(x)
        class(Shape), intent(in) :: self
        integer, intent(in) :: n
        integer :: x(n)
        x = 1
    end function
    function square_window(self, n) result(x)
        class(Square), intent(in) :: self
        integer, intent(in) :: n
        integer :: x(n)
        x = self%n
    end function
    function square_samples(self) result(x)
        class(Square), intent(in) :: self
        real(8), allocatable :: x(:)
        allocate(x(self%n))
        x = self%n**2
    end function
    subroutine check(v, n)
        class(ISamples), intent(in) :: v
        integer, intent(in) :: n
        if (size(v%samples()) /= n .or. any(v%samples() /= n**2)) error stop 51
        if (any(v%window(3) /= n)) error stop 52
    end subroutine
    subroutine via_class(s)
        class(Shape), intent(in) :: s
        class(ISamples), allocatable :: o
        call check(s, s%n)
        o = s
        if (size(o%samples()) /= s%n) error stop 53
    end subroutine
end module traits_runtime_array_result_04_shapes_m

program traits_runtime_array_result_04
    use traits_runtime_array_result_04_m
    use traits_runtime_array_result_04_shapes_m, only: Square, via_class
    implicit none
    type(Geo) :: g
    class(INamed), allocatable :: o
    call check(g)
    if (g%drained /= 6 .or. g%n /= 0) error stop 20
    allocate(o, source=Geo())
    call check(o)
    call via_class(Square(3))
    print '(a)', 'traits_runtime_array_result_04 ok'
end program traits_runtime_array_result_04
