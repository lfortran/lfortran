! Standard Fortran counterpart of traits_runtime_array_result_03: each trait
! is an abstract type whose deferred bindings return the same arrays.
module traits_runtime_array_result_03_oracle_m
    implicit none
    integer, parameter :: nmax = 3
    integer :: nvar = 2
    type, abstract :: IMap
    contains
        procedure(fixed_interface), deferred :: fixed
        procedure(first_interface), deferred :: first
        procedure(scaled_interface), deferred :: scaled
        procedure(table_interface), deferred :: table
        procedure(shifted_interface), deferred :: shifted
        procedure(back_interface), deferred, pass(self) :: back
        procedure(cap_interface), deferred :: cap
        procedure(made_interface), deferred, nopass :: made
        procedure(swapped_interface), deferred :: swapped
    end type IMap
    abstract interface
        function fixed_interface(self) result(x)
            import :: IMap
            class(IMap), intent(in) :: self
            real(8) :: x(3)
        end function fixed_interface
        function first_interface(self, n) result(x)
            import :: IMap
            class(IMap), intent(in) :: self
            integer, intent(in) :: n
            real(8) :: x(n)
        end function first_interface
        function scaled_interface(self, v, k) result(x)
            import :: IMap
            class(IMap), intent(in) :: self
            real(8), intent(in) :: v(:)
            real(8), intent(in) :: k
            real(8) :: x(size(v))
        end function scaled_interface
        function table_interface(self, n, m) result(x)
            import :: IMap
            class(IMap), intent(in) :: self
            integer, intent(in) :: n, m
            integer :: x(0:n-1, m)
        end function table_interface
        function shifted_interface(self, v, k) result(x)
            import :: IMap
            class(IMap), intent(in) :: self
            integer, intent(in) :: v(:)
            integer, intent(in) :: k
            integer, allocatable :: x(:)
        end function shifted_interface
        function back_interface(n, self) result(x)
            import :: IMap
            integer, intent(in) :: n
            class(IMap), intent(inout) :: self
            integer :: x(n)
        end function back_interface
        function cap_interface(self) result(x)
            import :: IMap, nmax
            class(IMap), intent(in) :: self
            integer :: x(nmax, 2)
        end function cap_interface
        function made_interface(n) result(x)
            integer, intent(in) :: n
            real(8) :: x(n)
        end function made_interface
        function swapped_interface(self, a) result(x)
            import :: IMap
            class(IMap), intent(in) :: self
            real, intent(in) :: a(:, :)
            real :: x(size(a, 2), size(a, 1))
        end function swapped_interface
    end interface
    type, extends(IMap) :: Lin
        real(8) :: a = 1
        integer :: calls = 0
    contains
        procedure :: fixed => lin_fixed
        procedure :: first => lin_first
        procedure :: scaled => lin_scaled
        procedure :: table => lin_table
        procedure :: shifted => lin_shifted
        procedure, pass(self) :: back => lin_back
        procedure :: cap => lin_cap
        procedure, nopass :: made => lin_made
        procedure :: swapped => lin_swapped
    end type Lin
contains
    function lin_fixed(self) result(x)
        class(Lin), intent(in) :: self
        real(8) :: x(3)
        x = [1, 2, 3] * self%a
    end function lin_fixed
    function lin_first(self, n) result(x)
        class(Lin), intent(in) :: self
        integer, intent(in) :: n
        real(8) :: x(n)
        integer :: i
        do i = 1, n
            x(i) = i * self%a
        end do
    end function lin_first
    function lin_scaled(self, v, k) result(x)
        class(Lin), intent(in) :: self
        real(8), intent(in) :: v(:)
        real(8), intent(in) :: k
        real(8) :: x(size(v))
        x = v * k * self%a
    end function lin_scaled
    function lin_table(self, n, m) result(x)
        class(Lin), intent(in) :: self
        integer, intent(in) :: n, m
        integer :: x(0:n-1, m)
        integer :: i, j
        do j = 1, m
            do i = 0, n - 1
                x(i, j) = 10 * i + j
            end do
        end do
    end function lin_table
    function lin_shifted(self, v, k) result(x)
        class(Lin), intent(in) :: self
        integer, intent(in) :: v(:)
        integer, intent(in) :: k
        integer, allocatable :: x(:)
        allocate(x(size(v)))
        x = v + k
    end function lin_shifted
    function lin_back(n, self) result(x)
        integer, intent(in) :: n
        class(Lin), intent(inout) :: self
        integer :: x(n)
        integer :: i
        self%calls = self%calls + 1
        do i = 1, n
            x(i) = n - i + 1
        end do
    end function lin_back
    function lin_cap(self) result(x)
        class(Lin), intent(in) :: self
        integer :: x(nmax, 2)
        x = 7
    end function lin_cap
    function lin_made(n) result(x)
        integer, intent(in) :: n
        real(8) :: x(n)
        x = n
    end function lin_made
    function lin_swapped(self, a) result(x)
        class(Lin), intent(in) :: self
        real, intent(in) :: a(:, :)
        real :: x(size(a, 2), size(a, 1))
        x = transpose(a)
    end function lin_swapped

    subroutine check(v)
        class(IMap), intent(inout) :: v
        real(8), allocatable :: y(:)
        real(8) :: w(4)
        integer, allocatable :: t(:,:), s(:), k(:)
        real :: a(2, 3), b(3, 2)
        integer :: n
        y = v%fixed()
        if (size(y) /= 3 .or. any(y /= [2, 4, 6])) error stop 1
        if (sum(v%fixed()) /= 12) error stop 2
        n = 4
        y = v%first(n)
        if (size(y) /= 4 .or. y(4) /= 8) error stop 3
        w = v%first(n)
        if (any(w /= [2, 4, 6, 8])) error stop 4
        if (size(v%first(n + 2)) /= 6) error stop 5
        if (sum(v%first(n) * 2) /= 40) error stop 6
        y = v%scaled([1d0, 2d0], 3d0)
        if (size(y) /= 2 .or. any(y /= [6, 12])) error stop 7
        y = v%scaled(y, 0.5d0)
        if (any(y /= [6, 12])) error stop 8
        ! A function result has lower bounds of one.
        t = v%table(2, 3)
        if (any(shape(t) /= [2, 3]) .or. any(lbound(t) /= 1)) error stop 9
        if (t(2, 3) /= 13 .or. t(1, 1) /= 1) error stop 10
        if (sum(v%table(n, 1)) /= 0 + 10 + 20 + 30 + 4) error stop 11
        if (size(v%table(2, 2), 1) /= 2) error stop 12
        s = v%shifted([1, 2, 3], 5)
        if (any(s /= [6, 7, 8])) error stop 13
        s = v%shifted(s, -5)
        if (any(s /= [1, 2, 3])) error stop 14
        k = v%back(n)
        if (any(k /= [4, 3, 2, 1])) error stop 15
        if (size(v%back(nvar)) /= 2) error stop 16
        if (any(shape(v%cap()) /= [3, 2]) .or. sum(v%cap()) /= 42) error stop 17
        a = reshape([1., 2., 3., 4., 5., 6.], [2, 3])
        b = v%swapped(a)
        if (b(3, 1) /= 5. .or. b(1, 2) /= 2.) error stop 18
        if (any(shape(v%swapped(a(:, 1:2))) /= [2, 2])) error stop 19
        if (sum(v%swapped(a(:, 2:3))) /= 18.) error stop 20
        y = v%made(n)
        if (size(y) /= 4 .or. any(y /= 4)) error stop 21
        if (sum(v%made(2)) /= 4) error stop 22
        print *, v%first(3)
    end subroutine check
end module traits_runtime_array_result_03_oracle_m

program traits_runtime_array_result_03_oracle
    use traits_runtime_array_result_03_oracle_m
    implicit none
    type(Lin) :: x
    class(IMap), allocatable :: owner
    x%a = 2
    call check(x)
    if (x%calls /= 2) error stop 30
    owner = x
    call check(owner)
    select type (owner)
    type is (Lin)
        if (owner%calls /= 4) error stop 31
    end select
    print '(a)', 'traits_runtime_array_result_03_oracle ok'
end program traits_runtime_array_result_03_oracle
