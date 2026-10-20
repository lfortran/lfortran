! Standard Fortran counterpart of traits_runtime_elemental_03: the trait is
! an abstract type whose deferred bindings are elemental.
module traits_runtime_elemental_03_oracle_m
    implicit none
    private
    public :: IFloors, Floors, Holder, make_floors, check_view, check_explicit

    type, abstract :: IFloors
    contains
        procedure(pfloor_interface), deferred :: pfloor
        procedure(blend_interface), deferred :: blend
        procedure(above_interface), deferred :: above
        procedure(bucket_interface), deferred :: bucket
        procedure(raise_interface), deferred :: raise
        procedure(combine_interface), deferred :: combine
    end type IFloors

    abstract interface
        elemental function pfloor_interface(self, p) result(res)
            import :: IFloors
            class(IFloors), intent(in) :: self
            real(8), intent(in) :: p
            real(8) :: res
        end function pfloor_interface
        elemental function blend_interface(self, a, b) result(res)
            import :: IFloors
            class(IFloors), intent(in) :: self
            real(8), intent(in) :: a, b
            real(8) :: res
        end function blend_interface
        elemental logical function above_interface(self, p)
            import :: IFloors
            class(IFloors), intent(in) :: self
            real(8), intent(in) :: p
        end function above_interface
        elemental integer function bucket_interface(self, p)
            import :: IFloors
            class(IFloors), intent(in) :: self
            real(8), intent(in) :: p
        end function bucket_interface
        elemental subroutine raise_interface(self, x)
            import :: IFloors
            class(IFloors), intent(in) :: self
            real(8), intent(inout) :: x
        end subroutine raise_interface
        elemental subroutine combine_interface(self, a, b, c)
            import :: IFloors
            class(IFloors), intent(in) :: self
            real(8), intent(in) :: a, b
            real(8), intent(out) :: c
        end subroutine combine_interface
    end interface

    ! The floor and the weight are private: only the procedures read them.
    type, extends(IFloors) :: Floors
        private
        real(8) :: smallp = 1.d-50
        real(8) :: weight = 1
    contains
        procedure :: pfloor => floors_pfloor
        procedure :: blend => floors_blend
        procedure :: above => floors_above
        procedure :: bucket => floors_bucket
        procedure :: raise => floors_raise
        procedure :: combine => floors_combine
    end type Floors

    type :: Holder
        class(IFloors), allocatable :: source
    end type Holder
contains
    function make_floors(smallp, weight) result(f)
        real(8), intent(in) :: smallp, weight
        type(Floors) :: f
        f%smallp = smallp
        f%weight = weight
    end function make_floors

    elemental function floors_pfloor(self, p) result(res)
        class(Floors), intent(in) :: self
        real(8), intent(in) :: p
        real(8) :: res
        res = max(p, self%smallp)
    end function floors_pfloor

    elemental function floors_blend(self, a, b) result(res)
        class(Floors), intent(in) :: self
        real(8), intent(in) :: a, b
        real(8) :: res
        res = self%weight * a + b
    end function floors_blend

    elemental logical function floors_above(self, p)
        class(Floors), intent(in) :: self
        real(8), intent(in) :: p
        floors_above = p > self%smallp
    end function floors_above

    elemental integer function floors_bucket(self, p)
        class(Floors), intent(in) :: self
        real(8), intent(in) :: p
        floors_bucket = nint(max(p, self%smallp) * self%weight)
    end function floors_bucket

    elemental subroutine floors_raise(self, x)
        class(Floors), intent(in) :: self
        real(8), intent(inout) :: x
        x = max(x, self%smallp)
    end subroutine floors_raise

    elemental subroutine floors_combine(self, a, b, c)
        class(Floors), intent(in) :: self
        real(8), intent(in) :: a, b
        real(8), intent(out) :: c
        c = self%weight * a - b
    end subroutine floors_combine

    ! Every form through a view of a Floors with floor 1 and weight 2.
    subroutine check_view(v)
        class(IFloors), intent(in) :: v
        real(8) :: p(6), q(6), r(6), s, m(2, 3), n(2, 3)
        real(8), allocatable :: z(:)
        integer :: i, k(6)
        p = [(i - 3d0, i = 1, 6)]
        q = v%pfloor(p)
        if (any(q /= [1d0, 1d0, 1d0, 1d0, 2d0, 3d0])) error stop 1
        q = 0
        q(:) = v%pfloor(p(:))
        if (any(q /= [1d0, 1d0, 1d0, 1d0, 2d0, 3d0])) error stop 2
        ! Scalar arguments give scalars.
        s = v%pfloor(-5d0)
        if (s /= 1d0) error stop 3
        if (v%pfloor(p(6)) /= 3d0 .or. v%blend(p(6), 1d0) /= 7d0) error stop 4
        ! Sections and strided sections.
        q = 0
        q(1:3) = v%pfloor(p(4:6))
        if (any(q /= [1d0, 2d0, 3d0, 0d0, 0d0, 0d0])) error stop 5
        q = 0
        q(1:6:2) = v%pfloor(p(6:1:-2))
        if (any(q /= [3d0, 0d0, 1d0, 0d0, 1d0, 0d0])) error stop 6
        ! Several array arguments, and scalars mixed with arrays.
        q = v%blend(p, p)
        if (any(q /= 3 * p)) error stop 7
        q = v%blend(p, 2d0)
        if (any(q /= 2 * p + 2)) error stop 8
        q = v%blend(1d0, p)
        if (any(q /= 2 + p)) error stop 9
        q = v%blend(p(6:1:-1), p)
        if (any(q /= 2 * p(6:1:-1) + p)) error stop 10
        ! In expressions, reductions and nested messages.
        q = 2 * v%pfloor(p) + 1
        if (any(q /= [3d0, 3d0, 3d0, 3d0, 5d0, 7d0])) error stop 11
        if (sum(v%pfloor(p)) /= 9d0) error stop 12
        if (maxval(v%pfloor(p)) /= 3d0 .or. minval(v%pfloor(p)) /= 1d0) error stop 13
        if (count(v%above(p)) /= 2 .or. any(v%above(p(1:4)))) error stop 14
        q = v%pfloor(v%blend(p, -1d0))
        if (any(q /= [1d0, 1d0, 1d0, 1d0, 3d0, 5d0])) error stop 15
        q = v%pfloor(p) * v%blend(p, 0d0)
        if (any(q /= [-4d0, -2d0, 0d0, 2d0, 8d0, 18d0])) error stop 16
        k = v%bucket(p)
        if (any(k /= [2, 2, 2, 2, 4, 6])) error stop 17
        allocate(z(6))
        z = v%pfloor(p)
        if (size(z) /= 6 .or. any(z /= [1d0, 1d0, 1d0, 1d0, 2d0, 3d0])) error stop 18
        ! Rank 2.
        m = reshape(p, [2, 3])
        n = v%pfloor(m)
        if (any(n /= reshape([1d0, 1d0, 1d0, 1d0, 2d0, 3d0], [2, 3]))) error stop 19
        n = v%blend(m, m)
        if (any(n /= 3 * m)) error stop 20
        n = 0
        n(1, :) = v%pfloor(m(2, :))
        if (any(n(1, :) /= [1d0, 1d0, 3d0]) .or. any(n(2, :) /= 0)) error stop 21
        if (sum(v%blend(m, 1d0)) /= 2 * sum(m) + 6) error stop 22
        ! WHERE.
        q = -9
        where (p > 0d0) q = v%pfloor(p - 1.5d0)
        if (any(q /= [-9d0, -9d0, -9d0, 1d0, 1d0, 1.5d0])) error stop 23
        where (v%above(p))
            q = v%blend(p, 0d0)
        elsewhere
            q = v%pfloor(p)
        end where
        if (any(q /= [1d0, 1d0, 1d0, 1d0, 4d0, 6d0])) error stop 24
        ! Targets that overlap the arguments: the right side is evaluated
        ! completely first.
        r = [5d0, -1d0, 7d0, -3d0, 9d0, 0d0]
        r(2:5) = v%pfloor(r(1:4))
        if (any(r /= [5d0, 5d0, 1d0, 7d0, 1d0, 0d0])) error stop 25
        r = [5d0, -1d0, 7d0, -3d0, 9d0, 0d0]
        r(1:4) = v%pfloor(r(2:5))
        if (any(r /= [1d0, 7d0, 1d0, 9d0, 9d0, 0d0])) error stop 26
        r = [5d0, -1d0, 7d0, -3d0, 9d0, 0d0]
        r(2:5) = v%blend(r(1:4), r(3:6))
        if (any(r /= [5d0, 17d0, -5d0, 23d0, -6d0, 0d0])) error stop 27
        r = [5d0, -1d0, 7d0, -3d0, 9d0, 0d0]
        r = v%pfloor(r(6:1:-1))
        if (any(r /= [1d0, 9d0, 1d0, 7d0, 1d0, 5d0])) error stop 28
        r = p
        r = v%pfloor(r)
        if (any(r /= [1d0, 1d0, 1d0, 1d0, 2d0, 3d0])) error stop 29
        ! Elemental subroutines.
        r = p
        call v%raise(r)
        if (any(r /= [1d0, 1d0, 1d0, 1d0, 2d0, 3d0])) error stop 30
        r = p
        call v%raise(r(2:6:2))
        if (any(r /= [-2d0, 1d0, 0d0, 1d0, 2d0, 3d0])) error stop 31
        call v%combine(p, 1d0, r)
        if (any(r /= 2 * p - 1)) error stop 32
        call v%combine(p, p(6:1:-1), r)
        if (any(r /= 2 * p - p(6:1:-1))) error stop 33
        n = 0
        call v%combine(m, m, n)
        if (any(n /= m)) error stop 34
        call v%raise(m)
        if (any(m /= reshape([1d0, 1d0, 1d0, 1d0, 2d0, 3d0], [2, 3]))) error stop 35
        s = -3
        call v%raise(s)
        if (s /= 1d0) error stop 36
        call v%combine(2d0, 1d0, s)
        if (s /= 3d0) error stop 37
    end subroutine check_view

    ! Explicit-shape dummies, whose elements are passed by address: each
    ! element is sent separately, not the first one n times.
    subroutine check_explicit(v, n, p)
        class(IFloors), intent(in) :: v
        integer, intent(in) :: n
        real(8), intent(inout) :: p(n)
        real(8) :: q(n), r(n)
        integer :: k(n)
        q = v%pfloor(p)
        if (any(q /= max(p, 1d0))) error stop 60
        r = v%blend(p, q)
        if (any(r /= 2 * p + q)) error stop 61
        k = v%bucket(p)
        if (any(k /= nint(2 * max(p, 1d0)))) error stop 62
        call v%combine(p, q, r)
        if (any(r /= 2 * p - q)) error stop 63
        r = p
        call v%raise(r)
        if (any(r /= q)) error stop 64
        p = v%pfloor(p)
        if (any(p /= q)) error stop 65
    end subroutine check_explicit
end module traits_runtime_elemental_03_oracle_m

program traits_runtime_elemental_03_oracle
    use traits_runtime_elemental_03_oracle_m
    implicit none
    class(IFloors), allocatable :: obj
    class(IFloors), pointer :: ptr
    type(Floors), target :: f
    type(Holder) :: h
    class(*), allocatable :: thing
    real(8) :: p(5), q(5), m(2, 2)
    integer :: i

    call check_view(make_floors(1d0, 2d0))
    p = [1.d2, -1.d0, 3.d0, -2.d0, 5.d0]
    call check_explicit(make_floors(1d0, 2d0), 5, p)
    if (any(p /= [1.d2, 1.d0, 3.d0, 1.d0, 5.d0])) error stop 66

    ! An owner, as reported.
    obj = Floors()
    p = 1.d2
    p(:) = obj%pfloor(p(:))
    if (any(p /= 1.d2)) error stop 40
    p = [1.d2, -1.d0, 0.d0, 1.d-60, 2.d-50]
    p = obj%pfloor(p)
    if (any(p /= [1.d2, 1.d-50, 1.d-50, 1.d-50, 2.d-50])) error stop 41
    deallocate(obj)
    obj = make_floors(1d0, 2d0)
    call check_view(obj)
    p = [(i - 2d0, i = 1, 5)]
    q = obj%blend(p, 2d0)
    if (any(q /= 2 * p + 2)) error stop 42
    p(2:5) = obj%pfloor(p(1:4))
    if (any(p /= [-1d0, 1d0, 1d0, 1d0, 2d0])) error stop 43
    call obj%raise(p)
    if (any(p /= [1d0, 1d0, 1d0, 1d0, 2d0])) error stop 44

    ! A trait pointer.
    f = make_floors(1d0, 2d0)
    ptr => f
    call check_view(ptr)
    p = [1.d2, -1.d0, 3.d0, -2.d0, 5.d0]
    call check_explicit(ptr, 4, p)
    if (any(p /= [1.d2, 1.d0, 3.d0, 1.d0, 5.d0])) error stop 67
    p = [(i - 2d0, i = 1, 5)]
    q = ptr%pfloor(p)
    if (any(q /= [1d0, 1d0, 1d0, 2d0, 3d0])) error stop 45
    m = reshape(p(1:4), [2, 2])
    m = ptr%blend(m, m)
    if (any(m /= 3 * reshape(p(1:4), [2, 2]))) error stop 46

    ! A trait component.
    allocate(h%source, source=make_floors(1d0, 2d0))
    call check_view(h%source)
    p = [(i - 2d0, i = 1, 5)]
    q = 2 * h%source%pfloor(p) + 1
    if (any(q /= [3d0, 3d0, 3d0, 5d0, 7d0])) error stop 47
    if (sum(h%source%pfloor(p(1:5:2))) /= 5d0) error stop 48
    call h%source%raise(p)
    if (any(p /= [1d0, 1d0, 1d0, 2d0, 3d0])) error stop 49

    ! A CLASS IS associate name.
    allocate(thing, source=make_floors(1d0, 2d0))
    select type (x => thing)
    class is (IFloors)
        call check_view(x)
        p = [(i - 2d0, i = 1, 5)]
        q = x%blend(p, p)
        if (any(q /= 3 * p)) error stop 50
        where (x%above(p)) p = x%blend(p, 1d0)
        if (any(p /= [-1d0, 0d0, 1d0, 5d0, 7d0])) error stop 51
    class default
        error stop 52
    end select
    print '(a)', 'traits_runtime_elemental_03_oracle ok'
end program traits_runtime_elemental_03_oracle
