! Trait messages with allocatable scalar results of intrinsic and derived
! types, deferred-length character results, derived-type results that are not
! allocatable, `class(Base), allocatable` results, and allocatable or
! explicit-shape arrays of derived and polymorphic elements, dispatched at run
! time. Results are assigned to targets of the same declared type, to targets
! of another declared type, to targets that are not allocatable and to targets
! that are also actual arguments of the call, and used in expressions.
module traits_runtime_polymorphic_result_02_m
    implicit none
    type :: Base
        integer :: id = 0
    end type Base

    type, extends(Base) :: Child
        real(8) :: w = 0
    end type Child

    type :: Bag
        character(:), allocatable :: name
        integer, allocatable :: v(:)
    end type Bag

    abstract interface :: IMake
        function count(self, n) result(r)
            type(itself), pass, intent(in) :: self
            integer, intent(in) :: n
            integer, allocatable :: r
        end function count
        function ratio(self) result(r)
            type(itself), pass, intent(in) :: self
            real(8), allocatable :: r
        end function ratio
        function word(self) result(r)
            type(itself), pass, intent(in) :: self
            character(:), allocatable :: r
        end function word
        function one(self) result(r)
            import :: Base
            type(itself), pass, intent(in) :: self
            type(Base) :: r
        end function one
        function boxed(self) result(r)
            import :: Base
            type(itself), pass, intent(in) :: self
            type(Base), allocatable :: r
        end function boxed
        function bag(self) result(r)
            import :: Bag
            type(itself), pass, intent(in) :: self
            type(Bag) :: r
        end function bag
        function poly(self, n) result(r)
            import :: Base
            type(itself), pass, intent(in) :: self
            integer, intent(in) :: n
            class(Base), allocatable :: r
        end function poly
        function many(self, n) result(r)
            import :: Base
            type(itself), pass, intent(in) :: self
            integer, intent(in) :: n
            type(Base), allocatable :: r(:)
        end function many
        function fixed(self, n) result(r)
            import :: Base
            type(itself), pass, intent(in) :: self
            integer, intent(in) :: n
            type(Base) :: r(n)
        end function fixed
        function polys(self, n) result(r)
            import :: Base
            type(itself), pass, intent(in) :: self
            integer, intent(in) :: n
            class(Base), allocatable :: r(:)
        end function polys
    end interface

    type :: Maker
        integer :: k = 0
    end type Maker
    implements IMake :: Maker
        procedure, pass :: count => maker_count
        procedure, pass :: ratio => maker_ratio
        procedure, pass :: word => maker_word
        procedure, pass :: one => maker_one
        procedure, pass :: boxed => maker_boxed
        procedure, pass :: bag => maker_bag
        procedure, pass :: poly => maker_poly
        procedure, pass :: many => maker_many
        procedure, pass :: fixed => maker_fixed
        procedure, pass :: polys => maker_polys
    end implements

    type :: Factory
        class(IMake), allocatable :: maker
    end type Factory
contains
    function maker_count(self, n) result(r)
        class(Maker), intent(in) :: self
        integer, intent(in) :: n
        integer, allocatable :: r
        allocate(r)
        r = self%k * 10 + n
    end function maker_count

    function maker_ratio(self) result(r)
        class(Maker), intent(in) :: self
        real(8), allocatable :: r
        r = self%k / 4d0
    end function maker_ratio

    function maker_word(self) result(r)
        class(Maker), intent(in) :: self
        character(:), allocatable :: r
        r = repeat("x", self%k)
    end function maker_word

    function maker_one(self) result(r)
        class(Maker), intent(in) :: self
        type(Base) :: r
        r%id = self%k
    end function maker_one

    function maker_boxed(self) result(r)
        class(Maker), intent(in) :: self
        type(Base), allocatable :: r
        allocate(r)
        r%id = -self%k
    end function maker_boxed

    function maker_bag(self) result(r)
        class(Maker), intent(in) :: self
        type(Bag) :: r
        r%name = repeat("n", self%k)
        allocate(r%v(self%k))
        r%v = self%k
    end function maker_bag

    function maker_poly(self, n) result(r)
        class(Maker), intent(in) :: self
        integer, intent(in) :: n
        class(Base), allocatable :: r
        if (n > 0) then
            r = Child(self%k, 1.5d0 * n)
        else
            r = Base(-self%k)
        end if
    end function maker_poly

    function maker_many(self, n) result(r)
        class(Maker), intent(in) :: self
        integer, intent(in) :: n
        type(Base), allocatable :: r(:)
        integer :: i
        allocate(r(n))
        do i = 1, n
            r(i)%id = self%k + i
        end do
    end function maker_many

    function maker_fixed(self, n) result(r)
        class(Maker), intent(in) :: self
        integer, intent(in) :: n
        type(Base) :: r(n)
        integer :: i
        do i = 1, n
            r(i)%id = self%k * i
        end do
    end function maker_fixed

    function maker_polys(self, n) result(r)
        class(Maker), intent(in) :: self
        integer, intent(in) :: n
        class(Base), allocatable :: r(:)
        integer :: i
        allocate(Child :: r(n))
        select type (r)
        type is (Child)
            do i = 1, n
                r(i)%id = i
                r(i)%w = self%k * 0.5d0
            end do
        end select
    end function maker_polys

    integer function weight(x)
        class(Base), intent(in) :: x
        select type (x)
        type is (Child)
            weight = x%id + nint(x%w)
        class default
            weight = x%id
        end select
    end function weight

    ! The receiver is a borrowed view of a Maker with k = 3.
    subroutine check(c)
        class(IMake), intent(in) :: c
        integer, allocatable :: i
        integer :: j
        real(8), allocatable :: x
        character(:), allocatable :: s
        character(5) :: fixed_word
        type(Base) :: b
        type(Base), allocatable :: ba
        type(Base), allocatable :: bs(:)
        type(Bag) :: g
        class(Base), allocatable :: q
        class(Base), allocatable :: ps(:)
        class(*), allocatable :: anything

        i = c%count(1)
        if (i /= 31) error stop 1
        if (c%count(2) + 1 /= 33) error stop 2
        ! The target is also an actual argument of the call.
        i = c%count(i)
        if (i /= 61) error stop 3
        j = c%count(4)
        if (j /= 34) error stop 4
        x = c%ratio()
        if (x /= 0.75d0) error stop 5
        if (c%ratio() * 4 /= 3d0) error stop 6
        s = c%word()
        if (s /= "xxx" .or. len(s) /= 3) error stop 7
        if (len(c%word()) /= 3) error stop 8
        fixed_word = c%word()
        if (fixed_word /= "xxx") error stop 9
        b = c%one()
        if (b%id /= 3) error stop 10
        if (weight(c%one()) /= 3) error stop 11
        ba = c%boxed()
        if (ba%id /= -3) error stop 12
        b = c%boxed()
        if (b%id /= -3) error stop 13
        do j = 1, 3
            g = c%bag()
        end do
        if (g%name /= "nnn" .or. size(g%v) /= 3 .or. any(g%v /= 3)) error stop 14
        q = c%poly(2)
        select type (q)
        type is (Child)
            if (q%id /= 3 .or. q%w /= 3d0) error stop 15
        class default
            error stop 16
        end select
        ! The result replaces one of another dynamic type.
        q = c%poly(0)
        select type (q)
        type is (Base)
            if (q%id /= -3) error stop 17
        class default
            error stop 18
        end select
        if (weight(c%poly(4)) /= 9) error stop 19
        select type (z => c%poly(1))
        type is (Child)
            if (z%w /= 1.5d0) error stop 20
        class default
            error stop 21
        end select
        deallocate(q)
        allocate(q, source=c%poly(2))
        if (weight(q) /= 6) error stop 22
        ! Targets of another declared type.
        anything = c%count(5)
        select type (anything)
        type is (integer)
            if (anything /= 35) error stop 23
        class default
            error stop 24
        end select
        q = c%boxed()
        if (weight(q) /= -3) error stop 25
        bs = c%many(4)
        if (size(bs) /= 4 .or. bs(4)%id /= 7) error stop 26
        bs = c%fixed(2)
        if (size(bs) /= 2 .or. bs(2)%id /= 6) error stop 27
        if (size(c%many(5)) /= 5) error stop 28
        associate (z => c%poly(3))
            if (weight(z) /= 8) error stop 31
        end associate
        associate (z => c%one())
            if (z%id /= 3) error stop 32
        end associate
        ps = c%polys(3)
        select type (ps)
        type is (Child)
            if (size(ps) /= 3 .or. ps(3)%id /= 3 .or. ps(1)%w /= 1.5d0) error stop 29
        class default
            error stop 30
        end select
    end subroutine check
end module traits_runtime_polymorphic_result_02_m

program traits_runtime_polymorphic_result_02
    use traits_runtime_polymorphic_result_02_m
    implicit none
    class(IMake), allocatable :: owner
    class(IMake), pointer :: p
    type(Maker), target :: m
    type(Factory) :: f
    class(*), allocatable :: thing
    integer, allocatable :: i
    class(Base), allocatable :: q

    call check(Maker(3))
    owner = Maker(3)
    i = owner%count(0)
    if (i /= 30) error stop 40
    call check(owner)
    m%k = 3
    p => m
    q = p%poly(2)
    if (weight(q) /= 6) error stop 41
    call check(p)
    allocate(f%maker, source=Maker(3))
    if (f%maker%word() /= "xxx") error stop 42
    call check(f%maker)
    allocate(thing, source=Maker(3))
    select type (x => thing)
    class is (IMake)
        if (x%count(7) /= 37) error stop 43
        call check(x)
    class default
        error stop 44
    end select
    print '(a)', 'traits_runtime_polymorphic_result_02 ok'
end program traits_runtime_polymorphic_result_02
