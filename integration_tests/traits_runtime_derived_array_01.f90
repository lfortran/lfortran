! A message that is not generic takes arrays of derived types, polymorphic or
! not, as in the reported `class(AbsType), intent(inout) :: arr(:)`: assumed
! shape, explicit shape and allocatable dummies are passed through a runtime
! view in the ordinary calling convention, also in the `--fast` mode. Another
! message takes allocatable, intent(out) scalars: a view of the trait, as in
! the reported `class(ITrait1), allocatable, intent(out) :: obj`, and a
! polymorphic derived type.
module traits_runtime_derived_array_01_traits
    implicit none
    private
    public :: AbsType, Leaf, ITrait1, ITrait2

    type, abstract :: AbsType
        integer :: n = 0
    contains
        procedure(bump_iface), deferred :: bump
    end type AbsType

    abstract interface
        subroutine bump_iface(self, by)
            import :: AbsType
            class(AbsType), intent(inout) :: self
            integer, intent(in) :: by
        end subroutine bump_iface
    end interface

    type, extends(AbsType) :: Leaf
    contains
        procedure :: bump => leaf_bump
    end type Leaf

    abstract interface :: ITrait1
        subroutine method1(self, arr)
            type(itself), pass, intent(inout) :: self
            class(AbsType), intent(inout) :: arr(:)
        end subroutine method1
        integer function total(self, arr, fixed, grown)
            type(itself), pass, intent(in) :: self
            type(Leaf), intent(in) :: arr(:)
            type(Leaf), intent(in) :: fixed(2)
            type(Leaf), allocatable, intent(inout) :: grown(:)
        end function total
    end interface

    abstract interface :: ITrait2
        subroutine method2(self, obj)
            type(itself), pass, intent(in) :: self
            class(ITrait1), allocatable, intent(out) :: obj
        end subroutine method2
        subroutine make_leaf(self, item)
            type(itself), pass, intent(in) :: self
            class(AbsType), allocatable, intent(out) :: item
        end subroutine make_leaf
    end interface

contains

    subroutine leaf_bump(self, by)
        class(Leaf), intent(inout) :: self
        integer, intent(in) :: by
        self%n = self%n + by
    end subroutine leaf_bump
end module traits_runtime_derived_array_01_traits

module traits_runtime_derived_array_01_impl
    use traits_runtime_derived_array_01_traits
    implicit none
    private
    public :: Bumper, Factory

    type, implements(ITrait1) :: Bumper
        integer :: step = 1
    contains
        procedure :: method1 => bumper_method1
        procedure :: total => bumper_total
    end type Bumper

    type, implements(ITrait2) :: Factory
        integer :: step = 0
    contains
        procedure :: method2 => factory_method2
        procedure :: make_leaf => factory_make_leaf
    end type Factory

contains

    subroutine bumper_method1(self, arr)
        class(Bumper), intent(inout) :: self
        class(AbsType), intent(inout) :: arr(:)
        integer :: i
        do i = 1, size(arr)
            call arr(i)%bump(self%step * i)
        end do
    end subroutine bumper_method1

    integer function bumper_total(self, arr, fixed, grown) result(s)
        class(Bumper), intent(in) :: self
        type(Leaf), intent(in) :: arr(:)
        type(Leaf), intent(in) :: fixed(2)
        type(Leaf), allocatable, intent(inout) :: grown(:)
        s = self%step * (sum(arr%n) + sum(fixed%n) + sum(grown%n))
        deallocate(grown)
        allocate(grown(3))
        grown%n = [7, 8, 9]
    end function bumper_total

    subroutine factory_method2(self, obj)
        class(Factory), intent(in) :: self
        class(ITrait1), allocatable, intent(out) :: obj
        obj = Bumper(self%step)
    end subroutine factory_method2

    subroutine factory_make_leaf(self, item)
        class(Factory), intent(in) :: self
        class(AbsType), allocatable, intent(out) :: item
        allocate(Leaf :: item)
        item%n = self%step
    end subroutine factory_make_leaf
end module traits_runtime_derived_array_01_impl

program traits_runtime_derived_array_01
    use traits_runtime_derived_array_01_traits
    use traits_runtime_derived_array_01_impl
    implicit none
    class(ITrait1), allocatable :: view
    class(ITrait2), allocatable :: maker
    type(Leaf) :: xs(4)
    type(Leaf), allocatable :: grown(:)
    class(AbsType), allocatable :: poly(:), item

    maker = Factory(10)
    call maker%method2(view)
    call view%method1(xs)
    if (any(xs%n /= [10, 20, 30, 40])) error stop 1
    call view%method1(xs(4:1:-2))
    if (any(xs%n /= [10, 40, 30, 50])) error stop 2
    allocate(Leaf :: poly(2))
    call view%method1(poly)
    if (any(poly%n /= [10, 20])) error stop 3

    allocate(grown(1))
    grown(1)%n = 5
    if (view%total(xs(1:2), xs(3:4), grown) /= 10 * (50 + 80 + 5)) error stop 4
    if (size(grown) /= 3) error stop 5
    if (any(grown%n /= [7, 8, 9])) error stop 6
    call maker%make_leaf(item)
    call item%bump(1)
    if (item%n /= 11) error stop 7
    print *, "ok"
end program traits_runtime_derived_array_01
