! A message with an assumed-shape array dummy that has the TARGET attribute,
! as the reported `real(8), target, intent(in) :: a(:)` of a trait whose view
! is an allocatable component. The implementation associates a pointer
! component of the object with the dummy; an actual with the TARGET
! attribute passed through the view stays the target of that pointer after
! the call, as in an ordinary call, for numeric and derived-type arrays.
module traits_runtime_target_argument_01_traits
    implicit none
    private
    public :: ITargetTrait, Point

    type :: Point
        integer :: x = 0
    end type Point

    abstract interface :: ITargetTrait
        subroutine method(self, a)
            import :: Point
            type(itself), pass, intent(inout) :: self
            real(8), target, intent(in) :: a(:)
        end subroutine method
        subroutine points(self, p)
            import :: Point
            type(itself), pass, intent(inout) :: self
            type(Point), target, intent(inout) :: p(:)
        end subroutine points
    end interface
end module traits_runtime_target_argument_01_traits

module traits_runtime_target_argument_01_impl
    use traits_runtime_target_argument_01_traits, only: ITargetTrait, Point
    implicit none
    private
    public :: Keeper, MyType

    type, implements(ITargetTrait) :: Keeper
        real(8), pointer :: view(:) => null()
        type(Point), pointer :: pts(:) => null()
    contains
        procedure :: method => keeper_method
        procedure :: points => keeper_points
    end type Keeper

    type :: MyType
        class(ITargetTrait), allocatable :: obj
    end type MyType

contains

    subroutine keeper_method(self, a)
        class(Keeper), intent(inout) :: self
        real(8), target, intent(in) :: a(:)
        self%view => a
    end subroutine keeper_method

    subroutine keeper_points(self, p)
        class(Keeper), intent(inout) :: self
        type(Point), target, intent(inout) :: p(:)
        p(1)%x = 11
        self%pts => p
    end subroutine keeper_points
end module traits_runtime_target_argument_01_impl

program traits_runtime_target_argument_01
    use traits_runtime_target_argument_01_traits, only: ITargetTrait, Point
    use traits_runtime_target_argument_01_impl, only: Keeper, MyType
    implicit none
    type(MyType) :: m
    real(8), target :: values(4) = [1d0, 2d0, 3d0, 4d0]
    type(Point), target :: ps(3)

    allocate(Keeper :: m%obj)
    call m%obj%method(values)
    call m%obj%points(ps)
    select type (k => m%obj)
    type is (Keeper)
        if (.not. associated(k%view, values)) error stop 1
        if (size(k%view) /= 4 .or. abs(k%view(3) - 3d0) > 0) error stop 2
        values(3) = 30d0
        if (abs(k%view(3) - 30d0) > 0) error stop 3
        if (.not. associated(k%pts, ps)) error stop 4
        if (ps(1)%x /= 11) error stop 5
        k%pts(2)%x = 22
        if (ps(2)%x /= 22) error stop 6
    class default
        error stop 7
    end select
    print *, "ok"
end program traits_runtime_target_argument_01
