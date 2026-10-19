! Results of runtime messages. Array results of intrinsic element types,
! derived-type results and allocatable scalar results, polymorphic or not, are
! dispatched at run time; each variable whose name ends with _result has a
! message whose result is still outside the runtime slot surface, which the
! error names.
module traits_array_result_negative_m
    implicit none
    type :: Point
        integer :: i = 0
    end type Point
    type :: Finalized
        integer :: i = 0
    contains
        final :: finalize
    end type Finalized
    abstract interface :: INumeric
        integer | real(8)
    end interface INumeric
    abstract interface :: ISupported
        function samples(self) result(x)
            type(itself), pass, intent(in) :: self
            real(8), allocatable :: x(:, :)
        end function samples
        function window(self, n) result(x)
            type(itself), pass, intent(in) :: self
            integer, intent(in) :: n
            integer :: x(n)
        end function window
    end interface
    abstract interface :: IPointerResult
        function samples(self) result(x)
            type(itself), pass, intent(in) :: self
            real(8), pointer :: x(:)
        end function samples
    end interface
    abstract interface :: IMoreSupported
        function points(self) result(x)
            import :: Point
            type(itself), pass, intent(in) :: self
            type(Point), allocatable :: x(:)
        end function points
        function point(self) result(x)
            import :: Point
            type(itself), pass, intent(in) :: self
            class(Point), allocatable :: x
        end function point
        function value(self) result(x)
            type(itself), pass, intent(in) :: self
            real(8), allocatable :: x
        end function value
        function anything(self) result(x)
            type(itself), pass, intent(in) :: self
            class(*), allocatable :: x
        end function anything
    end interface
    abstract interface :: IUnlimitedArrayResult
        function anything(self) result(x)
            type(itself), pass, intent(in) :: self
            class(*), allocatable :: x(:)
        end function anything
    end interface
    abstract interface :: IFinalizedResult
        function made(self) result(x)
            import :: Finalized
            type(itself), pass, intent(in) :: self
            type(Finalized) :: x
        end function made
    end interface
    abstract interface :: ITraitObjectResult
        function view(self) result(x)
            import :: ISupported
            type(itself), pass, intent(in) :: self
            class(ISupported), allocatable :: x
        end function view
    end interface
    abstract interface :: IGenericAllocatableResult
        function first{INumeric :: T}(self, x) result(r)
            type(itself), pass, intent(in) :: self
            type(T), intent(in) :: x(:)
            type(T), allocatable :: r
        end function first
    end interface
    abstract interface :: IGenericArrayResult
        function twice{INumeric :: T}(self, x) result(r)
            type(itself), pass, intent(in) :: self
            type(T), intent(in) :: x(:)
            type(T) :: r(size(x))
        end function twice
    end interface
    class(ISupported), allocatable :: supported
    class(IPointerResult), allocatable :: pointer_result
    class(IMoreSupported), allocatable :: more_supported
    class(IUnlimitedArrayResult), allocatable :: unlimited_array_result
    class(IFinalizedResult), allocatable :: finalized_result
    class(ITraitObjectResult), allocatable :: trait_object_result
    class(IGenericArrayResult), allocatable :: generic_array_result
    class(IGenericAllocatableResult), allocatable :: generic_allocatable_result
contains
    subroutine finalize(x)
        type(Finalized), intent(inout) :: x
        x%i = 0
    end subroutine finalize
end module traits_array_result_negative_m
