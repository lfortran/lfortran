! Results of runtime messages. Array results of intrinsic element types are
! dispatched at run time; each variable whose name ends with _result has a
! message whose result is still outside the runtime slot surface, which the
! error names.
module traits_array_result_negative_m
    implicit none
    type :: Point
        integer :: i = 0
    end type Point
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
    abstract interface :: IDerivedResult
        function points(self) result(x)
            import :: Point
            type(itself), pass, intent(in) :: self
            type(Point), allocatable :: x(:)
        end function points
    end interface
    abstract interface :: IPolymorphicResult
        function point(self) result(x)
            import :: Point
            type(itself), pass, intent(in) :: self
            class(Point), allocatable :: x
        end function point
    end interface
    abstract interface :: IAllocatableScalarResult
        function value(self) result(x)
            type(itself), pass, intent(in) :: self
            real(8), allocatable :: x
        end function value
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
    class(IDerivedResult), allocatable :: derived_result
    class(IPolymorphicResult), allocatable :: polymorphic_result
    class(IAllocatableScalarResult), allocatable :: allocatable_scalar_result
    class(IGenericArrayResult), allocatable :: generic_array_result
end module traits_array_result_negative_m
