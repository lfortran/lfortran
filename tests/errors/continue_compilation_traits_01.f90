! Keep existing cases in place and append new trait diagnostics at the end.
! M1: single-trait constraints and implementations.

! traits_missing_method_01.f90
module traits_missing_method_01_m
    implicit none

    abstract interface :: IValue
        function get_value() result(res)
            integer :: res
        end function get_value
    end interface IValue

    type :: Box
        integer :: value
    end type Box

    implements IValue :: Box
    end implements Box
end module traits_missing_method_01_m

! traits_structural_no_nominal_01.f90
module traits_structural_no_nominal_01_m
    implicit none

    abstract interface :: IValue
        function get_value() result(res)
            integer :: res
        end function get_value
    end interface IValue

    type :: Box
        integer :: value
    contains
        procedure, pass :: get_value => box_get_value
    end type Box

contains

    function box_get_value(self) result(res)
        class(Box), intent(in) :: self
        integer :: res
        res = self%value
    end function box_get_value

    function read_value{IValue :: T}(x) result(res)
        type(T), intent(in) :: x
        integer :: res
        res = x%get_value()
    end function read_value

end module traits_structural_no_nominal_01_m

module traits_structural_no_nominal_01
    implicit none
contains
    subroutine run()
        use traits_structural_no_nominal_01_m
        implicit none
        type(Box) :: object
        integer :: value
        object = Box(3)
        value = read_value(object)
    end subroutine run
end module traits_structural_no_nominal_01

! traits_wrong_return_kind_rank_01.f90
module traits_wrong_return_kind_rank_01_m
    implicit none

    abstract interface :: IValue
        function get_value() result(res)
            integer :: res
        end function get_value
    end interface IValue

    type :: Box
        integer :: value
    end type Box

    implements IValue :: Box
        procedure, pass :: get_value => box_get_value
    end implements Box

contains

    function box_get_value(self) result(res)
        class(Box), intent(in) :: self
        real :: res
        res = real(self%value)
    end function box_get_value
end module traits_wrong_return_kind_rank_01_m

module traits_wrong_argument_type_01_m
    implicit none
    abstract interface :: IConsume
        subroutine consume(value)
            integer, intent(in) :: value
        end subroutine
    end interface
    type :: Box
        integer :: value
    end type
    implements IConsume :: Box
        procedure, nopass :: consume => consume_value
    end implements
contains
    subroutine consume_value(value)
        real, intent(in) :: value
        print *, value
    end subroutine
end module

module traits_wrong_argument_rank_01_m
    implicit none
    abstract interface :: IConsume
        subroutine consume(value)
            integer, intent(in) :: value
        end subroutine
    end interface
    type :: Box
        integer :: value
    end type
    implements IConsume :: Box
        procedure, nopass :: consume => consume_value
    end implements
contains
    subroutine consume_value(value)
        integer, intent(in) :: value(1)
        print *, value
    end subroutine
end module

module traits_wrong_return_kind_01_m
    implicit none
    abstract interface :: IValue
        function get_value() result(value)
            integer :: value
        end function
    end interface
    type :: Box
        integer :: value
    end type
    implements IValue :: Box
        procedure, pass :: get_value => box_value
    end implements
contains
    function box_value(self) result(value)
        class(Box), intent(in) :: self
        integer(8) :: value
        value = self%value
    end function
end module

module traits_wrong_return_rank_01_m
    implicit none
    abstract interface :: IValue
        function get_value() result(value)
            integer :: value
        end function
    end interface
    type :: Box
        integer :: value
    end type
    implements IValue :: Box
        procedure, pass :: get_value => box_value
    end implements
contains
    function box_value(self) result(value)
        class(Box), intent(in) :: self
        integer :: value(1)
        value = [self%value]
    end function
end module

! traits_bad_body_undeclared_method_01.f90
module traits_bad_body_undeclared_method_01_m
    implicit none

    abstract interface :: IValue
        function get_value() result(res)
            integer :: res
        end function get_value
    end interface IValue

    type :: Box
        integer :: value
    end type Box

    implements IValue :: Box
        procedure, pass :: get_value => box_get_value
    end implements Box

contains

    function box_get_value(self) result(res)
        class(Box), intent(in) :: self
        integer :: res
        res = self%value
    end function box_get_value

    function bad_read_value{IValue :: T}(x) result(res)
        type(T), intent(in) :: x
        integer :: res
        res = x%missing_value()
    end function bad_read_value
end module traits_bad_body_undeclared_method_01_m

! traits_explicit_type_arg_conflict_01.f90
module traits_explicit_type_arg_conflict_01_m
    implicit none

    abstract interface :: IValue
        function get_value() result(res)
            integer :: res
        end function get_value
    end interface IValue

    type :: SmallBox
        integer :: value
    end type SmallBox

    type :: LargeBox
        integer :: value
    end type LargeBox

    implements IValue :: SmallBox
        procedure, pass :: get_value => small_value
    end implements SmallBox

    implements IValue :: LargeBox
        procedure, pass :: get_value => large_value
    end implements LargeBox

contains

    function small_value(self) result(res)
        class(SmallBox), intent(in) :: self
        integer :: res
        res = self%value
    end function small_value

    function large_value(self) result(res)
        class(LargeBox), intent(in) :: self
        integer :: res
        res = 2 * self%value
    end function large_value

    function read_value{IValue :: T}(x) result(res)
        type(T), intent(in) :: x
        integer :: res
        res = x%get_value()
    end function read_value

    subroutine drive()
        type(SmallBox) :: left
        type(LargeBox) :: right
        left = SmallBox(7)
        right = LargeBox(11)
        if (read_value{SmallBox}(right) /= 22) error stop
        if (read_value{LargeBox}(left) /= 7) error stop
    end subroutine drive
end module traits_explicit_type_arg_conflict_01_m

! traits_ambiguous_visible_implementations_01.f90
module traits_ambiguous_visible_implementations_01_trait_m
    implicit none

    abstract interface :: IValue
        function get_value() result(res)
            integer :: res
        end function get_value
    end interface IValue

contains

    function read_value{IValue :: T}(x) result(res)
        type(T), intent(in) :: x
        integer :: res
        res = x%get_value()
    end function read_value
end module traits_ambiguous_visible_implementations_01_trait_m

module traits_ambiguous_visible_implementations_01_types_m
    implicit none

    type :: Box
        integer :: value
    end type Box
end module traits_ambiguous_visible_implementations_01_types_m

module traits_ambiguous_visible_implementations_01_impl_a_m
    use traits_ambiguous_visible_implementations_01_trait_m, only: IValue, read_value
    use traits_ambiguous_visible_implementations_01_types_m
    implicit none

    implements IValue :: Box
        procedure, pass :: get_value => box_get_value_a
    end implements Box

contains

    function box_get_value_a(self) result(res)
        class(Box), intent(in) :: self
        integer :: res
        res = self%value
    end function box_get_value_a
end module traits_ambiguous_visible_implementations_01_impl_a_m

module traits_ambiguous_visible_implementations_01_impl_b_m
    use traits_ambiguous_visible_implementations_01_trait_m, only: IValue, read_value
    use traits_ambiguous_visible_implementations_01_types_m
    implicit none

    implements IValue :: Box
        procedure, pass :: get_value => box_get_value_b
    end implements Box

contains

    function box_get_value_b(self) result(res)
        class(Box), intent(in) :: self
        integer :: res
        res = 2 * self%value
    end function box_get_value_b
end module traits_ambiguous_visible_implementations_01_impl_b_m

module traits_ambiguous_visible_implementations_01
    implicit none
contains
    subroutine run()
        use traits_ambiguous_visible_implementations_01_impl_a_m
        use traits_ambiguous_visible_implementations_01_impl_b_m
        implicit none
        type(Box) :: object

        object = Box(7)
        if (read_value(object) /= 7) error stop
    end subroutine run
end module traits_ambiguous_visible_implementations_01

! traits_function_subroutine_mismatch_01.f90
module traits_function_subroutine_mismatch_01_m
    implicit none

    abstract interface :: IValue
        function get_value() result(res)
            integer :: res
        end function get_value
    end interface IValue

    type :: Box
        integer :: value
    end type Box

    implements IValue :: Box
        procedure, pass :: get_value => box_get_value
    end implements Box

contains

    subroutine box_get_value(self)
        class(Box), intent(in) :: self
        print *, self%value
    end subroutine box_get_value
end module traits_function_subroutine_mismatch_01_m

! traits_missing_type_inference_01.f90
module traits_missing_type_inference_01_m
    implicit none

    abstract interface :: IValue
        subroutine fill_value(out)
            integer, intent(out) :: out
        end subroutine fill_value
    end interface IValue

    type :: Box
        integer :: value
    end type Box

    implements IValue :: Box
        procedure, nopass :: fill_value => box_fill_value
    end implements Box

contains

    subroutine box_fill_value(out)
        integer, intent(out) :: out
        out = 17
    end subroutine box_fill_value

    function make_value{IValue :: T}() result(out)
        integer :: out
        out = 17
    end function make_value
end module traits_missing_type_inference_01_m

module traits_missing_type_inference_01
    implicit none
contains
    subroutine run()
        use traits_missing_type_inference_01_m
        implicit none
        integer :: value
        value = make_value()
    end subroutine run
end module traits_missing_type_inference_01

! traits_private_binding_01.f90
module traits_private_binding_01_m
    implicit none

    abstract interface :: IValue
        function get_value() result(res)
            integer :: res
        end function get_value
    end interface IValue

    type :: Box
        integer :: value
    end type Box

    implements IValue :: Box
        procedure, private :: get_value => box_get_value
    end implements Box

contains

    function box_get_value(self) result(res)
        class(Box), intent(in) :: self
        integer :: res
        res = self%value
    end function box_get_value
end module traits_private_binding_01_m

! traits_optional_ordinary_arg_mismatch_01.f90
module traits_optional_ordinary_arg_mismatch_01_m
    implicit none

    abstract interface :: ISetter
        subroutine set_value(value)
            integer, optional, intent(in) :: value
        end subroutine set_value
    end interface ISetter

    type :: Box
        integer :: value
    end type Box

    implements ISetter :: Box
        procedure, nopass :: set_value => box_set_value
    end implements Box

contains

    subroutine box_set_value(value)
        integer, intent(in) :: value
    end subroutine box_set_value
end module traits_optional_ordinary_arg_mismatch_01_m

! traits_same_name_nonominal_01.f90
module traits_same_name_nonominal_01_m
    implicit none

    abstract interface :: IFirst
        subroutine touch(out)
            integer, intent(out) :: out
        end subroutine touch
    end interface IFirst

    abstract interface :: ISecond
        subroutine touch(out)
            integer, intent(out) :: out
        end subroutine touch
    end interface ISecond

    type :: Box
        integer :: value
    contains
        procedure, pass :: touch => box_touch
    end type Box

    implements IFirst :: Box
    end implements Box

contains

    subroutine box_touch(self, out)
        class(Box), intent(in) :: self
        integer, intent(out) :: out
        out = self%value
    end subroutine box_touch

    subroutine use_second{ISecond :: T}(x, out)
        type(T), intent(in) :: x
        integer, intent(out) :: out
        call x%touch(out)
    end subroutine use_second
end module traits_same_name_nonominal_01_m

module traits_same_name_nonominal_01
    implicit none
contains
    subroutine run()
        use traits_same_name_nonominal_01_m
        implicit none
        type(Box) :: value
        integer :: result
        value = Box(3)
        call use_second(value, result)
    end subroutine run
end module traits_same_name_nonominal_01

! traits_call_argument_type_01.f90
module traits_call_argument_type_01_m
    implicit none
    abstract interface :: IValue
        subroutine get_value(value)
            integer, intent(out) :: value
        end subroutine
    end interface
    type :: Box
        integer :: value
    end type
    implements IValue :: Box
        procedure, pass :: get_value => box_value
    end implements
contains
    subroutine box_value(self, value)
        class(Box), intent(in) :: self
        integer, intent(out) :: value
        value = self%value
    end subroutine
    subroutine query{IValue :: T}(object)
        type(T), intent(in) :: object
        real :: value
        call object%get_value(value)
    end subroutine
end module

! traits_concrete_type_01.f90
module traits_concrete_type_01_m
    implicit none
    abstract interface :: IValue
        function value() result(res)
            integer :: res
        end function
    end interface
    type(IValue) :: object
end module

! traits_runtime_unimplemented_01.f90
module traits_runtime_unimplemented_01_m
    implicit none
    abstract interface :: IValue
        function value() result(res)
            integer :: res
        end function
    end interface
    ! This guard is temporary until runtime trait objects are implemented.
    class(IValue), allocatable :: object
end module

