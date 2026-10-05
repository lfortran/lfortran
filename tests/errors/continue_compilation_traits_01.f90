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

! M2: static trait inheritance and composition (append-only).

! traits_inheritance_parent_only_01.f90
module traits_inheritance_parent_only_01_m
    implicit none

    abstract interface :: IBase
        function value() result(res)
            integer :: res
        end function value
    end interface IBase

    abstract interface, extends(IBase) :: IChild
    end interface IChild

    type :: Payload
        integer :: data
    end type Payload

    implements IBase :: Payload
        procedure, pass :: value => payload_value
    end implements Payload

contains

    function payload_value(self) result(res)
        class(Payload), intent(in) :: self
        integer :: res
        res = self%data
    end function payload_value

    function child_value{IChild :: T}(object) result(res)
        type(T), intent(in) :: object
        integer :: res
        res = object%value()
    end function child_value
end module traits_inheritance_parent_only_01_m

module traits_inheritance_parent_only_01_driver_m
    use traits_inheritance_parent_only_01_m
    implicit none

contains

    subroutine check_child_conformance()
        type(Payload) :: object
        object = Payload(7)
        ! M2A-E01: parent conformance does not imply nominal child conformance.
        if (child_value(object) /= 7) error stop
    end subroutine check_child_conformance
end module traits_inheritance_parent_only_01_driver_m

! traits_inheritance_missing_method_01.f90
module traits_inheritance_missing_method_01_m
    implicit none

    abstract interface :: IBase
        function value() result(res)
            integer :: res
        end function value
    end interface IBase

    abstract interface, extends(IBase) :: IChild
        function child_value() result(res)
            integer :: res
        end function child_value
    end interface IChild

    type :: Payload
        integer :: data
    end type Payload

    ! M2A-E02: a child implementation must provide the inherited value method.
    implements IChild :: Payload
        procedure, pass :: child_value => payload_child_value
    end implements Payload

contains

    function payload_child_value(self) result(res)
        class(Payload), intent(in) :: self
        integer :: res
        res = self%data
    end function payload_child_value
end module traits_inheritance_missing_method_01_m

! traits_composition_missing_nominal_01.f90
module traits_composition_missing_nominal_01_left_m
    implicit none

    abstract interface :: IValue
        function value() result(res)
            integer :: res
        end function value
    end interface IValue
end module traits_composition_missing_nominal_01_left_m

module traits_composition_missing_nominal_01_right_m
    implicit none

    abstract interface :: IValue
        function value() result(res)
            integer :: res
        end function value
    end interface IValue
end module traits_composition_missing_nominal_01_right_m

module traits_composition_missing_nominal_01_m
    use traits_composition_missing_nominal_01_left_m, only: LeftTrait => IValue
    use traits_composition_missing_nominal_01_right_m, only: RightTrait => IValue
    implicit none

    type :: Payload
        integer :: data
    end type Payload

    implements LeftTrait :: Payload
        procedure, pass :: value => payload_value
    end implements Payload

contains

    function payload_value(self) result(res)
        class(Payload), intent(in) :: self
        integer :: res
        res = self%data
    end function payload_value

    function combined_value{LeftTrait + RightTrait :: T}(object) result(res)
        type(T), intent(in) :: object
        integer :: res
        res = object%value()
    end function combined_value

    function reversed_value{RightTrait + LeftTrait :: T}(object) result(res)
        type(T), intent(in) :: object
        integer :: res
        res = object%value()
    end function reversed_value
end module traits_composition_missing_nominal_01_m

module traits_composition_missing_nominal_01_driver_m
    use traits_composition_missing_nominal_01_m
    implicit none

contains

    subroutine check_combined_constraint()
        type(Payload) :: object
        object = Payload(7)
        ! M2A-E03: coalescing a callable must retain the RightTrait obligation.
        if (combined_value(object) /= 7) error stop 1
    end subroutine check_combined_constraint

    subroutine check_reversed_constraint()
        type(Payload) :: object
        object = Payload(7)
        ! M2A-E04: reversing constraints must still require nominal RightTrait evidence.
        if (reversed_value(object) /= 7) error stop 2
    end subroutine check_reversed_constraint
end module traits_composition_missing_nominal_01_driver_m

! traits_composition_return_kind_01.f90
module traits_composition_return_kind_01_m
    implicit none

    abstract interface :: INarrow
        function value() result(res)
            integer(4) :: res
        end function value
    end interface INarrow

    abstract interface :: IWide
        function value() result(res)
            integer(8) :: res
        end function value
    end interface IWide

    ! M2A-E05: identical arguments cannot distinguish incompatible result kinds.
    abstract interface, extends(INarrow + IWide) :: ICombined
    end interface ICombined
end module traits_composition_return_kind_01_m

! traits_composition_return_rank_01.f90
module traits_composition_return_rank_01_m
    implicit none

    abstract interface :: IScalar
        function value() result(res)
            integer :: res
        end function value
    end interface IScalar

    abstract interface :: IArray
        function value() result(res)
            integer :: res(2)
        end function value
    end interface IArray

    ! M2A-E06: identical arguments cannot distinguish incompatible result ranks.
    abstract interface, extends(IScalar + IArray) :: ICombined
    end interface ICombined
end module traits_composition_return_rank_01_m

module traits_composition_result_extent_01_m
    implicit none
    abstract interface :: ITwo
        function values() result(res)
            integer :: res(2)
        end function
    end interface
    abstract interface :: IThree
        function values() result(res)
            integer :: res(3)
        end function
    end interface
    ! M2A-E26: same rank does not make incompatible result extents equivalent.
    abstract interface, extends(ITwo + IThree) :: IConflict
    end interface
end module traits_composition_result_extent_01_m

! traits_composition_attributes_01.f90
module traits_composition_attributes_01_optional_m
    implicit none

    abstract interface :: IRequired
        subroutine consume(value)
            integer, intent(in) :: value
        end subroutine consume
    end interface IRequired

    abstract interface :: IOptional
        subroutine consume(value)
            integer, intent(in), optional :: value
        end subroutine consume
    end interface IOptional

    ! M2A-E07: incompatible optional attributes.
    abstract interface, extends(IRequired + IOptional) :: ICombined
    end interface ICombined
end module traits_composition_attributes_01_optional_m

module traits_composition_attributes_01_intent_m
    implicit none

    abstract interface :: IInput
        subroutine consume(value)
            integer, intent(in) :: value
        end subroutine consume
    end interface IInput

    abstract interface :: IOutput
        subroutine consume(value)
            integer, intent(out) :: value
        end subroutine consume
    end interface IOutput

    ! M2A-E08: incompatible intent attributes.
    abstract interface, extends(IInput + IOutput) :: ICombined
    end interface ICombined
end module traits_composition_attributes_01_intent_m

module traits_composition_attributes_01_value_m
    implicit none

    abstract interface :: IReference
        subroutine consume(value)
            integer, intent(in) :: value
        end subroutine consume
    end interface IReference

    abstract interface :: IByValue
        subroutine consume(value)
            integer, intent(in), value :: value
        end subroutine consume
    end interface IByValue

    ! M2A-E09: incompatible value attributes.
    abstract interface, extends(IReference + IByValue) :: ICombined
    end interface ICombined
end module traits_composition_attributes_01_value_m

module traits_composition_attributes_01_pure_m
    implicit none

    abstract interface :: IOrdinary
        function value() result(res)
            integer :: res
        end function value
    end interface IOrdinary

    abstract interface :: IPure
        pure function value() result(res)
            integer :: res
        end function value
    end interface IPure

    ! M2A-E10: incompatible pure attributes.
    abstract interface, extends(IOrdinary + IPure) :: ICombined
    end interface ICombined
end module traits_composition_attributes_01_pure_m

module traits_composition_attributes_01_elemental_m
    implicit none

    abstract interface :: IPure
        pure function shifted(delta) result(res)
            integer, intent(in) :: delta
            integer :: res
        end function shifted
    end interface IPure

    abstract interface :: IElemental
        elemental function shifted(delta) result(res)
            integer, intent(in) :: delta
            integer :: res
        end function shifted
    end interface IElemental

    ! M2A-E11: incompatible elemental attributes.
    abstract interface, extends(IPure + IElemental) :: ICombined
    end interface ICombined
end module traits_composition_attributes_01_elemental_m

module traits_composition_attributes_01_names_m
    implicit none

    abstract interface :: IDelta
        function shifted(delta) result(res)
            integer, intent(in) :: delta
            integer :: res
        end function shifted
    end interface IDelta

    abstract interface :: IAmount
        function shifted(amount) result(res)
            integer, intent(in) :: amount
            integer :: res
        end function shifted
    end interface IAmount

    ! M2A-E12: ordinary dummy names are part of signature compatibility.
    abstract interface, extends(IDelta + IAmount) :: ICombined
    end interface ICombined
end module traits_composition_attributes_01_names_m

! traits_inheritance_dummy_name_01.f90
module traits_inheritance_dummy_name_01_m
    implicit none

    abstract interface :: IBase
        function shifted(delta) result(res)
            integer, intent(in) :: delta
            integer :: res
        end function shifted
    end interface IBase

    abstract interface, extends(IBase) :: IChild
    end interface IChild

    type :: Payload
        integer :: data
    end type Payload

    ! M2A-E13: renamed implementation dummy amount is accepted (no diagnostic).
    implements IChild :: Payload
        procedure, pass :: shifted => payload_shifted
    end implements Payload

contains

    function payload_shifted(self, amount) result(res)
        class(Payload), intent(in) :: self
        integer, intent(in) :: amount
        integer :: res
        res = self%data + amount
    end function payload_shifted
end module traits_inheritance_dummy_name_01_m

! traits_inheritance_conflicting_witness_01.f90
module traits_inheritance_conflicting_witness_01_contracts_m
    implicit none

    abstract interface :: IBase
        function value() result(res)
            integer :: res
        end function value
    end interface IBase

    abstract interface, extends(IBase) :: IChild
    end interface IChild

contains

    function read_value{IBase :: T}(object) result(res)
        type(T), intent(in) :: object
        integer :: res
        res = object%value()
    end function read_value
end module traits_inheritance_conflicting_witness_01_contracts_m

module traits_inheritance_conflicting_witness_01_types_m
    implicit none

    type :: Payload
        integer :: data
    end type Payload
end module traits_inheritance_conflicting_witness_01_types_m

module traits_inheritance_conflicting_witness_01_parent_m
    use traits_inheritance_conflicting_witness_01_contracts_m, only: IBase, read_value
    use traits_inheritance_conflicting_witness_01_types_m
    implicit none

    implements IBase :: Payload
        procedure, pass :: value => parent_value
    end implements Payload

contains

    function parent_value(self) result(res)
        class(Payload), intent(in) :: self
        integer :: res
        res = self%data
    end function parent_value
end module traits_inheritance_conflicting_witness_01_parent_m

module traits_inheritance_conflicting_witness_01_child_m
    use traits_inheritance_conflicting_witness_01_contracts_m, only: IChild, read_value
    use traits_inheritance_conflicting_witness_01_types_m
    implicit none

    implements IChild :: Payload
        procedure, pass :: value => child_value
    end implements Payload

contains

    function child_value(self) result(res)
        class(Payload), intent(in) :: self
        integer :: res
        res = -self%data
    end function child_value
end module traits_inheritance_conflicting_witness_01_child_m

module traits_inheritance_conflicting_witness_01_forward_m
    use traits_inheritance_conflicting_witness_01_parent_m
    use traits_inheritance_conflicting_witness_01_child_m
    implicit none
contains
    subroutine probe_forward()
        type(Payload) :: object
        object = Payload(7)
        ! M2A-E14: distinct parent/child witnesses conflict with parent imported first.
        if (read_value(object) /= 7) error stop
    end subroutine probe_forward
end module traits_inheritance_conflicting_witness_01_forward_m

module traits_inheritance_conflicting_witness_01_reverse_m
    use traits_inheritance_conflicting_witness_01_child_m
    use traits_inheritance_conflicting_witness_01_parent_m
    implicit none
contains
    subroutine probe_reverse()
        type(Payload) :: object
        object = Payload(7)
        ! M2A-E15: reversing import order must not choose a conflicting witness.
        if (read_value(object) /= 7) error stop
    end subroutine probe_reverse
end module traits_inheritance_conflicting_witness_01_reverse_m

module traits_inheritance_conflicting_witness_01_driver_m
    use traits_inheritance_conflicting_witness_01_forward_m, only: probe_forward
    use traits_inheritance_conflicting_witness_01_reverse_m, only: probe_reverse
    implicit none

contains

    subroutine check_import_orders()
        call probe_forward()
        call probe_reverse()
    end subroutine check_import_orders
end module traits_inheritance_conflicting_witness_01_driver_m

module traits_inheritance_conflicting_receiver_01_types_m
    implicit none

    type :: Payload
        integer :: data
    end type Payload

contains

    function pair_value(left, right) result(res)
        class(Payload), intent(in) :: left, right
        integer :: res
        res = left%data - right%data
    end function pair_value
end module traits_inheritance_conflicting_receiver_01_types_m

module traits_inheritance_conflicting_receiver_01_contracts_m
    use traits_inheritance_conflicting_receiver_01_types_m, only: Payload
    implicit none

    abstract interface :: IBase
        function value(other) result(res)
            import :: Payload
            class(Payload), intent(in) :: other
            integer :: res
        end function value
    end interface IBase

    abstract interface, extends(IBase) :: IChild
    end interface IChild

contains

    function read_value{IBase :: T}(object, other) result(res)
        type(T), intent(in) :: object
        type(Payload), intent(in) :: other
        integer :: res
        res = object%value(other)
    end function read_value
end module traits_inheritance_conflicting_receiver_01_contracts_m

module traits_inheritance_conflicting_receiver_01_parent_m
    use traits_inheritance_conflicting_receiver_01_contracts_m, only: IBase, read_value
    use traits_inheritance_conflicting_receiver_01_types_m
    implicit none

    implements IBase :: Payload
        procedure, pass(left) :: value => pair_value
    end implements Payload
end module traits_inheritance_conflicting_receiver_01_parent_m

module traits_inheritance_conflicting_receiver_01_child_m
    use traits_inheritance_conflicting_receiver_01_contracts_m, only: IChild, read_value
    use traits_inheritance_conflicting_receiver_01_types_m
    implicit none

    implements IChild :: Payload
        procedure, pass(right) :: value => pair_value
    end implements Payload
end module traits_inheritance_conflicting_receiver_01_child_m

module traits_inheritance_conflicting_receiver_01_driver_m
    use traits_inheritance_conflicting_receiver_01_parent_m
    use traits_inheritance_conflicting_receiver_01_child_m
    implicit none

contains

    subroutine check_receiver_positions()
        type(Payload) :: object, other
        object = Payload(9)
        other = Payload(2)
        ! M2A-E25: the same canonical procedure has inconsistent receiver positions.
        if (read_value(object, other) /= 7) error stop
    end subroutine check_receiver_positions
end module traits_inheritance_conflicting_receiver_01_driver_m

! traits_composition_unused_body_01.f90
module traits_composition_unused_body_01_m
    implicit none

    abstract interface :: IValue
        function value() result(res)
            integer :: res
        end function value
    end interface IValue

    abstract interface :: IShift
        function shift(delta) result(res)
            integer, intent(in) :: delta
            integer :: res
        end function shift
    end interface IShift

    abstract interface, extends(IValue + IShift) :: IChild
        function child_value() result(res)
            integer :: res
        end function child_value
    end interface IChild

contains

    ! M2A-E16: a child-only capability is unavailable even in an unused generic.
    function unused_value{IValue + IShift :: T}(object) result(res)
        type(T), intent(in) :: object
        integer :: res
        res = object%child_value()
    end function unused_value
end module traits_composition_unused_body_01_m

! traits_inheritance_invalid_parents_01.f90
module traits_inheritance_invalid_parents_01_unknown_m
    implicit none

    ! M2A-E17: unknown parent trait.
    abstract interface, extends(IMissing) :: IChild
    end interface IChild
end module traits_inheritance_invalid_parents_01_unknown_m

module traits_inheritance_invalid_parents_01_nontrait_m
    implicit none

    type :: Payload
        integer :: data
    end type Payload

    ! M2A-E18: a derived type is not a trait parent.
    abstract interface, extends(Payload) :: IChild
    end interface IChild
end module traits_inheritance_invalid_parents_01_nontrait_m

module traits_inheritance_invalid_parents_01_self_m
    implicit none

    ! M2A-E19: a trait cannot extend itself.
    abstract interface, extends(ISelf) :: ISelf
    end interface ISelf
end module traits_inheritance_invalid_parents_01_self_m

! traits_composition_unsupported_overload_01.f90
! These are valid overloaded requirements, temporarily unsupported in this slice.
module traits_composition_unsupported_overload_01_type_m
    implicit none

    abstract interface :: IInteger
        function measure(value) result(res)
            integer, intent(in) :: value
            integer :: res
        end function measure
    end interface IInteger

    abstract interface :: IReal
        function measure(value) result(res)
            real, intent(in) :: value
            integer :: res
        end function measure
    end interface IReal

    ! M2A-E20: temporarily unsupported ordinary-argument type overload.
    abstract interface, extends(IInteger + IReal) :: ICombined
    end interface ICombined
end module traits_composition_unsupported_overload_01_type_m

module traits_composition_unsupported_overload_01_arity_m
    implicit none

    abstract interface :: INoArgument
        function measure() result(res)
            integer :: res
        end function measure
    end interface INoArgument

    abstract interface :: IArgument
        function measure(value) result(res)
            integer, intent(in) :: value
            integer :: res
        end function measure
    end interface IArgument

    ! M2A-E21: temporarily unsupported ordinary-argument count overload.
    abstract interface, extends(INoArgument + IArgument) :: ICombined
    end interface ICombined
end module traits_composition_unsupported_overload_01_arity_m

module traits_composition_unsupported_overload_01_kind_m
    implicit none

    abstract interface :: INarrow
        function measure(value) result(res)
            integer(4), intent(in) :: value
            integer :: res
        end function measure
    end interface INarrow

    abstract interface :: IWide
        function measure(value) result(res)
            integer(8), intent(in) :: value
            integer :: res
        end function measure
    end interface IWide

    ! M2A-E22: temporarily unsupported ordinary-argument kind overload.
    abstract interface, extends(INarrow + IWide) :: ICombined
    end interface ICombined
end module traits_composition_unsupported_overload_01_kind_m

module traits_composition_unsupported_overload_01_rank_m
    implicit none

    abstract interface :: IScalar
        function measure(value) result(res)
            integer, intent(in) :: value
            integer :: res
        end function measure
    end interface IScalar

    abstract interface :: IArray
        function measure(value) result(res)
            integer, intent(in) :: value(:)
            integer :: res
        end function measure
    end interface IArray

    ! M2A-E23: temporarily unsupported ordinary-argument rank overload.
    abstract interface, extends(IScalar + IArray) :: ICombined
    end interface ICombined
end module traits_composition_unsupported_overload_01_rank_m

module traits_composition_unsupported_overload_01_constraint_m
    implicit none

    abstract interface :: IInteger
        function measure(value) result(res)
            integer, intent(in) :: value
            integer :: res
        end function measure
    end interface IInteger

    abstract interface :: IReal
        function measure(value) result(res)
            real, intent(in) :: value
            integer :: res
        end function measure
    end interface IReal

contains

    ! M2A-E24: temporarily unsupported overloaded method in composed constraints.
    function query{IInteger + IReal :: T}(object) result(res)
        type(T), intent(in) :: object
        integer :: res
        res = object%measure(2)
    end function query
end module traits_composition_unsupported_overload_01_constraint_m
