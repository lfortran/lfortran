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
    ! Scalar owning storage is now valid; unsupported escapes are tested below.
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

! M2A-E25: assumed-shape and assumed-size inherited contracts are incompatible.
module traits_shape_contract_inheritance_01_m
    implicit none
    abstract interface :: IShape
        function count(a) result(r)
            integer, intent(in) :: a(:)
            integer :: r
        end function
    end interface
    abstract interface :: ISize
        function count(a) result(r)
            integer, intent(in) :: a(*)
            integer :: r
        end function
    end interface
    abstract interface, extends(IShape + ISize) :: IChild
    end interface
end module

! M2A-E26: reversing the parent order must not change shape compatibility.
module traits_shape_contract_inheritance_02_m
    implicit none
    abstract interface :: IShape
        function count(a) result(r)
            integer, intent(in) :: a(:)
            integer :: r
        end function
    end interface
    abstract interface :: ISize
        function count(a) result(r)
            integer, intent(in) :: a(*)
            integer :: r
        end function
    end interface
    abstract interface, extends(ISize + IShape) :: IChild
    end interface
end module

! M2A-E27: direct composition requires the same array shape category.
module traits_shape_contract_composition_01_m
    implicit none
    abstract interface :: IShape
        function count(a) result(r)
            integer, intent(in) :: a(:)
            integer :: r
        end function
    end interface
    abstract interface :: ISize
        function count(a) result(r)
            integer, intent(in) :: a(*)
            integer :: r
        end function
    end interface
contains
    function query{IShape + ISize :: T}(object) result(r)
        type(T), intent(in) :: object
        integer :: r
        r = 0
    end function
end module

! M2A-E28: reversing the constraint order must not change shape compatibility.
module traits_shape_contract_composition_02_m
    implicit none
    abstract interface :: IShape
        function count(a) result(r)
            integer, intent(in) :: a(:)
            integer :: r
        end function
    end interface
    abstract interface :: ISize
        function count(a) result(r)
            integer, intent(in) :: a(*)
            integer :: r
        end function
    end interface
contains
    function query{ISize + IShape :: T}(object) result(r)
        type(T), intent(in) :: object
        integer :: r
        r = 0
    end function
end module

! M2A-E29: both subscripts are valid, but independent inherited bounds differ.
module traits_arrayitem_contract_inheritance_m
    implicit none
    abstract interface :: IFirst
        function count(n, a) result(r)
            integer, intent(in) :: n(2), a(n(1))
            integer :: r
        end function
    end interface
    abstract interface :: ISecond
        function count(n, a) result(r)
            integer, intent(in) :: n(2), a(n(2))
            integer :: r
        end function
    end interface
    abstract interface, extends(IFirst + ISecond) :: IChild
    end interface
end module

! M2A-E30: direct composition must also distinguish array subscripts.
module traits_arrayitem_contract_composition_m
    implicit none
    abstract interface :: IFirst
        function count(n, zinput) result(r)
            integer, intent(in) :: n(2), zinput(n(1))
            integer :: r
        end function
    end interface
    abstract interface :: ISecond
        function count(n, zinput) result(r)
            integer, intent(in) :: n(2), zinput(n(2))
            integer :: r
        end function
    end interface
contains
    function unused{IFirst + ISecond :: T}(x) result(r)
        type(T), intent(in) :: x
        integer :: r
        r = 0
    end function
end module

! M2A-E31: positional implementation renaming cannot change the bound's index.
module traits_arrayitem_contract_binding_m
    implicit none
    abstract interface :: IFirst
        function count(n, a) result(r)
            integer, intent(in) :: n(2), a(n(1))
            integer :: r
        end function
    end interface
    type :: Box
        integer :: data
    end type
    implements IFirst :: Box
        procedure, pass(self) :: count => box_count
    end implements
contains
    function box_count(extents, self, values) result(r)
        integer, intent(in) :: extents(2), values(extents(2))
        class(Box), intent(in) :: self
        integer :: r
        r = self%data + sum(values)
    end function
end module

! A-N01: integer(int64) is not a member of default integer | real(real64).
module traits_numeric_a_n01_m
    use iso_fortran_env, only: int64, real64
    implicit none
    abstract interface :: INumeric
        integer | real(real64)
    end interface INumeric
contains
    function identity{INumeric :: T}(x) result(value)
        type(T), intent(in) :: x
        type(T) :: value
        value = x
    end function identity
    subroutine run_case()
        integer(int64) :: value
        value = identity(7_int64)
    end subroutine run_case
end module traits_numeric_a_n01_m

! A-N02: real(real32) is not a member, even though its arithmetic would work.
module traits_numeric_a_n02_m
    use iso_fortran_env, only: real32, real64
    implicit none
    abstract interface :: INumeric
        integer | real(real64)
    end interface INumeric
contains
    function identity{INumeric :: T}(x) result(value)
        type(T), intent(in) :: x
        type(T) :: value
        value = x
    end function identity
    subroutine run_case()
        real(real32) :: value
        value = identity(7.0_real32)
    end subroutine run_case
end module traits_numeric_a_n02_m

! A-N03: complex(real64) is not a member, even with an otherwise valid identity body.
module traits_numeric_a_n03_m
    use iso_fortran_env, only: real64
    implicit none
    abstract interface :: INumeric
        integer | real(real64)
    end interface INumeric
contains
    function identity{INumeric :: T}(x) result(value)
        type(T), intent(in) :: x
        type(T) :: value
        value = x
    end function identity
    subroutine run_case()
        complex(real64) :: value
        value = identity((2.0_real64, 1.0_real64))
    end subroutine run_case
end module traits_numeric_a_n03_m

! A-N04: adding complex removes <; reject this unused generic at definition time.
module traits_numeric_a_n04_m
    use iso_fortran_env, only: real64
    implicit none
    abstract interface :: INumeric
        integer | real(real64) | complex(real64)
    end interface INumeric
contains
    function less_than{INumeric :: T}(x, y) result(answer)
        type(T), intent(in) :: x, y
        logical :: answer
        answer = x < y
    end function less_than
end module traits_numeric_a_n04_m

! A-N05: neither INT nor REAL accepts a logical source; the unused generic must fail.
module traits_numeric_a_n05_m
    use iso_fortran_env, only: real64
    implicit none
    abstract interface :: INumeric
        integer | real(real64)
    end interface INumeric
contains
    function invalid_cast{INumeric :: T}() result(value)
        type(T) :: value
        value = T(.true.)
    end function invalid_cast
end module traits_numeric_a_n05_m

! A-N06: explicit integer conflicts with an ordinary real(real64) actual.
module traits_numeric_a_n06_m
    use iso_fortran_env, only: real64
    implicit none
    abstract interface :: INumeric
        integer | real(real64)
    end interface INumeric
contains
    function identity{INumeric :: T}(x) result(value)
        type(T), intent(in) :: x
        type(T) :: value
        value = x
    end function identity
    subroutine run_case()
        integer :: value
        value = identity{integer}(2.0_real64)
    end subroutine run_case
end module traits_numeric_a_n06_m

! A-N07: neither integer n nor the assignment context determines T.
module traits_numeric_a_n07_m
    use iso_fortran_env, only: real64
    implicit none
    abstract interface :: INumeric
        integer | real(real64)
    end interface INumeric
contains
    function from_integer{INumeric :: T}(n) result(value)
        integer, intent(in) :: n
        type(T) :: value
        value = T(n)
    end function from_integer
    subroutine run_case()
        real(real64) :: value
        value = from_integer(2)
    end subroutine run_case
end module traits_numeric_a_n07_m

! A-N08: a type-set trait cannot declare a runtime class(...) object.
module traits_numeric_a_n08_m
    use iso_fortran_env, only: real64
    implicit none
    abstract interface :: INumeric
        integer | real(real64)
    end interface INumeric
contains
    subroutine consume()
        class(INumeric), pointer :: value
    end subroutine consume
end module traits_numeric_a_n08_m

! A-N09: type(INumeric) is not a concrete union variable.
module traits_numeric_a_n09_m
    use iso_fortran_env, only: real64
    implicit none
    abstract interface :: INumeric
        integer | real(real64)
    end interface INumeric
    type(INumeric) :: value
end module traits_numeric_a_n09_m

! A-N10: a derived type cannot manually implement a type-set trait.
module traits_numeric_a_n10_m
    use iso_fortran_env, only: real64
    implicit none
    abstract interface :: INumeric
        integer | real(real64)
    end interface INumeric
    type :: Box
        integer :: value
    end type Box
    implements INumeric :: Box
    end implements Box
end module traits_numeric_a_n10_m

module traits_numeric_b1_n01_m
    implicit none
contains
    function identity{integer | real(8) :: T}(x) result(r)
        type(T), intent(in) :: x
        type(T) :: r
        r = x
    end function
    subroutine probe()
        integer(8) :: r
        ! B1-N01: exact membership excludes an integer of another kind.
        r = identity(1_8)
    end subroutine
end module traits_numeric_b1_n01_m

module traits_numeric_b1_n02_m
    implicit none
contains
    function identity{integer | real(8) :: T}(x) result(r)
        type(T), intent(in) :: x
        type(T) :: r
        r = x
    end function
    subroutine probe()
        real :: r
        ! B1-N02: default real is not real(8).
        r = identity(1.0)
    end subroutine
end module traits_numeric_b1_n02_m

module traits_numeric_b1_n03_m
    implicit none
contains
    function identity{integer | real(8) :: T}(x) result(r)
        type(T), intent(in) :: x
        type(T) :: r
        r = x
    end function
    subroutine probe()
        complex(8) :: r
        ! B1-N03: a valid copy does not grant membership to another category.
        r = identity((1.d0, 2.d0))
    end subroutine
end module traits_numeric_b1_n03_m

module traits_numeric_b1_n04_m
    implicit none
contains
    function unused{integer | real(8) | complex(8) :: T}(x, y) result(r)
        type(T), intent(in) :: x, y
        logical :: r
        ! B1-N04: every member must support ordering, even without a call.
        r = x < y
    end function
end module traits_numeric_b1_n04_m

module traits_numeric_b1_n05_m
    implicit none
contains
    function unused{integer | real(8) :: T}(x) result(r)
        type(T), intent(in) :: x
        type(T) :: r
        ! B1-N05: a logical is not a numeric conversion source.
        r = T(.true.)
    end function
end module traits_numeric_b1_n05_m

module traits_numeric_b1_n06_m
    implicit none
contains
    function identity{integer | real(8) :: T}(x) result(r)
        type(T), intent(in) :: x
        type(T) :: r
        r = x
    end function
    subroutine probe()
        real(8) :: r
        ! B1-N06: explicit type actuals cannot cast a conflicting ordinary actual.
        r = identity{integer}(1.d0)
    end subroutine
end module traits_numeric_b1_n06_m

module traits_numeric_b1_n07_m
    implicit none
    abstract interface :: IValue
        function value() result(r)
            integer :: r
        end function
    end interface
contains
    ! B1-N07: a bare intrinsic singleton cannot be composed with a nominal trait.
    function unused{IValue + integer :: T}(x) result(r)
        type(T), intent(in) :: x
        type(T) :: r
        r = x
    end function
end module traits_numeric_b1_n07_m

module traits_numeric_b1_n08_m
    implicit none
contains
    function first{integer | real(8) :: T}(x) result(r)
        type(T), intent(in) :: x
        type(T) :: r
        ! B1-N08: equal lists do not intern independently declared constraints.
        r = second{T}(x)
    end function
    function second{integer | real(8) :: U}(x) result(r)
        type(U), intent(in) :: x
        type(U) :: r
        r = x
    end function
end module traits_numeric_b1_n08_m

module traits_numeric_b1_n09_m
    implicit none
contains
    function identity{integer(8) :: T}(x) result(r)
        type(T), intent(in) :: x
        type(T) :: r
        r = x
    end function
    subroutine probe()
        integer :: r
        ! B1-N09: singleton constraints use the same exact-membership checker.
        r = identity(1)
    end subroutine
end module traits_numeric_b1_n09_m

! R0/R1: borrowed runtime views, nominal packing and ordinary dynamic calls.

module traits_runtime_n01_m
    implicit none
    abstract interface :: IValue
        function value() result(r)
            integer :: r
        end function value
    end interface IValue
contains
    subroutine invalid_local()
        class(IValue) :: object
    end subroutine invalid_local
end module traits_runtime_n01_m

module traits_runtime_n02_m
    implicit none
    abstract interface :: IValue
        function value() result(r)
            integer :: r
        end function value
    end interface IValue
    type :: Box
        integer :: payload
    contains
        procedure :: value => box_value
    end type Box
contains
    function box_value(self) result(r)
        class(Box), intent(in) :: self
        integer :: r
        r = self%payload
    end function box_value
    subroutine consume(object)
        class(IValue), intent(in) :: object
    end subroutine consume
    subroutine invalid_nominal()
        type(Box) :: object
        object%payload = 7
        call consume(object)
    end subroutine invalid_nominal
end module traits_runtime_n02_m

module traits_runtime_n03_m
    implicit none
    abstract interface :: IValue
        function value() result(r)
            integer :: r
        end function value
    end interface IValue
contains
    function invalid_member(object) result(r)
        class(IValue), intent(in) :: object
        integer :: r
        r = object%secret()
    end function invalid_member
end module traits_runtime_n03_m

module traits_runtime_n08_m
    implicit none
    abstract interface :: IValue
        function value() result(r)
            integer :: r
        end function value
    end interface IValue
contains
    subroutine requires_definition(object)
        class(*), intent(inout) :: object
    end subroutine requires_definition
    subroutine invalid_actual(object)
        class(IValue), intent(in) :: object
        call requires_definition(object)
    end subroutine invalid_actual
end module traits_runtime_n08_m

module traits_runtime_n11_contracts_m
    implicit none
    abstract interface :: IValue
        function value() result(r)
            integer :: r
        end function value
    end interface IValue
    type :: Box
        integer :: payload
    end type Box
end module traits_runtime_n11_contracts_m

module traits_runtime_n11_a_m
    use traits_runtime_n11_contracts_m, only: IValue, Box
    implicit none
    implements IValue :: Box
        procedure, pass :: value => a_value
    end implements Box
contains
    function a_value(self) result(r)
        class(Box), intent(in) :: self
        integer :: r
        r = self%payload
    end function a_value
end module traits_runtime_n11_a_m

module traits_runtime_n11_b_m
    use traits_runtime_n11_contracts_m, only: IValue, Box
    implicit none
    implements IValue :: Box
        procedure, pass :: value => b_value
    end implements Box
contains
    function b_value(self) result(r)
        class(Box), intent(in) :: self
        integer :: r
        r = -self%payload
    end function b_value
end module traits_runtime_n11_b_m

module traits_runtime_n11_consumer_m
    use traits_runtime_n11_a_m
    use traits_runtime_n11_b_m
    implicit none
contains
    function observe(object) result(r)
        class(IValue), intent(in) :: object
        integer :: r
        r = object%value()
    end function observe
    function invalid_pack() result(r)
        type(Box) :: object
        integer :: r
        object%payload = 7
        r = observe(object)
    end function invalid_pack
end module traits_runtime_n11_consumer_m

module traits_runtime_n11_reverse_m
    use traits_runtime_n11_b_m
    use traits_runtime_n11_a_m
    implicit none
contains
    function observe(object) result(r)
        class(IValue), intent(in) :: object
        integer :: r
        r = object%value()
    end function observe
    function invalid_pack() result(r)
        type(Box) :: object
        integer :: r
        object%payload = 7
        r = observe(object)
    end function invalid_pack
end module traits_runtime_n11_reverse_m

module traits_runtime_nyi_pointer_m
    implicit none
    abstract interface :: IValue
        function value() result(r)
            integer :: r
        end function
    end interface
    class(IValue), pointer :: object
end module traits_runtime_nyi_pointer_m

module traits_runtime_nyi_array_m
    implicit none
    abstract interface :: IValue
        function value() result(r)
            integer :: r
        end function
    end interface
contains
    subroutine array_view(object)
        class(IValue), intent(in) :: object(:)
    end subroutine
end module traits_runtime_nyi_array_m

module traits_runtime_nyi_mutable_m
    implicit none
    abstract interface :: IValue
        function value() result(r)
            integer :: r
        end function
    end interface
contains
    subroutine mutable_view(object)
        class(IValue), intent(inout) :: object
    end subroutine
end module traits_runtime_nyi_mutable_m

module traits_runtime_nyi_conversion_m
    implicit none
    abstract interface :: IValue
        function value() result(r)
            integer :: r
        end function
    end interface
    type :: Payload
        integer :: n
    end type
    implements IValue :: Payload
        procedure, pass :: value => get_value
    end implements
contains
    function get_value(self) result(r)
        class(Payload), intent(in) :: self
        integer :: r
        r = self%n
    end function
    subroutine consume(object)
        class(IValue), intent(in) :: object
    end subroutine
    subroutine polymorphic_source(object)
        class(Payload), intent(in) :: object
        call consume(object)
    end subroutine
    subroutine concrete_inspection(object)
        class(IValue), intent(in) :: object
        select type(object)
        type is(Payload)
            print *, object%n
        end select
    end subroutine
end module traits_runtime_nyi_conversion_m

module traits_runtime_strengthening_m
    implicit none
    abstract interface :: IValue
        function value() result(r)
            integer :: r
        end function
    end interface
    abstract interface, extends(IValue) :: IChild
    end interface
contains
    subroutine consume(object)
        class(IChild), intent(in) :: object
    end subroutine
    subroutine project(object)
        class(IValue), intent(in) :: object
        call consume(object)
    end subroutine
end module traits_runtime_strengthening_m

module traits_runtime_combination_unknown_m
    implicit none
    abstract interface :: IValue
        function value() result(r)
            integer :: r
        end function
    end interface
    abstract interface :: ITag
        function tag() result(r)
            integer :: r
        end function
    end interface
contains
    subroutine combined_view(object)
        class(IValue + IMissing), intent(in) :: object
    end subroutine
    function combined_function(object) result(r)
        class(IMissing + ITag), intent(in) :: object
        integer :: r
        r = 0
    end function
end module traits_runtime_combination_unknown_m

module traits_runtime_impure_function_m
    implicit none
    abstract interface :: IValue
        function value() result(r)
            integer :: r
        end function
    end interface
contains
    pure function observe(object) result(r)
        class(IValue), intent(in) :: object
        integer :: r
        r = object%value()
    end function
end module

module traits_runtime_impure_subroutine_m
    implicit none
    abstract interface :: IAction
        subroutine act()
        end subroutine
    end interface
contains
    pure subroutine observe(object)
        class(IAction), intent(in) :: object
        call object%act()
    end subroutine
end module

module traits_runtime_bindc_contract_m
    implicit none
    abstract interface :: IValue
        function value(n) result(r) bind(c)
            integer, value, intent(in) :: n
            integer :: r
        end function
    end interface
contains
    subroutine consume(object)
        class(IValue), intent(in) :: object
    end subroutine
end module

module traits_runtime_bindc_implementation_m
    implicit none
    abstract interface :: IValue
        function value(n) result(r)
            integer, value, intent(in) :: n
            integer :: r
        end function
    end interface
    type :: Payload
        integer :: n
    end type
    implements IValue :: Payload
        procedure, nopass :: value => c_value
    end implements
contains
    function c_value(n) result(r) bind(c, name="traits_runtime_bindc_implementation_value")
        integer, value, intent(in) :: n
        integer :: r
        r = n + 19
    end function
    function observe(object) result(r)
        class(IValue), intent(in) :: object
        integer :: r
        r = object%value(4)
    end function
    function invalid_pack() result(r)
        type(Payload) :: object
        integer :: r
        r = observe(object)
    end function
end module

module traits_runtime_completed_attributes_m
    implicit none
    abstract interface :: IValue
        function value() result(r)
            integer :: r
        end function
    end interface
contains
    subroutine split_optional(object)
        class(IValue), intent(in) :: object
        optional :: object
    end subroutine
    function split_optional_function(object) result(r)
        class(IValue), intent(in) :: object
        optional :: object
        integer :: r
        r = 0
    end function
    subroutine split_out(object)
        class(IValue) :: object
        intent(out) :: object
    end subroutine
    subroutine split_inout(object)
        class(IValue) :: object
        intent(inout) :: object
    end subroutine
    subroutine split_save(object)
        class(IValue), intent(in) :: object
        save :: object
    end subroutine
    subroutine split_allocatable(object)
        class(IValue), intent(in) :: object
        allocatable :: object
    end subroutine
    subroutine split_array(object)
        class(IValue), intent(in) :: object
        dimension :: object(2)
    end subroutine
    subroutine missing_intent(object)
        class(IValue) :: object
    end subroutine
    subroutine split_value(object)
        class(IValue), intent(in) :: object
        value :: object
    end subroutine
    subroutine split_pointer(object)
        class(IValue), intent(in) :: object
        pointer :: object
    end subroutine
end module

module traits_owning_boundaries_m
    implicit none
    abstract interface :: IValue
        function value() result(r)
            integer :: r
        end function
    end interface
    type :: Item
        integer :: n = 7
    end type
    type :: NoConformance
        integer :: n = 7
    end type
    implements IValue :: Item
        procedure, pass :: value => read_item
    end implements
contains
    function read_item(self) result(r)
        class(Item), intent(in) :: self
        integer :: r
        r = self%n
    end function
end module

module traits_owning_escape_boundary_m
    use traits_owning_boundaries_m
contains
    function unsupported_factory() result(owner)
        class(IValue), allocatable :: owner
        allocate(Item :: owner)
    end function
    subroutine unsupported_out_slot(owner)
        class(IValue), allocatable, intent(out) :: owner
    end subroutine
    subroutine unsupported_inout_slot(owner)
        class(IValue), allocatable, intent(inout) :: owner
    end subroutine
    subroutine unsupported_in_slot(owner)
        class(IValue), allocatable, intent(in) :: owner
    end subroutine
end module

module traits_owning_body_boundary_m
    use traits_owning_boundaries_m
contains
    subroutine allocation_needs_type()
        class(IValue), allocatable :: owner
        allocate(owner)
    end subroutine
    subroutine allocation_needs_conformance()
        class(IValue), allocatable :: owner
        type(NoConformance) :: source
        allocate(owner, source=source)
    end subroutine
    subroutine assignment_needs_conformance()
        class(IValue), allocatable :: owner
        type(NoConformance) :: source
        owner = source
    end subroutine
    subroutine unsupported_allocation_status()
        class(IValue), allocatable :: owner
        integer :: status
        allocate(Item :: owner, stat=status)
    end subroutine
    subroutine unsupported_deallocation_status()
        class(IValue), allocatable :: owner
        character(80) :: message
        deallocate(owner, errmsg=message)
    end subroutine
    subroutine unsupported_multiple_allocation()
        class(IValue), allocatable :: owner, copy
        allocate(Item :: owner, copy)
    end subroutine
    subroutine incompatible_initializers()
        class(IValue), allocatable :: owner
        type(Item) :: source
        allocate(owner, source=source, mold=source)
    end subroutine
    subroutine borrowed_storage_is_readonly(view)
        class(IValue), intent(in) :: view
        view = Item(9)
    end subroutine
    subroutine borrowed_storage_cannot_be_allocated(view)
        class(IValue), intent(in) :: view
        allocate(Item :: view)
    end subroutine
    pure subroutine unsupported_pure_lifecycle()
        class(IValue), allocatable :: owner
        allocate(Item :: owner)
    end subroutine
    subroutine unsupported_move()
        class(IValue), allocatable :: owner, copy
        call move_alloc(owner, copy)
    end subroutine
    subroutine no_inspection_yet()
        class(IValue), allocatable :: owner
        select type(owner)
        type is (Item)
        end select
    end subroutine
end module

module traits_owning_unowned_boundary_m
    use traits_owning_boundaries_m
contains
    subroutine no_implicit_owner()
        class(IValue) :: owner
    end subroutine
end module

module traits_owning_component_boundary_m
    use traits_owning_boundaries_m, only: IValue
    type :: Container
        class(IValue), allocatable :: owner
    end type
end module

module traits_inherited_component_bounds_m
    implicit none
    type Bounds
        integer :: extent(2), other(2)
    end type
    abstract interface :: ILeft
        function count(n, a) result(r)
            import :: Bounds
            type(Bounds), intent(in) :: n
            integer, intent(in) :: a(n%extent(1))
            integer :: r
        end function
    end interface
    abstract interface :: IRight
        function count(n, a) result(r)
            import :: Bounds
            type(Bounds), intent(in) :: n
            integer, intent(in) :: a(n%other(1))
            integer :: r
        end function
    end interface
    abstract interface, extends(ILeft + IRight) :: IChild
    end interface
end module

module traits_after_component_bounds_error_m
    integer :: marker = 17
end module

module traits_owning_ordinary_allocation_boundary_m
    use traits_owning_boundaries_m
contains
    subroutine erased_source()
        class(IValue), allocatable :: owner
        class(*), allocatable :: erased
        allocate(Item :: owner)
        allocate(erased, source=owner)
    end subroutine
    subroutine erased_mold()
        class(IValue), allocatable :: owner
        class(*), allocatable :: erased
        allocate(Item :: owner)
        allocate(erased, mold=owner)
    end subroutine
    subroutine declared_source()
        class(IValue), allocatable :: owner
        class(Item), allocatable :: ordinary
        allocate(Item :: owner)
        allocate(ordinary, source=owner)
    end subroutine
    subroutine declared_mold()
        class(IValue), allocatable :: owner
        class(Item), allocatable :: ordinary
        allocate(Item :: owner)
        allocate(ordinary, mold=owner)
    end subroutine
    subroutine borrowed_source(view)
        class(IValue), intent(in) :: view
        class(*), allocatable :: erased
        allocate(erased, source=view)
    end subroutine
    subroutine borrowed_mold(view)
        class(IValue), intent(in) :: view
        type(Item), allocatable :: ordinary
        allocate(ordinary, mold=view)
    end subroutine
end module

module traits_owning_pure_nested_boundary_m
    use traits_owning_boundaries_m
contains
    pure subroutine nested_allocation()
        block
            class(IValue), allocatable :: owner
            block
                allocate(Item :: owner)
            end block
        end block
    end subroutine
    pure subroutine nested_assignment()
        block
            class(IValue), allocatable :: owner
            block
                owner = Item(7)
            end block
        end block
    end subroutine
    pure subroutine nested_deallocation()
        block
            class(IValue), allocatable :: owner
            associate(marker => 1)
                block
                    deallocate(owner)
                end block
            end associate
        end block
    end subroutine
    pure subroutine associate_automatic_cleanup()
        associate(marker => 1)
            block
                class(IValue), allocatable :: owner
                allocate(Item :: owner)
            end block
        end associate
    end subroutine
    pure subroutine nested_message(view, result)
        class(IValue), intent(in) :: view
        integer, intent(out) :: result
        block
            associate(marker => 1)
                result = view%value()
            end associate
        end block
    end subroutine
end module

module traits_slot_invariance_m
    use traits_owning_boundaries_m, only: IValue, Item
    implicit none
    abstract interface, extends(IValue) :: IChild
    end interface
    abstract interface :: IEqual
        function value() result(r)
            integer :: r
        end function
    end interface
    abstract interface :: IMarker
        function marker() result(r)
            integer :: r
        end function
    end interface
    abstract interface, extends(IValue + IMarker) :: ICombined
    end interface
contains
    subroutine read_slot(slot)
        class(IValue), allocatable, intent(in) :: slot
    end subroutine
    subroutine write_slot(slot)
        class(IValue), allocatable, intent(inout) :: slot
    end subroutine
    subroutine replace_slot(slot)
        class(IValue), allocatable, intent(out) :: slot
    end subroutine
    subroutine unknown_slot(slot)
        class(IValue), allocatable :: slot
    end subroutine
    subroutine child_in()
        class(IChild), allocatable :: child
        call read_slot(child)
    end subroutine
    subroutine child_inout()
        class(IChild), allocatable :: child
        call write_slot(child)
    end subroutine
    subroutine child_out()
        class(IChild), allocatable :: child
        call replace_slot(child)
    end subroutine
    subroutine child_unspecified()
        class(IChild), allocatable :: child
        call unknown_slot(child)
    end subroutine
    subroutine combined_in()
        class(ICombined), allocatable :: combined
        call read_slot(combined)
    end subroutine
    subroutine equal_signature_is_not_same_contract()
        class(IEqual), allocatable :: other
        call read_slot(other)
    end subroutine
    subroutine concrete_slot()
        type(Item), allocatable :: concrete
        call read_slot(concrete)
    end subroutine
    subroutine borrowed_slot(view)
        class(IValue), intent(in) :: view
        call read_slot(view)
    end subroutine
    subroutine input_allocation(slot)
        class(IValue), allocatable, intent(in) :: slot
        allocate(Item :: slot)
    end subroutine
    subroutine input_assignment(slot)
        class(IValue), allocatable, intent(in) :: slot
        slot = Item(7)
    end subroutine
    subroutine input_deallocation(slot)
        class(IValue), allocatable, intent(in) :: slot
        deallocate(slot)
    end subroutine
    subroutine input_to_output(slot)
        class(IValue), allocatable, intent(in) :: slot
        call replace_slot(slot)
    end subroutine
    subroutine input_to_inout(slot)
        class(IValue), allocatable, intent(in) :: slot
        call write_slot(slot)
    end subroutine
    subroutine optional_slot(slot)
        class(IValue), allocatable, intent(in) :: slot
        optional :: slot
    end subroutine
    subroutine value_slot(slot)
        class(IValue), allocatable, intent(in), value :: slot
    end subroutine
    pure subroutine pure_output_cleanup(slot)
        class(IValue), allocatable, intent(out) :: slot
    end subroutine
    subroutine bindc_slot(slot) bind(c)
        class(IValue), allocatable, intent(inout) :: slot
    end subroutine
end module

module traits_result_boundaries_m
    use traits_owning_boundaries_m
    implicit none
contains
    function make() result(object)
        class(IValue), allocatable :: object
        allocate(Item :: object)
    end function
    subroutine read_slot(slot)
        class(IValue), allocatable, intent(in) :: slot
    end subroutine
    logical function has_value(slot)
        class(IValue), allocatable, intent(in) :: slot
        has_value = allocated(slot)
    end function
    pure subroutine observe(view)
        class(IValue), intent(in) :: view
    end subroutine
    function unowned_result() result(object)
        class(IValue) :: object
    end function
    function saved_result() result(object)
        class(IValue), allocatable :: object
        save :: object
    end function
    pure function pure_result() result(object)
        class(IValue), allocatable :: object
    end function
    function bindc_result() result(object) bind(c)
        class(IValue), allocatable :: object
    end function
    subroutine result_is_not_a_slot()
        call read_slot(make())
    end subroutine
    subroutine result_is_not_a_function_slot()
        if (has_value(make())) error stop
    end subroutine
    subroutine result_is_not_an_inquiry_variable()
        if (allocated(make())) error stop
    end subroutine
    pure subroutine nested_result_cleanup()
        block
            associate(marker => 1)
                call observe(make())
            end associate
        end block
    end subroutine
end module

module traits_indirect_slot_boundaries_m
    implicit none
    abstract interface :: IValue
        integer function value()
        end function
    end interface
    abstract interface, extends(IValue) :: IChild
    end interface
contains
    function make() result(object)
        class(IValue), allocatable :: object
    end function
    logical function has_value(slot)
        class(IValue), allocatable, intent(in) :: slot
        has_value = allocated(slot)
    end function
    subroutine child_slot()
        class(IChild), allocatable :: child
        procedure(has_value), pointer :: query
        query => has_value
        if (query(child)) error stop
    end subroutine
    subroutine borrowed_slot(view)
        class(IValue), intent(in) :: view
        procedure(has_value), pointer :: query
        query => has_value
        if (query(view)) error stop
    end subroutine
    subroutine result_actual()
        procedure(has_value), pointer :: query
        query => has_value
        if (query(make())) error stop
    end subroutine
    subroutine extra_actual()
        class(IValue), allocatable :: owner
        procedure(has_value), pointer :: query
        query => has_value
        if (query(owner, owner)) error stop
    end subroutine
end module

module traits_out_entry_effects_m
    implicit none
    abstract interface :: IValue
        integer function value()
        end function
    end interface
contains
    integer function clear(slot)
        class(IValue), allocatable, intent(out) :: slot
        clear = 1
    end function
    subroutine clear_sub(slot)
        class(IValue), allocatable, intent(out) :: slot
    end subroutine
    integer function wrapper(slot)
        class(IValue), allocatable, intent(inout) :: slot
        procedure(clear), pointer :: callback
        callback => clear
        wrapper = callback(slot)
    end function
    pure subroutine direct_output(slot)
        class(IValue), allocatable, intent(inout) :: slot
        block
            integer :: ignored
            ignored = clear(slot)
        end block
    end subroutine
    pure subroutine indirect_output(slot)
        class(IValue), allocatable, intent(inout) :: slot
        block
            procedure(clear), pointer :: callback
            integer :: ignored
            callback => clear
            ignored = callback(slot)
        end block
    end subroutine
    pure subroutine transitive_output(slot)
        class(IValue), allocatable, intent(inout) :: slot
        integer :: ignored
        associate(marker => 1)
            ignored = wrapper(slot)
        end associate
    end subroutine
    pure subroutine subroutine_output(slot)
        class(IValue), allocatable, intent(inout) :: slot
        associate(marker => 1)
            call clear_sub(slot)
        end associate
    end subroutine
end module

module traits_pointer_boundaries_m
    implicit none
    abstract interface :: IValue
        integer function value()
        end function
    end interface
    type :: Cell
        integer :: n
    end type
    implements IValue :: Cell
        procedure, pass :: value => read_cell
    end implements
    type(Cell), target :: shared
    class(IValue), pointer :: shared_view
contains
    integer function read_cell(self)
        class(Cell), intent(in) :: self
        read_cell = self%n
    end function
    subroutine change(view)
        class(IValue), pointer, intent(out) :: view
        nullify(view)
    end subroutine
    pure subroutine pure_change(view)
        class(IValue), pointer, intent(inout) :: view
        nullify(view)
    end subroutine
    subroutine missing_target()
        class(IValue), pointer :: view
        type(Cell) :: object
        view => object
    end subroutine
    subroutine readonly_pointer(view, object)
        class(IValue), pointer, intent(in) :: view
        type(Cell), target :: object
        view => object
        nullify(view)
        call change(view)
    end subroutine
    subroutine missing_pointer_actual(object)
        type(Cell), target :: object
        call change(object)
    end subroutine
    subroutine value_assignment(view, object)
        class(IValue), pointer :: view
        type(Cell) :: object
        view = object
    end subroutine
    subroutine nonpointer_inquiry(view)
        class(IValue), intent(in) :: view
        if (associated(view)) error stop
    end subroutine
    subroutine ordinary_pointer_inquiry(view, pointer)
        class(IValue), pointer :: view
        type(Cell), pointer :: pointer
        if (associated(pointer, view)) error stop
    end subroutine
    pure subroutine host_association()
        class(IValue), pointer :: local
        local => shared
        nullify(shared_view)
    end subroutine
    pure subroutine readonly_target(object)
        type(Cell), target, intent(in) :: object
        class(IValue), pointer :: local
        local => object
    end subroutine
    pure integer function function_pointer(view)
        class(IValue), pointer, intent(inout) :: view
        nullify(view)
        function_pointer = 0
    end function
    pure subroutine pointer_forwarding(view)
        class(IValue), pointer, intent(in) :: view
        call pure_change(view)
    end subroutine
    pure subroutine polymorphic_out(view)
        class(IValue), pointer, intent(out) :: view
    end subroutine
end module

module traits_projection_boundaries_m
    implicit none
    abstract interface :: IValue
        integer function value()
        end function
    end interface
    abstract interface, extends(IValue) :: IChild
    end interface
    abstract interface :: ILabel
        integer function label()
        end function
    end interface
    abstract interface, extends(IChild + ILabel) :: ICombined
    end interface
    abstract interface :: IUnrelated
        integer function value()
        end function
    end interface
contains
    subroutine borrow(view)
        class(IValue), intent(in) :: view
    end subroutine
    subroutine read_child(view)
        class(IChild), pointer, intent(in) :: view
    end subroutine
    subroutine read_slot(slot)
        class(IValue), allocatable, intent(in) :: slot
    end subroutine
    subroutine write_slot(slot)
        class(IValue), allocatable, intent(inout) :: slot
    end subroutine
    subroutine replace_slot(slot)
        class(IValue), allocatable, intent(out) :: slot
    end subroutine
    subroutine unknown_slot(slot)
        class(IValue), allocatable :: slot
    end subroutine
    subroutine write_pointer(slot)
        class(IValue), pointer, intent(inout) :: slot
    end subroutine
    subroutine replace_pointer(slot)
        class(IValue), pointer, intent(out) :: slot
    end subroutine
    subroutine unknown_pointer(slot)
        class(IValue), pointer :: slot
    end subroutine
    subroutine combined_owner_slots(combined)
        class(ICombined), allocatable :: combined
        call read_slot(combined)
        call write_slot(combined)
        call replace_slot(combined)
        call unknown_slot(combined)
    end subroutine
    subroutine defining_pointer_slots(child, combined)
        class(IChild), pointer :: child
        class(ICombined), pointer :: combined
        call write_pointer(child)
        call replace_pointer(child)
        call unknown_pointer(child)
        call write_pointer(combined)
        call replace_pointer(combined)
        call unknown_pointer(combined)
    end subroutine
    subroutine strengthening(parent, child, unrelated)
        class(IValue), pointer :: parent
        class(IChild), pointer :: child
        class(IUnrelated), pointer :: unrelated
        child => parent
        parent => unrelated
        call read_child(parent)
        call borrow(unrelated)
        if (associated(child, parent)) error stop
        if (associated(parent, unrelated)) error stop
        if (associated(parent, null(child))) error stop
    end subroutine
    subroutine transient_target(view)
        class(IChild), intent(in) :: view
        class(IValue), pointer :: pointer
        pointer => view
    end subroutine
    subroutine pointer_is_not_an_owner(child)
        class(IChild), pointer :: child
        call read_slot(child)
    end subroutine
end module

! Anonymous conjunctions preserve original nominal obligations and storage slots.
module traits_runtime_combination_negative_m
    implicit none
    abstract interface :: A
        integer function value()
        end function
    end interface
    abstract interface :: B
        integer function label()
        end function
    end interface
    abstract interface :: D
        integer function extra()
        end function
    end interface
    abstract interface :: Alias
        integer function value()
        end function
    end interface
    abstract interface, extends(A + B) :: Child
    end interface
    type :: Box
        integer :: n
    end type
    implements Child + D + Alias :: Box
        procedure, pass :: value => read_value
        procedure, nopass :: label => read_label
        procedure, nopass :: extra => read_extra
    end implements
contains
    integer function read_value(self)
        type(Box), intent(in) :: self
        read_value = self%n
    end function
    integer function read_label()
        read_label = 101
    end function
    integer function read_extra()
        read_extra = 202
    end function
    subroutine need_combination(view)
        class(A + B), intent(in) :: view
    end subroutine
    subroutine need_alias(view)
        class(Alias), intent(in) :: view
    end subroutine
    subroutine need_child(view)
        class(Child), pointer, intent(in) :: view
    end subroutine
    subroutine need_named(view)
        class(Child), intent(in) :: view
    end subroutine
    subroutine cannot_strengthen(view, duplicate)
        class(A), intent(in) :: view
        class(A + A), intent(in) :: duplicate
        call need_combination(view)
        call need_alias(duplicate)
    end subroutine
    subroutine cannot_discover(view)
        class(A + B), pointer :: view
        class(Child), pointer :: named
        class(A + B + D), pointer :: richer
        class(A + Alias), pointer :: independent
        class(Child), allocatable :: owned
        named => view
        richer => view
        independent => view
        call need_child(view)
        call need_named(view)
        owned = view
        allocate(owned, source=view)
    end subroutine
    subroutine slot_in(view)
        class(A + B), allocatable, intent(in) :: view
    end subroutine
    subroutine slot_inout(view)
        class(B + A), allocatable, intent(inout) :: view
    end subroutine
    subroutine slot_out(view)
        class(A + B), allocatable, intent(out) :: view
    end subroutine
    subroutine slot_unspecified(view)
        class(B + A), allocatable :: view
    end subroutine
    subroutine invariant_owners(named, richer)
        class(Child), allocatable :: named
        class(A + B + D), allocatable :: richer
        call slot_in(named)
        call slot_inout(named)
        call slot_out(named)
        call slot_unspecified(named)
        call slot_in(richer)
        call slot_inout(richer)
        call slot_out(richer)
        call slot_unspecified(richer)
    end subroutine
    subroutine pointer_inout(view)
        class(B + A), pointer, intent(inout) :: view
    end subroutine
    subroutine pointer_out(view)
        class(A + B), pointer, intent(out) :: view
    end subroutine
    subroutine pointer_unspecified(view)
        class(B + A), pointer :: view
    end subroutine
    subroutine invariant_pointers(named, richer)
        class(Child), pointer :: named
        class(A + B + D), pointer :: richer
        call pointer_inout(named)
        call pointer_out(named)
        call pointer_unspecified(named)
        call pointer_inout(richer)
        call pointer_out(richer)
        call pointer_unspecified(richer)
    end subroutine
    subroutine readonly_pointer(view, source)
        class(A + B), pointer, intent(in) :: view
        type(Box), target, intent(in) :: source
        view => source
        nullify(view)
    end subroutine
end module

module traits_runtime_combination_missing_m
    use traits_runtime_combination_negative_m, only: A, B
    implicit none
    type :: OnlyA
        integer :: n
    end type
    implements A :: OnlyA
        procedure, pass :: value => read_value
    end implements
contains
    integer function read_value(self)
        type(OnlyA), intent(in) :: self
        read_value = self%n
    end function
    subroutine construct(object)
        type(OnlyA), target :: object
        class(A + B), pointer :: view
        class(A + B), allocatable :: owner
        view => object
        owner = object
        allocate(OnlyA :: owner)
    end subroutine
end module

module traits_runtime_combination_conflicting_m
    use traits_runtime_combination_negative_m, only: A, Alias
    implicit none
    type :: Box
        integer :: n
    end type
    implements A :: Box
        procedure, pass :: value => first
    end implements
    implements Alias :: Box
        procedure, pass :: value => second
    end implements
contains
    integer function first(self)
        type(Box), intent(in) :: self
        first = self%n
    end function
    integer function second(self)
        type(Box), intent(in) :: self
        second = self%n + 100
    end function
    subroutine construct(object)
        type(Box), target :: object
        class(A + Alias), pointer :: view
        view => object
    end subroutine
end module

module traits_runtime_combination_signature_m
    use traits_runtime_combination_negative_m, only: A
    implicit none
    abstract interface :: IPure
        pure integer function value()
        end function
    end interface
    class(A + IPure), pointer :: conflict
end module

module traits_runtime_combination_declarations_m
    use traits_runtime_combination_negative_m, only: A, B, Box
    implicit none
    abstract interface :: INumeric
        integer | real
    end interface
    class(A + Box), pointer :: not_a_trait
    class(A + INumeric), pointer :: not_universal
    type :: Holder
        class(A + B), pointer :: component
    end type
contains
    subroutine arrays(view)
        class(A + B), intent(in) :: view(:)
    end subroutine
    subroutine bare_local()
        class(A + B) :: view
    end subroutine
    function pointer_result() result(view)
        class(A + B), pointer :: view
    end function
end module

module traits_runtime_inspection_negative_m
    implicit none
    abstract interface :: IValue
        integer function value()
        end function
    end interface
    abstract interface, extends(IValue) :: IChild
    end interface
    type :: Cell
        integer :: n
        integer, pointer :: link
        integer, allocatable :: data(:)
    end type
    class(IValue), pointer :: shared
contains
    subroutine output(value)
        integer, intent(out) :: value
    end subroutine
    integer function define(value)
        integer, intent(out) :: value
        define = 0
    end function
    subroutine pointer_slot(value)
        type(Cell), pointer, intent(inout) :: value
    end subroutine
    subroutine allocation_slot(value)
        type(Cell), allocatable, intent(in) :: value
    end subroutine
    subroutine no_pointer_nullify(view)
        class(IValue), pointer :: view
        select type (concrete => view)
        type is (Cell)
            nullify(concrete)
        end select
    end subroutine
    subroutine no_pointer_assignment(view, target)
        class(IValue), pointer :: view
        type(Cell), target :: target
        select type (concrete => view)
        type is (Cell)
            concrete => target
        end select
    end subroutine
    subroutine no_pointer_slot(view)
        class(IValue), pointer :: view
        select type (concrete => view)
        type is (Cell)
            call pointer_slot(concrete)
        end select
    end subroutine
    subroutine no_pointer_inquiry(view)
        class(IValue), pointer :: view
        select type (concrete => view)
        type is (Cell)
            if (associated(concrete)) error stop
        end select
    end subroutine
    subroutine no_allocate(view)
        class(IValue), allocatable :: view
        select type (concrete => view)
        type is (Cell)
            allocate(concrete)
        end select
    end subroutine
    subroutine no_deallocate(view)
        class(IValue), allocatable :: view
        select type (concrete => view)
        type is (Cell)
            deallocate(concrete)
        end select
    end subroutine
    subroutine no_allocation_slot(view)
        class(IValue), allocatable :: view
        select type (concrete => view)
        type is (Cell)
            call allocation_slot(concrete)
        end select
    end subroutine
    subroutine no_allocated_inquiry(view)
        class(IValue), allocatable :: view
        select type (concrete => view)
        type is (Cell)
            if (allocated(concrete)) error stop
        end select
    end subroutine
    subroutine readonly_component(view)
        class(IValue), intent(in) :: view
        select type (concrete => view)
        type is (Cell)
            concrete%n = 1
        end select
    end subroutine
    subroutine readonly_whole(view, value)
        class(IValue), intent(in) :: view
        type(Cell), intent(in) :: value
        select type (concrete => view)
        type is (Cell)
            concrete = value
        end select
    end subroutine
    subroutine readonly_output(view)
        class(IValue), intent(in) :: view
        select type (concrete => view)
        type is (Cell)
            call output(concrete%n)
        end select
    end subroutine
    subroutine readonly_function_output(view)
        class(IValue), intent(in) :: view
        integer :: n
        select type (concrete => view)
        type is (Cell)
            n = define(concrete%n)
        end select
    end subroutine
    subroutine readonly_allocate(view)
        class(IValue), intent(in) :: view
        select type (concrete => view)
        type is (Cell)
            allocate(concrete%data(2))
        end select
    end subroutine
    subroutine readonly_deallocate(view)
        class(IValue), intent(in) :: view
        select type (concrete => view)
        type is (Cell)
            deallocate(concrete%data)
        end select
    end subroutine
    subroutine readonly_nullify(view)
        class(IValue), intent(in) :: view
        select type (concrete => view)
        type is (Cell)
            nullify(concrete%link)
        end select
    end subroutine
    subroutine readonly_read(view)
        class(IValue), intent(in) :: view
        select type (concrete => view)
        type is (Cell)
            read *, concrete%n
        end select
    end subroutine
    subroutine readonly_nested_select(view)
        class(IValue), intent(in) :: view
        select type (concrete => view)
        class is (Cell)
            select type (nested => concrete)
            type is (Cell)
                nested%n = 1
            end select
        end select
    end subroutine
    subroutine readonly_nested_associate(view)
        class(IValue), intent(in) :: view
        select type (concrete => view)
        type is (Cell)
            associate (nested => concrete)
                nested%n = 1
            end associate
        end select
    end subroutine
    function make() result(owner)
        class(IValue), allocatable :: owner
    end function
    subroutine readonly_result()
        select type (concrete => make())
        type is (Cell)
            concrete%n = 1
        end select
    end subroutine
    pure subroutine readonly_pure_pointer(view)
        class(IValue), pointer, intent(in) :: view
        select type (concrete => view)
        type is (Cell)
            concrete%n = 1
        end select
    end subroutine
    pure subroutine readonly_pure_host()
        select type (concrete => shared)
        type is (Cell)
            concrete%n = 1
        end select
    end subroutine
    pure integer function readonly_pure_function(view)
        class(IValue), pointer, intent(inout) :: view
        select type (concrete => view)
        type is (Cell)
            concrete%n = 1
        end select
        readonly_pure_function = 0
    end function
    subroutine not_type_conformance(view)
        class(IValue), pointer :: view
        select type (concrete => view)
        type is (IChild)
        end select
    end subroutine
    subroutine not_class_conformance(view)
        class(IValue), pointer :: view
        select type (concrete => view)
        class is (IChild)
        end select
    end subroutine
    subroutine no_intrinsic_guard(view)
        class(IValue), pointer :: view
        select type (concrete => view)
        type is (integer)
        end select
    end subroutine
    subroutine duplicate_guard(view)
        class(IValue), pointer :: view
        select type (concrete => view)
        type is (Cell)
        type is (Cell)
        end select
    end subroutine
    subroutine default_not_pointer(view)
        class(IValue), pointer :: view
        select type (concrete => view)
        class default
            nullify(concrete)
        end select
    end subroutine
    subroutine default_not_allocatable(view)
        class(IValue), allocatable :: view
        select type (concrete => view)
        class default
            allocate(concrete)
        end select
    end subroutine
    subroutine not_target(view)
        class(IValue), allocatable :: view
        type(Cell), pointer :: pointer
        select type (concrete => view)
        type is (Cell)
            pointer => concrete
        end select
    end subroutine
    subroutine result_not_target()
        type(Cell), pointer :: pointer
        select type (concrete => make())
        type is (Cell)
            pointer => concrete
        end select
    end subroutine
    subroutine readonly_nested_component(view)
        class(IValue), intent(in) :: view
        select type (concrete => view)
        type is (Cell)
            associate (n => concrete%n)
                call output(n)
            end associate
        end select
    end subroutine
    subroutine readonly_allocation_status(view)
        class(IValue), intent(in) :: view
        integer, allocatable :: local
        select type (concrete => view)
        type is (Cell)
            allocate(local, stat=concrete%n)
        end select
    end subroutine
end module

! A generic implementation cannot narrow a universally promised nominal domain.
module traits_generic_method_narrowed
    implicit none
    abstract interface :: IValue
        integer function value()
        end function
    end interface
    abstract interface, extends(IValue) :: IMore
        integer function extra()
        end function
    end interface
    abstract interface :: IAlgorithm
        function apply{IValue :: T}(object) result(r)
            type(T), intent(in) :: object
            integer :: r
        end function
    end interface
    type :: Algorithm
    end type
    implements IAlgorithm :: Algorithm
        procedure, nopass :: apply
    end implements
contains
    function apply{IMore :: Element}(object) result(r)
        type(Element), intent(in) :: object
        integer :: r
        r = object%extra()
    end function
end module

! One concrete argument type is not an implementation of a universal method.
module traits_generic_method_missing_binder
    implicit none
    abstract interface :: IValue
        integer function value()
        end function
    end interface
    abstract interface :: IAlgorithm
        function apply{IValue :: T}(object) result(r)
            type(T), intent(in) :: object
            integer :: r
        end function
    end interface
    type :: Algorithm
    end type
    type :: Value
        integer :: n
    end type
    implements IAlgorithm :: Algorithm
        procedure, nopass :: apply
    end implements
contains
    integer function apply(object)
        type(Value), intent(in) :: object
        apply = object%n
    end function
end module

! Equal constraints do not identify distinct positional binders.
module traits_generic_method_swapped_binders
    implicit none
    abstract interface :: IValue
        integer function value()
        end function
    end interface
    abstract interface :: IAlgorithm
        function apply{IValue :: T, IValue :: U}(left, right) result(r)
            type(T), intent(in) :: left
            type(U), intent(in) :: right
            integer :: r
        end function
    end interface
    type :: Algorithm
    end type
    implements IAlgorithm :: Algorithm
        procedure, nopass :: apply
    end implements
contains
    function apply{IValue :: X, IValue :: Y}(left, right) result(r)
        type(Y), intent(in) :: left
        type(X), intent(in) :: right
        integer :: r
        r = left%value() + right%value()
    end function
end module

! Check generic implementation bodies even without any calls or runtime view.
module traits_generic_method_unused_body
    implicit none
    abstract interface :: IValue
        integer function value()
        end function
    end interface
    abstract interface :: IAlgorithm
        function apply{IValue :: T}(object) result(r)
            type(T), intent(in) :: object
            integer :: r
        end function
    end interface
    type :: Algorithm
    end type
    implements IAlgorithm :: Algorithm
        procedure, nopass :: apply
    end implements
contains
    function apply{IValue :: Element}(object) result(r)
        type(Element), intent(in) :: object
        integer :: r
        r = object%undeclared()
    end function
end module

! Erased generic storage/result/mutation semantics are not inferred from a view.
module traits_generic_runtime_boundaries
    implicit none
    abstract interface :: IValue
        integer function value()
        end function
    end interface
    abstract interface :: IArray
        function apply{IValue :: T}(object) result(r)
            type(T), intent(in) :: object(:)
            integer :: r
        end function
    end interface
    abstract interface :: IMutable
        function apply{IValue :: T}(object) result(r)
            type(T), intent(inout) :: object
            integer :: r
        end function
    end interface
    abstract interface :: IOwning
        function apply{IValue :: T}(object) result(r)
            type(T), allocatable, intent(in) :: object
            integer :: r
        end function
    end interface
    abstract interface :: IResult
        function apply{IValue :: T}(object) result(r)
            type(T), intent(in) :: object
            type(T) :: r
        end function
    end interface
    class(IArray), allocatable :: array_method
    class(IMutable), allocatable :: mutable_method
    class(IOwning), allocatable :: owning_method
    class(IResult), allocatable :: generic_result
end module

! Runtime generic calls still require nominal and type-argument agreement.
module traits_generic_runtime_bad_actual
    implicit none
    abstract interface :: IValue
        integer function value()
        end function
    end interface
    abstract interface :: IAlgorithm
        function apply{IValue :: T}(object) result(r)
            type(T), intent(in) :: object
            integer :: r
        end function
    end interface
    type :: Good
        integer :: n
    end type
    type :: Bad
        integer :: n
    end type
    implements IValue :: Good
        procedure, nopass :: value
    end implements
contains
    integer function value()
        value = 7
    end function
    subroutine bad_calls(algorithm, object)
        class(IAlgorithm), intent(in) :: algorithm
        type(Bad), intent(in) :: object
        integer :: r
        r = algorithm%apply(object)
        r = algorithm%apply{Good}(object)
        r = algorithm%apply(1)
    end subroutine
end module

! A checked generic local cannot silently become a borrowed descriptor.
module traits_generic_runtime_local_storage
    implicit none
    abstract interface :: IValue
        integer function value()
        end function
    end interface
    abstract interface :: IAlgorithm
        function apply{IValue :: T}(object) result(r)
            type(T), intent(in) :: object
            integer :: r
        end function
    end interface
    type :: Algorithm
    end type
    implements IAlgorithm :: Algorithm
        procedure, nopass :: apply
    end implements
contains
    function apply{IValue :: T}(object) result(r)
        type(T), intent(in) :: object
        type(T) :: scratch
        integer :: r
        r = object%value()
    end function
end module

module traits_generic_nominal_left
    implicit none
    abstract interface :: IValue
    end interface
    abstract interface :: IAlgorithm
        function apply{IValue :: T}(object) result(r)
            type(T), intent(in) :: object
            integer :: r
        end function
    end interface
end module

module traits_generic_nominal_right
    implicit none
    abstract interface :: IValue
    end interface
end module

! Same spelling and an equal empty method set do not identify nominal domains.
module traits_generic_runtime_nominal_mismatch
    use traits_generic_nominal_left, only: IAlgorithm
    use traits_generic_nominal_right, only: IValue
    implicit none
    type :: Algorithm
    end type
    implements IAlgorithm :: Algorithm
        procedure, nopass :: apply
    end implements
contains
    function apply{IValue :: Renamed}(object) result(r)
        type(Renamed), intent(in) :: object
        integer :: r
        r = 0
    end function
end module

! A finite intrinsic type set cannot implement a universal nominal method.
module traits_generic_runtime_finite_narrowing
    use traits_generic_nominal_left, only: IAlgorithm
    implicit none
    type :: Algorithm
    end type
    implements IAlgorithm :: Algorithm
        procedure, nopass :: apply
    end implements
contains
    function apply{integer | real(8) :: T}(object) result(r)
        type(T), intent(in) :: object
        integer :: r
        r = 0
    end function
end module

! The first erased provider ABI is module-owned, not a local closure ABI.
program traits_generic_runtime_local_provider
    use traits_generic_nominal_left, only: IValue, IAlgorithm
    implicit none
    type :: Algorithm
    end type
    implements IAlgorithm :: Algorithm
        procedure, nopass :: apply
    end implements
contains
    function apply{IValue :: T}(object) result(r)
        type(T), intent(in) :: object
        integer :: r
        r = 0
    end function
end program

! A helper's generic result cannot acquire borrowed-view result semantics.
module traits_generic_runtime_helper_result
    use traits_generic_nominal_left, only: IValue, IAlgorithm
    implicit none
    type :: Algorithm
    end type
    implements IAlgorithm :: Algorithm
        procedure, nopass :: apply
    end implements
contains
    function apply{IValue :: T}(object) result(r)
        type(T), intent(in) :: object
        integer :: r
        r = consume(identity(object))
    end function
    function identity{IValue :: U}(object) result(copy)
        type(U), intent(in) :: object
        type(U) :: copy
        copy = object
    end function
    function consume{IValue :: V}(object) result(r)
        type(V), intent(in) :: object
        integer :: r
        r = 0
    end function
end module

! R3G-01: every defining I/O specifier preserves the selector's readonly state.
module traits_inspection_readonly_io
    implicit none
    abstract interface :: IValue
        integer function value()
        end function
    end interface
    type :: Box
        integer :: n
        logical :: flag
        character(80) :: text
    end type
contains
    subroutine inspect(view)
        class(IValue), intent(in) :: view
        integer :: item
        select type (concrete => view)
        type is (Box)
            inquire(iolength=concrete%n) 7
            inquire(unit=20, number=concrete%n)
            inquire(unit=20, opened=concrete%flag)
            inquire(unit=20, name=concrete%text)
            inquire(unit=20, name=concrete%text(1:10))
            inquire(unit=20, iomsg=concrete%text)
            open(newunit=concrete%n, status="scratch")
            open(unit=20, iostat=concrete%n)
            open(unit=20, iomsg=concrete%text)
            close(20, iostat=concrete%n)
            close(20, iomsg=concrete%text)
            rewind(20, iostat=concrete%n)
            rewind(20, iomsg=concrete%text)
            backspace(20, iostat=concrete%n)
            backspace(20, iomsg=concrete%text)
            endfile(20, iostat=concrete%n)
            endfile(20, iomsg=concrete%text)
            flush(20, iostat=concrete%n)
            flush(20, iomsg=concrete%text)
            read(20, "(i1)", advance="no", size=concrete%n) item
            read(20, *, iostat=concrete%n) item
            write(20, *, iostat=concrete%n) item
            write(concrete%text(1:10), "(i1)") item
            associate (index => concrete%n)
                write(20, *) (item, index=1,2)
            end associate
        end select
    end subroutine
end module

! R3G-03: a nested BLOCK cannot hide unsupported erased local T storage.
module traits_generic_runtime_block_storage
    use traits_generic_nominal_left, only: IValue, IAlgorithm
    implicit none
    type :: Algorithm
    end type
    implements IAlgorithm :: Algorithm
        procedure, nopass :: apply
    end implements
contains
    function apply{IValue :: T}(object) result(r)
        type(T), intent(in) :: object
        integer :: r
        block
            integer :: local
            local = 1
            block
                type(T) :: scratch
                r = local
            end block
        end block
    end function
end module
