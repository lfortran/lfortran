module traits_composition_01_m
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

    type :: JointBox
        integer :: data
    end type JointBox

    type :: SplitBox
        integer :: data
    end type SplitBox

    implements (IValue + IShift) :: JointBox
        procedure, pass :: value => joint_value
        procedure, pass :: shift => joint_shift
    end implements JointBox

    implements IShift :: SplitBox
        procedure, pass :: shift => split_shift
    end implements SplitBox

    implements IValue :: SplitBox
        procedure, pass :: value => split_value
    end implements SplitBox

contains

    function joint_value(self) result(res)
        class(JointBox), intent(in) :: self
        integer :: res
        res = self%data
    end function joint_value

    function joint_shift(self, delta) result(res)
        class(JointBox), intent(in) :: self
        integer, intent(in) :: delta
        integer :: res
        res = self%data + delta
    end function joint_shift

    function split_value(self) result(res)
        class(SplitBox), intent(in) :: self
        integer :: res
        res = 2 * self%data
    end function split_value

    function split_shift(self, delta) result(res)
        class(SplitBox), intent(in) :: self
        integer, intent(in) :: delta
        integer :: res
        res = 2 * self%data + delta
    end function split_shift

    function read_value{IValue :: T}(object) result(res)
        type(T), intent(in) :: object
        integer :: res
        res = object%value()
    end function read_value

    function combined_value{IValue + IShift :: T}(object) result(res)
        type(T), intent(in) :: object
        integer :: res
        res = read_value{T}(object) + object%shift(4)
    end function combined_value

    function reversed_value{IShift + IValue :: T}(object) result(res)
        type(T), intent(in) :: object
        integer :: res
        res = combined_value(object)
    end function reversed_value
end module traits_composition_01_m

program traits_composition_01
    use traits_composition_01_m
    implicit none
    type(JointBox) :: joint
    type(SplitBox) :: split

    joint = JointBox(6)
    split = SplitBox(5)
    if (combined_value(joint) /= 16) error stop 1
    if (combined_value(split) /= 24) error stop 2
    if (combined_value{JointBox}(joint) /= 16) error stop 3
    if (combined_value{SplitBox}(split) /= 24) error stop 4
    if (reversed_value(joint) /= 16) error stop 5
    if (reversed_value(split) /= 24) error stop 6
end program traits_composition_01
