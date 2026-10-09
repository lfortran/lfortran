module traits_composition_01_oracle_m
    implicit none

    type :: JointBox
        integer :: data
    contains
        procedure :: value => joint_value
        procedure :: shift => joint_shift
    end type JointBox

    type :: SplitBox
        integer :: data
    contains
        procedure :: value => split_value
        procedure :: shift => split_shift
    end type SplitBox

    interface read_value
        module procedure read_joint
        module procedure read_split
    end interface read_value

    interface combined_value
        module procedure combined_joint
        module procedure combined_split
    end interface combined_value

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

    function read_joint(object) result(res)
        type(JointBox), intent(in) :: object
        integer :: res
        res = object%value()
    end function read_joint

    function read_split(object) result(res)
        type(SplitBox), intent(in) :: object
        integer :: res
        res = object%value()
    end function read_split

    function combined_joint(object) result(res)
        type(JointBox), intent(in) :: object
        integer :: res
        res = read_value(object) + object%shift(4)
    end function combined_joint

    function combined_split(object) result(res)
        type(SplitBox), intent(in) :: object
        integer :: res
        res = read_value(object) + object%shift(4)
    end function combined_split
end module traits_composition_01_oracle_m

program traits_composition_01_oracle
    use traits_composition_01_oracle_m
    implicit none
    type(JointBox) :: joint
    type(SplitBox) :: split

    joint = JointBox(6)
    split = SplitBox(5)
    if (combined_value(joint) /= 16) error stop 1
    if (combined_value(split) /= 24) error stop 2
    if (read_value(joint) /= 6) error stop 3
    if (read_value(split) /= 10) error stop 4
end program traits_composition_01_oracle
