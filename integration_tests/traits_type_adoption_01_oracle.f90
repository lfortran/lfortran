! Ordinary Fortran counterpart of the trait views; CLASS replaces SEALED's TYPE receiver.
module traits_type_adoption_01_oracle_m
    implicit none
    private
    public :: Parent, Child, Leaf, Closed, inspect, read_extra, final_count
    integer :: final_count = 0
    type :: Parent
        real(8) :: padding(2) = [1.0_8, 2.0_8]
        integer :: seed = 7
    contains
        procedure :: value => parent_value
    end type
    type, extends(Parent) :: Child
        real(8) :: other_padding = 3.0_8
        integer :: bonus = 11
    contains
        procedure, pass(self) :: extra => child_extra
        final :: finalize_child
    end type
    type, extends(Child) :: Leaf
        integer :: leaf_value = 13
    contains
        procedure :: value => leaf_value_impl
    end type
    type, extends(Child) :: Closed
        integer :: closed_value = 17
    contains
        procedure :: value => closed_value_impl
    end type
contains
    integer function parent_value(self) result(n)
        class(Parent), intent(in) :: self
        n = self%seed
        select type(self)
        type is (Child)
            n = n + self%bonus
        end select
    end function
    integer function child_extra(n, self) result(v)
        integer, intent(in) :: n
        class(Child), intent(in) :: self
        v = self%seed + self%bonus + n
    end function
    integer function leaf_value_impl(self) result(n)
        class(Leaf), intent(in) :: self
        n = self%seed + self%bonus + self%leaf_value
    end function
    integer function closed_value_impl(self) result(n)
        class(Closed), intent(in) :: self
        n = self%seed + self%bonus + self%closed_value
    end function
    subroutine finalize_child(self)
        type(Child), intent(inout) :: self
        final_count = final_count + 1
        self%seed = -999
    end subroutine
    integer function read_extra(self, n) result(v)
        class(Parent), intent(in) :: self
        integer, intent(in) :: n
        select type(self)
        class is (Child)
            v = self%extra(n)
        class default
            error stop
        end select
    end function
    integer function inspect(self) result(n)
        class(Parent), intent(in) :: self
        n = self%value()
        select type (self)
        type is (Child)
            n = n + 100
        type is (Leaf)
            n = n + 200
        type is (Closed)
            n = n + 300
        class default
            error stop
        end select
    end function
end module

program traits_type_adoption_01_oracle
    use traits_type_adoption_01_oracle_m
    implicit none
    type(Parent) :: p
    type(Child), target :: c
    type(Leaf), target :: l
    type(Closed), target :: s
    class(Parent), pointer :: view
    class(Parent), allocatable :: owner
    if (p%value() /= 7 .or. c%value() /= 18) error stop
    if (l%value() /= 31 .or. s%value() /= 35) error stop
    if (inspect(c) /= 118 .or. inspect(l) /= 231 .or. inspect(s) /= 335) error stop
    view => c
    if (view%value() /= 18 .or. read_extra(view, 3) /= 21) error stop
    c%seed = 19
    if (view%value() /= 30 .or. inspect(view) /= 130) error stop
    nullify(view)
    allocate(owner, source=c)
    c%seed = 29
    if (owner%value() /= 30 .or. read_extra(owner, 3) /= 33) error stop
    if (inspect(owner) /= 130 .or. final_count /= 0) error stop
    deallocate(owner)
    if (final_count /= 1 .or. c%seed /= 29) error stop
    allocate(owner, source=l)
    if (owner%value() /= 31 .or. inspect(owner) /= 231) error stop
    deallocate(owner)
    if (final_count /= 2) error stop
    allocate(owner, source=s)
    if (owner%value() /= 35 .or. inspect(owner) /= 335) error stop
    deallocate(owner)
    if (final_count /= 3) error stop
    print *, "type adoption: 18 31 35; parent finalizations:", final_count
end program
