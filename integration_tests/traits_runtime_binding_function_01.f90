! A view argument of a type-bound function: an allocatable trait owner, a
! concrete variable and a forwarded view are borrowed or packed for the
! `class(ITrait)` dummy, as in a subroutine call through a binding, also when
! the function returns an allocatable trait object (the reported
! `res = obj%my_method(tobj)`).
module traits_runtime_binding_function_01_m
    implicit none
    private
    public :: ITrait, IResultTrait, Item, Answer, MyType, forward

    abstract interface :: ITrait
        integer function value(self)
            type(itself), intent(in), pass :: self
        end function value
    end interface

    abstract interface :: IResultTrait
        integer function code(self)
            type(itself), intent(in), pass :: self
        end function code
    end interface

    type, implements(ITrait) :: Item
        integer :: v = 0
    contains
        procedure :: value => item_value
    end type Item

    type, implements(IResultTrait) :: Answer
        integer :: c = 0
    contains
        procedure :: code => answer_code
    end type Answer

    type :: MyType
        integer :: calls = 0
    contains
        procedure :: my_method
        procedure :: plain
    end type MyType

contains

    integer function item_value(self)
        class(Item), intent(in) :: self
        item_value = self%v
    end function item_value

    integer function answer_code(self)
        class(Answer), intent(in) :: self
        answer_code = self%c
    end function answer_code

    function my_method(self, tobj) result(res)
        class(MyType), intent(inout) :: self
        class(ITrait), intent(in) :: tobj
        class(IResultTrait), allocatable :: res
        self%calls = self%calls + 1
        res = Answer(10 * tobj%value())
    end function my_method

    integer function plain(self, tobj)
        class(MyType), intent(in) :: self
        class(ITrait), intent(in) :: tobj
        plain = tobj%value() + self%calls
    end function plain

    ! Forwards its view to the binding.
    integer function forward(obj, view)
        type(MyType), intent(in) :: obj
        class(ITrait), intent(in) :: view
        forward = obj%plain(view)
    end function forward
end module traits_runtime_binding_function_01_m

program traits_runtime_binding_function_01
    use traits_runtime_binding_function_01_m
    implicit none
    type(MyType), allocatable :: obj
    class(ITrait), allocatable :: tobj
    class(IResultTrait), allocatable :: res
    type(Item) :: concrete

    obj = MyType()
    tobj = Item(4)
    res = obj%my_method(tobj)
    if (res%code() /= 40) error stop 1
    concrete = Item(5)
    res = obj%my_method(concrete)
    if (res%code() /= 50) error stop 2
    if (obj%calls /= 2) error stop 3
    if (obj%plain(tobj) /= 6) error stop 4
    if (obj%plain(concrete) /= 7) error stop 5
    if (forward(obj, tobj) /= 6) error stop 6
    print *, "ok"
end program traits_runtime_binding_function_01
