! Generic derived types whose components are private. The initializer and the
! method of `Box` give them values with structure constructors of the type
! itself: these are the generic definition's own procedures, which keep the
! access of the module that defines the type when a client instantiates them.
! `Stack` has only private components that are default initialized or
! allocatable, and `Tagged` a public component next to a private one, so a
! client may construct them by giving the public components only.
module traits_generic_type_07_lib
    implicit none
    private
    public :: Box, Stack, Tagged

    type, sealed :: Box{T}
        private
        type(T) :: value
        integer :: count = 0
    contains
        initial :: make_box
        procedure, pass :: bumped
        procedure, pass :: value_of
        procedure, pass :: count_of
    end type Box

    type, sealed :: Stack{T}
        private
        type(T), allocatable :: items(:)
        integer :: n = 0
    contains
        procedure, pass :: size_of
    end type Stack

    type, sealed :: Tagged{T}
        type(T) :: item
        integer, private :: tag = 7
    contains
        procedure, pass :: tag_of
    end type Tagged

contains

    function make_box(v) result(r)
        type(T), intent(in) :: v
        type(Box{T}) :: r
        r = Box{T}(value=v, count=1)
    end function make_box

    function bumped(self) result(r)
        type(Box{T}), intent(in) :: self
        type(Box{T}) :: r
        r = Box{T}(self%value, self%count + 1)
    end function bumped

    function value_of(self) result(v)
        type(Box{T}), intent(in) :: self
        type(T) :: v
        v = self%value
    end function value_of

    integer function count_of(self)
        type(Box{T}), intent(in) :: self
        count_of = self%count
    end function count_of

    integer function size_of(self)
        type(Stack{T}), intent(in) :: self
        size_of = self%n
        if (allocated(self%items)) size_of = -1
    end function size_of

    integer function tag_of(self)
        type(Tagged{T}), intent(in) :: self
        tag_of = self%tag
    end function tag_of

end module traits_generic_type_07_lib
