! Private bindings inherited by an extension type defined in another module
! remain accessible in the module that declares them, including its
! submodules, when referenced through an object of the extension type.
module type_bound_private_03_a
    implicit none
    type :: t
        integer :: v = 4
    contains
        procedure, nopass :: p
        generic, private :: g => p
        procedure, private :: q
        generic :: h => q
    end type
    interface
        module subroutine use_child()
        end subroutine
    end interface
contains
    integer function p()
        p = 7
    end function
    integer function q(self)
        class(t), intent(in) :: self
        q = self%v
    end function
end module

module type_bound_private_03_b
    use type_bound_private_03_a
    implicit none
    type, extends(t) :: c
        integer :: w = 3
    end type
end module

submodule (type_bound_private_03_a) type_bound_private_03_s
    use type_bound_private_03_b
    implicit none
contains
    module subroutine use_child()
        type(c) :: y
        y%v = 10
        if (y%g() /= 7) error stop 1
        if (y%p() /= 7) error stop 2
        if (y%q() /= 10) error stop 3
        if (y%h() /= 10) error stop 4
        if (y%w /= 3) error stop 5
    end subroutine
end submodule

program type_bound_private_03
    use type_bound_private_03_a, only: use_child
    use type_bound_private_03_b
    implicit none
    type(c) :: y
    call use_child()
    if (y%h() /= 4) error stop 6
    if (y%p() /= 7) error stop 7
    print *, "ok"
end program
