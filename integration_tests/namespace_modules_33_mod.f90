! Modules compiled separately from namespace_modules_33.f90, which reads them
! from their .mod files. Module namespace_modules_33_mod_b declares public
! module entities, for a user module and for an intrinsic module, and extends
! a type accessed through one of them.
!
! Both modules are in one file: the Fortran dependency scanner of CMake does
! not recognize `use, namespace`, so it would not compile a separate file
! for module namespace_modules_33_mod_a first.
module namespace_modules_33_mod_a
    implicit none
    integer :: x = 1
    type :: t
        integer :: x = 0
    contains
        procedure :: twice
    end type
contains
    integer function f(i)
        integer, intent(in) :: i
        f = 10*i
    end function

    integer function twice(self)
        class(t), intent(in) :: self
        twice = 2*self%x
    end function
end module

module namespace_modules_33_mod_b
    use, namespace :: a => namespace_modules_33_mod_a
    use, namespace :: env => iso_fortran_env
    implicit none
    ! Not the parent component `t` of type u, which is not a name here
    integer :: t = 3
    type, extends(a%t) :: u
        integer :: y = 0
    end type
    real(env%real64), parameter :: half = real(0.5, env%real64)
contains
    subroutine set_parent(v, i)
        type(u), intent(inout) :: v
        integer, intent(in) :: i
        v%t%x = i
    end subroutine
end module
