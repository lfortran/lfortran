module type_bound_private_01_m
implicit none
type :: base_t
    integer(4) :: i = 3
contains
    procedure :: add_one
    procedure, private :: bump
    procedure :: driver
    procedure, private :: getf
end type base_t
type, extends(base_t) :: child_t
contains
    procedure :: add_one
    procedure :: run
    procedure, private :: twice
end type child_t
integer(4), save :: counter = 0
interface
    subroutine sub_driver(self)
        import base_t
        type(base_t), intent(in) :: self
    end subroutine sub_driver
end interface

contains

subroutine add_one()
    call bump_twice()
end subroutine add_one

subroutine bump()
    counter = counter + 1
end subroutine bump

subroutine bump_twice()
    type(base_t) :: b
    call b%bump()
    call b%bump()
end subroutine bump_twice

subroutine driver(self)
    class(base_t), intent(in) :: self
    call self%bump()
    if (self%getf() /= 3) then
        error stop
    end if
end subroutine driver

integer(4) function getf(self)
    class(base_t), intent(in) :: self
    getf = self%i
end function getf

subroutine run(self)
    class(child_t), intent(in) :: self
    call self%bump()
    if (self%twice() /= 6) then
        error stop
    end if
end subroutine run

integer(4) function twice(self)
    class(child_t), intent(in) :: self
    twice = 2*self%getf()
end function twice

end module type_bound_private_01_m

module type_bound_private_01_s
use type_bound_private_01_m, only: add_one
use type_bound_private_01_m, only: base_t
use type_bound_private_01_m, only: bump
use type_bound_private_01_m, only: bump_twice
use type_bound_private_01_m, only: child_t
use type_bound_private_01_m, only: counter
use type_bound_private_01_m, only: driver
use type_bound_private_01_m, only: getf
use type_bound_private_01_m, only: run
use type_bound_private_01_m, only: twice
implicit none

contains

subroutine sub_driver(self)
    type(base_t), intent(in) :: self
    call self%bump()
    if (self%getf() /= 3) then
        error stop
    end if
end subroutine sub_driver

end module type_bound_private_01_s

program type_bound_private_01
use type_bound_private_01_m, only: base_t
use type_bound_private_01_m, only: child_t
use type_bound_private_01_m, only: counter
use type_bound_private_01_m, only: sub_driver
implicit none
type(base_t) :: b
type(child_t) :: c
call b%driver()
if (counter /= 1) then
    error stop
end if
call c%driver()
if (counter /= 2) then
    error stop
end if
call c%run()
if (counter /= 3) then
    error stop
end if
call b%add_one()
if (counter /= 5) then
    error stop
end if
call sub_driver(c)
if (counter /= 6) then
    error stop
end if
print *, "ok"
end program type_bound_private_01
