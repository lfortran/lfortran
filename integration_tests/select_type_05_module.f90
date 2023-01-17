module a_foo
    implicit none

    public :: child_value


    type, abstract :: toml_value
     character(len=:), allocatable :: key
    end type toml_value

     type, extends(toml_value) :: child_value
       logical :: ok
    end type child_value

    interface new
       module procedure :: check_proc
    end interface new

     contains

     subroutine check_proc()
       class(toml_value), pointer :: ptr
       type(child_value), pointer :: array

       if (associated(ptr)) then
          select type(ptr)
          type is(child_value)
             if (ptr%ok) then
                array => ptr
             else
                call not_ok()
                return
             end if
          class default
             call default_class()
             return
          end select
       end if
    end subroutine check_proc

    subroutine not_ok()
       print *, "not ok"
    end subroutine

    subroutine default_class()
       print *, "default class"
    end subroutine

 end module a_foo
