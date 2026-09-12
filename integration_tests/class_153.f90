! Type bound procedure bindings are collected per program unit. Two program
! units that declare a derived type with the same name and the same binding
! must each get their own binding, and the bookkeeping of one unit must not
! leak into the next one.
subroutine class_153_sub(res)
    implicit none
    integer, intent(out) :: res
    interface
        integer function class_153_answer()
        end function class_153_answer
    end interface
    type :: dup_t
        integer :: k
    contains
        procedure, nopass :: class_153_answer
    end type dup_t
    type(dup_t) :: obj
    obj%k = 1
    res = obj%k + obj%class_153_answer()
end subroutine class_153_sub

integer function class_153_answer()
    class_153_answer = 42
end function class_153_answer

program class_153
    implicit none
    interface
        subroutine class_153_sub(res)
            integer, intent(out) :: res
        end subroutine class_153_sub
        integer function class_153_answer()
        end function class_153_answer
    end interface
    type :: dup_t
        integer :: j
    contains
        procedure, nopass :: class_153_answer
    end type dup_t
    type(dup_t) :: l
    integer :: r
    l%j = 7
    if (l%j /= 7) error stop
    if (l%class_153_answer() /= 42) error stop
    call class_153_sub(r)
    if (r /= 43) error stop
end program class_153
