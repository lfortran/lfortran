! submodule_63_q, and through it submodule_63_r, are used only by the nested
! submodule; submodule_63_k only by its parent submodule, from which the
! nested one reaches it by host association.
module submodule_63_r
    implicit none
    type :: tc
        character(len=4) :: nm = "abcd"
        integer :: k = 2
    end type
    type(tc) :: r_objs(4) = tc("wxyz", 1)
    integer :: r_calls = 0
contains
    integer function r_sum()
        integer :: i
        r_calls = r_calls + 1
        r_sum = 0
        do i = 1, 4
            if (r_objs(i)%nm == "wxyz") r_sum = r_sum + i*r_objs(i)%k
        end do
    end function
end module

module submodule_63_q
    use submodule_63_r
    implicit none
contains
    integer function q_value()
        q_value = 2*r_sum()
        q_value = q_value + r_calls
    end function
end module

module submodule_63_k
    implicit none
    integer :: k_val = 7
contains
    integer function k_twice()
        k_twice = 2*k_val
    end function
end module
