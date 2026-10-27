module mangle_underscore_02_sys
    implicit none
    integer :: group = 7
end module

integer function my_trans(group_)
    use mangle_underscore_02_sys, only: group
    implicit none
    integer, intent(in) :: group_
    my_trans = group + group_
end function

program mangle_underscore_02
    use mangle_underscore_02_sys, only: group
    implicit none
    interface
        integer function my_trans(group_)
            integer, intent(in) :: group_
        end function
    end interface
    integer :: group_ = 2
    print '(I0)', group + group_
    if (group + group_ /= 9) error stop
    print '(I0)', my_trans(5)
    if (my_trans(5) /= 12) error stop
    print '(I0)', internal(3)
    if (internal(3) /= 7) error stop
    block
        integer :: k, k_
        k = 10
        k_ = 20
        print '(I0)', k + k_
        if (k + k_ /= 30) error stop
    end block
contains
    integer function internal(m)
        integer, intent(in) :: m
        integer :: m_
        m_ = 4
        internal = m + m_
    end function
end program
