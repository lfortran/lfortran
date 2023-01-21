module d_foo
    use a_foo, only: check_proc
    implicit none
    contains
        subroutine check_here()
            call check_proc()
        end subroutine
end module
