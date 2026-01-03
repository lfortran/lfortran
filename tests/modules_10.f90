module my_other_module
    integer :: my_global = 0
contains
    subroutine my_other_proc
        print *, my_global
    end subroutine
end module

module modules_10
    use my_other_module, only: my_global
contains
    subroutine my_proc
        use my_other_module, only: my_other_proc

        call my_other_proc
    end subroutine
end module
