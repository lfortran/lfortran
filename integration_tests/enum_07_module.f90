module enum_07_m_constants
    implicit none
    enum, bind(C)
        enumerator :: sparse_full = 0
    end enum
end module

module enum_07_parent_mod
    use enum_07_m_constants
    implicit none
    public :: sparse_full
    enum, bind(C)
        enumerator :: wksp_a = 3
    end enum
    interface
        module subroutine do_work(x)
            integer, intent(inout) :: x
        end subroutine
    end interface
end module
