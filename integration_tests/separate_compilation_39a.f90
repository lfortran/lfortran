module separate_compilation_39a
    implicit none

    type recursive_t(k)
        integer, kind :: k = kind(1.0)
        type(recursive_t(k)), allocatable :: next
    end type recursive_t

    interface
        module subroutine noop()
        end subroutine noop
    end interface
end module separate_compilation_39a
