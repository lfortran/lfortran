module template_unused_01_m
    implicit none
    template unused_array_intrinsics {t}
        deferred type :: t
    contains
        subroutine apply(x, mask)
            type(t), intent(inout) :: x(2)
            logical, intent(in) :: mask(2)
            x = merge(x, x, mask)
            x = cshift(x, 1)
            x = pack(x, .true.)
        end subroutine
    end template
end module

program template_unused_01
    use template_unused_01_m
    implicit none
    ! Neither the module template nor this program-local template is instantiated.
    template unused_merge {t}
        deferred type :: t
    contains
        subroutine f(x)
            type(t) :: x(1)
            x = merge(x, x, .true.)
        end subroutine
    end template
end program
