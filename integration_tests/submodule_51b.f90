module submodule_51_mod
implicit none
interface
    module subroutine show(mask)
        logical, intent(in) :: mask(:,:)
    end subroutine
end interface
end module

submodule (submodule_51_mod) submodule_51_impl
implicit none
contains
module subroutine show(mask)
    logical, intent(in) :: mask(:,:)
    if (size(mask, 1) /= 2) error stop
    if (size(mask, 2) /= 3) error stop
    if (.not. mask(1,1)) error stop
    if (.not. mask(2,1)) error stop
    if (mask(1,2)) error stop
    if (mask(2,2)) error stop
    if (mask(1,3)) error stop
    if (mask(2,3)) error stop
end subroutine
end submodule
