module template_instantiate_rename_07_other
    implicit none
    type :: box
        integer :: w
    end type
    type :: get
        integer :: w
    end type
    type(box) :: gob
end module

! Types from another module share the names of the template's type `box` and
! function `get`; their components are not those of the instances.
module template_instantiate_rename_07_m
    use template_instantiate_rename_07_other, only: gob, oget => get
    implicit none
    template t {T}
        deferred type :: T
        type :: box
            type(T) :: v
        end type
    contains
        integer function get(b, x)
            type(box), intent(inout) :: b
            type(T), intent(in) :: x
            type(oget) :: ob
            b%v = x
            gob%w = 5
            ob%w = 7
            get = gob%w + ob%w
        end function
    end template
end module

program template_instantiate_rename_07
    use template_instantiate_rename_07_m
    implicit none
    instantiate t {real}, only: rbox => box, rget => get
    type(rbox) :: b
    if (rget(b, 2.5) /= 12) error stop
    if (abs(b%v - 2.5) > 1e-6) error stop
    print *, "ok"
end program
