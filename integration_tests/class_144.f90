module class_144_module
    implicit none
    type :: topol_t
        character(len=256), allocatable :: geometries(:)
    end type topol_t
contains
    subroutine init(this)
        class(topol_t), intent(inout) :: this
        integer :: j
        j = 1
        allocate(this%geometries(1))
        this%geometries(j) = repeat(" ", len(this%geometries(j)))
    end subroutine init
end module class_144_module

program class_144
    use class_144_module
    implicit none
    type(topol_t) :: topo
    call init(topo)
    if (len(topo%geometries(1)) /= 256) error stop "incorrect length"
    if (topo%geometries(1) /= repeat(" ", 256)) error stop "incorrect content"
    print *, "all tests passed"
end program class_144