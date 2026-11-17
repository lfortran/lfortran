module class_intent_out_reset_01_m
    implicit none
    type :: writer
        logical :: opened = .false.
    contains
        procedure :: label
    end type
    type, extends(writer) :: mesh_writer
        logical :: offsets_ready = .false.
        integer :: step = 2
        integer, allocatable :: work(:)
    contains
        procedure :: label => mesh_label
    end type
contains
    integer function label(self)
        class(writer), intent(in) :: self
        label = 1
    end function
    integer function mesh_label(self)
        class(mesh_writer), intent(in) :: self
        mesh_label = 2
    end function
    subroutine close_writer(w)
        class(writer), intent(out) :: w
    end subroutine
    subroutine maybe_close(w)
        class(writer), intent(out), optional :: w
    end subroutine
end module

program class_intent_out_reset_01
    use class_intent_out_reset_01_m
    implicit none
    type(mesh_writer) :: file
    type(writer) :: plain
    class(writer), allocatable :: dyn
    integer :: i
    do i = 1, 2
        file%opened = .true.
        file%offsets_ready = .true.
        file%step = 9
        allocate(file%work(3))
        call close_writer(file)
        if (file%opened) error stop 1
        if (file%offsets_ready) error stop 2
        if (file%step /= 2) error stop 3
        if (allocated(file%work)) error stop 4
    end do
    if (file%label() /= 2) error stop 5
    file%offsets_ready = .true.
    call maybe_close(file)
    if (file%offsets_ready) error stop 6
    plain%opened = .true.
    call close_writer(plain)
    if (plain%opened) error stop 7
    allocate(mesh_writer :: dyn)
    select type (dyn)
    type is (mesh_writer)
        dyn%opened = .true.
        dyn%offsets_ready = .true.
        allocate(dyn%work(2))
    end select
    call close_writer(dyn)
    if (dyn%label() /= 2) error stop 8
    select type (dyn)
    type is (mesh_writer)
        if (dyn%opened .or. dyn%offsets_ready) error stop 9
        if (dyn%step /= 2 .or. allocated(dyn%work)) error stop 10
    class default
        error stop 11
    end select
    print *, "ok"
end program
