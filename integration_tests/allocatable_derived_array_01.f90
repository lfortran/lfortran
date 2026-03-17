module hashmap_wrappers_01
    implicit none
    type :: key_type
        integer(1), allocatable :: value(:)
    end type key_type
end module hashmap_wrappers_01

module hashmap_chaining_01
    use hashmap_wrappers_01
    implicit none
    type(key_type) :: prev_keys_inverse(1024)
    integer(4), allocatable :: duplicate_key_entries(:)
end module hashmap_chaining_01

module test_chaining_maps_01
    use hashmap_chaining_01
    implicit none
contains
    subroutine allocater()
        integer :: i
        do i = 1, 1024
            allocate(prev_keys_inverse(i)%value(16))
        end do
    end subroutine

    subroutine deallocater()
        integer :: i
        do i = 1, 1024
            deallocate(prev_keys_inverse(i)%value)
        end do
    end subroutine
end module

program allocatable_derived_array_01
    use test_chaining_maps_01
    implicit none

    call allocater()
    if (.not. allocated(prev_keys_inverse(1)%value)) error stop
    if (size(prev_keys_inverse(1)%value) /= 16) error stop
    if (.not. allocated(prev_keys_inverse(1024)%value)) error stop

    call deallocater()
    if (allocated(prev_keys_inverse(1)%value)) error stop
    if (allocated(prev_keys_inverse(1024)%value)) error stop
end program
