! With sequence association enabled, an array element actual argument must
! still have the type and kind of the explicit-shape dummy array (#4602)
module legacy_array_sections_type_mismatch_01_mod
    implicit none
    real(8), dimension(:), allocatable :: alloc
end module legacy_array_sections_type_mismatch_01_mod

program legacy_array_sections_type_mismatch_01
    use legacy_array_sections_type_mismatch_01_mod, only: alloc
    implicit none
    allocate(alloc(2))
    call sub(alloc(1))
contains
    subroutine sub(alloc)
        real, dimension(4), intent(out) :: alloc
        alloc = 0
    end subroutine sub
end program legacy_array_sections_type_mismatch_01
