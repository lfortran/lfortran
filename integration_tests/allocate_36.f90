module allocate_36_mod
    implicit none

    type :: string_t
        character(len=:), allocatable :: s
    end type string_t

    type :: preprocess_config_t
        type(string_t), allocatable :: macros(:)
    end type preprocess_config_t

contains

    subroutine new(self, macros)
        type(preprocess_config_t), intent(inout) :: self
        type(string_t), intent(in) :: macros(:)

        ! Defensive checks
        if (allocated(self%macros)) then
            print *, "Deallocating existing macros"
            deallocate(self%macros)
        end if

        if (allocated(self%macros)) then
            error stop "ERROR: self%macros still allocated after deallocate"
        end if

        allocate(self%macros, source=macros)

        if (.not. allocated(self%macros)) then
            error stop "ERROR: allocation of self%macros failed"
        end if

        if (size(self%macros) /= size(macros)) then
            error stop "ERROR: size mismatch after allocate(source=)"
        end if

    end subroutine new

end module allocate_36_mod


program allocate_36
    use allocate_36_mod
    implicit none

    type(preprocess_config_t) :: cpp_config

    ! First initialization
    call new(cpp_config, [ &
        string_t('SOME_FEATURE'), &
        string_t('SIMPLE_MACRO') ])

    if (.not. allocated(cpp_config%macros)) then
        error stop "ERROR: macros not allocated after first call"
    end if

    if (size(cpp_config%macros) /= 2) then
        error stop "ERROR: wrong size after first call"
    end if

    ! Second re-initialization
    call new(cpp_config, [ string_t('FIFTH_FEATURE') ])

    if (.not. allocated(cpp_config%macros)) then
        error stop "ERROR: macros not allocated after second call"
    end if

    if (size(cpp_config%macros) /= 1) then
        error stop "ERROR: wrong size after second call"
    end if

    print *, "OK: MRE completed successfully"

end program allocate_36
