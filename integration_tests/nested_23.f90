module nested_23_mod
    implicit none

    type :: build_target
        character(len=:), allocatable :: name
        character(len=:), allocatable :: names(:)
    end type build_target

contains

    subroutine prune_build_targets(targets)
        type(build_target), allocatable, intent(inout) :: targets(:)
        integer :: i

        ! ---- Basic sanity checks ----
        if (.not. allocated(targets)) error stop "targets not allocated"
        if (size(targets) == 0) error stop "targets is empty"

        do i = 1, size(targets)
            associate(target => targets(i))

                if (.not. allocated(target%name)) then
                    error stop "target name not allocated"
                end if

                if (temp_call(targets)) then
                    call collect_used_modules(target)
                end if

            end associate
        end do

    contains

        subroutine collect_used_modules(target)
            type(build_target), intent(inout) :: target

            ! ---- Validate access to host-associated variable ----
            if (.not. allocated(targets)) then
                error stop "host variable 'targets' unexpectedly unallocated"
            end if

            if (.not. allocated(target%name)) then
                error stop "collect_used_modules: target%name not allocated"
            end if

            ! Dummy logic: attach a single "used module"
            allocate(character(len=len_trim(target%name)) :: target%names(1))
            target%names(1) = target%name

        end subroutine collect_used_modules

    end subroutine prune_build_targets


    logical function temp_call(targets)
        type(build_target), allocatable, intent(in) :: targets(:)

        if (.not. allocated(targets)) error stop "temp_call: targets not allocated"

        ! Simple logic: trigger processing only if >1 target exists
        temp_call = size(targets) > 1
    end function temp_call

end module nested_23_mod


program nested_23
    use nested_23_mod
    implicit none

    type(build_target), allocatable :: targets(:)

    allocate(targets(2))

    targets(1)%name = "core_lib"
    targets(2)%name = "util_lib"

    call prune_build_targets(targets)

    ! ---- Post-condition check ----
    if (.not. allocated(targets(1)%names)) error stop "names not populated"
    if (targets(1)%names(1) /= "core_lib") error stop "unexpected module name"

    print *, "MRE completed successfully"

end program nested_23
