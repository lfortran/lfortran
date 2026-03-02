! Test: associate with array section of a derived-type member.
! Regression: EXPR2VAR assumed ArraySection base is Var_t, but
! workspace%tmp(:,1) yields a StructInstanceMember_t base.
program associate_38
    implicit none

    type :: workspace_type
        real, allocatable :: tmp(:,:)
    end type

    type(workspace_type) :: workspace
    allocate(workspace%tmp(3,2))
    workspace%tmp = 0.0

    associate(R => workspace%tmp(:,1))
        R = 1.0
    end associate

    if (any(abs(workspace%tmp(:,1) - 1.0) > 1e-6)) error stop
    if (any(abs(workspace%tmp(:,2) - 0.0) > 1e-6)) error stop
    print *, "PASS"
end program
