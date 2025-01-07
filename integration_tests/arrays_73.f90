program arrays_73
    implicit none
    integer :: A(4) = [1,2,3,4]
    integer :: A1(3,3) = reshape([1,2,3,4,5,6,7,8,9], [3,3])
    integer :: A3(3,3) = reshape([1,2,3,4,5,6,7,8,9], [3,3])
    integer :: A2(2,2,2) = reshape([1,2,3,4,5,6,7,8], [2,2,2])
    integer :: tmp(2,2) = reshape([5,5,5,5], [2,2])
    call temp(A, A1, A2, A3, tmp)
contains
    subroutine temp(A, A1, A2, A3, tmp)
        integer, intent(inout) :: A(:)
        integer, intent(inout) :: A1(:,:)
        integer, intent(inout) :: A3(:,:)
        integer, intent(inout) :: A2(:,:,:)
        integer, intent(inout) :: tmp(:,:)

        A1(:,[2,1]) = A3(:,[2,3])
        if (A1(1,1) /= 7 .or. A1(2,1) /= 8 .or. A1(3,1) /= 9 .or. &
            & A1(1,2) /= 4 .or. A1(2,2) /= 5 .or. A1(3,2) /= 6 .or. &
            & A1(1,3) /= 7 .or. A1(2,3) /= 8 .or. A1(3,3) /= 9) error stop

        A1(:,1:2) = A3([2,1,3],[2,1])
        if (A1(1,1) /= 5 .or. A1(2,1) /= 4 .or. A1(3,1) /= 6 .or. &
            & A1(1,2) /= 2 .or. A1(2,2) /= 1 .or. A1(3,2) /= 3 .or. &
            & A1(1,3) /= 7 .or. A1(2,3) /= 8 .or. A1(3,3) /= 9) error stop

        A1(1:2,2:3) = A3(2:3,1:2)
        if (A1(1,1) /= 5 .or. A1(2,1) /= 4 .or. A1(3,1) /= 6 .or. &
            & A1(1,2) /= 2 .or. A1(2,2) /= 3 .or. A1(3,2) /= 3 .or. &
            & A1(1,3) /= 5 .or. A1(2,3) /= 6 .or. A1(3,3) /= 9) error stop

        !! TODO: Handle cases by creating Temporary for RHS where LHS and RHS has common variable

    end subroutine
end program