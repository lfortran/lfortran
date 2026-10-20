program intrinsics_496
    ! transpose and spread of deferred-length character arrays (#14775)
    implicit none
    character(:), allocatable :: s(:), t(:,:), r2(:,:)

    allocate(character(2) :: s(4))
    s = ["bb", "cc", "aa", "dd"]
    allocate(character(2) :: t(2, 3))
    t = reshape(["ab", "cd", "ef", "gh", "ij", "kl"], [2, 3])

    call check2(transpose(t), reshape(["ab", "ef", "ij", "cd", "gh", "kl"], [3, 2]))
    call check2(transpose(t(1:2, 1:2)), reshape(["ab", "ef", "cd", "gh"], [2, 2]))
    call check2(spread(s, 1, 2), reshape(["bb", "bb", "cc", "cc", "aa", "aa", "dd", "dd"], [2, 4]))
    call check2(spread(s(1:2), 2, 2), reshape(["bb", "cc", "bb", "cc"], [2, 2]))

    allocate(character(2) :: r2(3, 2))
    r2 = transpose(t)
    call check2(r2, reshape(["ab", "ef", "ij", "cd", "gh", "kl"], [3, 2]))
    deallocate(r2)
    allocate(character(2) :: r2(2, 2))
    r2 = spread(s(1:2), 1, 2)
    call check2(r2, reshape(["bb", "bb", "cc", "cc"], [2, 2]))

contains

    subroutine check2(res, expected)
        character(*), intent(in) :: res(:,:), expected(:,:)
        if (len(res) /= 2) error stop 1
        if (any(shape(res) /= shape(expected))) error stop 2
        if (any(res /= expected)) error stop 3
    end subroutine

end program
