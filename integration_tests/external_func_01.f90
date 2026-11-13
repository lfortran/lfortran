subroutine check_external_before_type(ok)
    implicit none
    logical, intent(out) :: ok
    external :: lsame
    logical :: lsame
    ok = lsame('D', 'D') .and. .not. lsame('D', 'N')
end subroutine

program external_func_01
    implicit none
    character(len=1) :: kind
    logical :: lsame, ok
    external :: lsame, check_external_before_type
    kind = 'D'
    if (.not. lsame(kind, 'D')) error stop 1
    call check_external_before_type(ok)
    if (.not. ok) error stop 2
    print *, 'ok'
end program
