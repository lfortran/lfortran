program namelist_15
    implicit none

    ! Variables of different intrinsic types
    integer           :: act_i
    real              :: act_r
    character(len=10) :: act_c
    logical           :: act_l

    integer :: lun

    namelist /act_cli/ act_i
    namelist /act_cli/ act_r, act_c
    namelist /act_cli/ act_l

    act_i = 42
    act_r = 3.5
    act_c = 'hello'
    act_l = .true.

    open(newunit=lun, file='test.nml', status='replace', action='write')
    write(lun, nml=act_cli)
    close(lun)

    act_i = -1
    act_r = -1.0
    act_c = ''
    act_l = .false.

    open(newunit=lun, file='test.nml', status='old', action='read')
    read(lun, nml=act_cli)
    close(lun, status='delete')

    if (act_i /= 42)              error stop 'act_i mismatch'
    if (abs(act_r - 3.5) > 1e-6)  error stop 'act_r mismatch'
    if (act_c /= 'hello')         error stop 'act_c mismatch'
    if (.not. act_l)              error stop 'act_l mismatch'

    print *, 'PASSED: file-based NAMELIST with mixed intrinsic types'

end program namelist_15
