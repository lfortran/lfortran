program test_namelist_internal_proc
    implicit none

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

    call write_namelist()

    act_i = 0
    act_r = 0.0
    act_c = ''
    act_l = .false.

    open(unit=10, file='namelist_22.dat', status='old', form='formatted')
    read(10, nml=act_cli)
    close(10)

    if (act_i /= 42) error stop
    if (abs(act_r - 3.5) > 1.0e-5) error stop
    if (act_c /= 'hello') error stop
    if (.not. act_l) error stop

contains

    subroutine write_namelist()
        open(file='namelist_22.dat', newunit=lun, status='replace', form='formatted')
        write(lun, nml=act_cli)
        close(unit=lun)
    end subroutine write_namelist
end program test_namelist_internal_proc
