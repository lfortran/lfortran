program allocatable_component_struct_array_02
  implicit none
  integer, parameter :: dp=kind(0.d0), nslots=100, s=200, n=80, m=1000
  type :: var_t
    character(len=n) :: var_name=""
    character(len=s) :: description=""
    integer :: var_type=0
    character(len=m) :: stored_data=""
    real(dp), allocatable :: real_data(:)
    character(len=s), allocatable :: char_data(:)
  end type
  type :: cfg_t
    integer :: num_vars=0
    type(var_t), allocatable :: vars(:)
  end type
  type(cfg_t) :: cfg

  call init(cfg)
  call test_copies1(cfg)

  deallocate(cfg%vars(1)%real_data)
  allocate(cfg%vars(1)%real_data(2))
  cfg%vars(1)%real_data = [1.0_dp, 2.0_dp]
  cfg%vars(2)%char_data(1) = "J."
  cfg%vars(3)%char_data = [character(len=s) :: "H.J.", "Teunissen"]
  cfg%vars(4)%char_data(1) = "another_file"
  call test_copies2(cfg)

contains

  subroutine init(cfg)
    type(cfg_t), intent(out) :: cfg
    allocate(cfg%vars(nslots))
    cfg%num_vars = 4
    cfg%vars(1)%var_name = "author%fav_reals"
    cfg%vars(1)%var_type = 2
    allocate(cfg%vars(1)%real_data(3))
    cfg%vars(1)%real_data = [1.337_dp, 13.37_dp, 133.7_dp]
    cfg%vars(2)%var_name = "author_name%first"
    cfg%vars(2)%var_type = 3
    allocate(cfg%vars(2)%char_data(1))
    cfg%vars(2)%char_data(1) = "jannis"
    cfg%vars(3)%var_name = "author_name%full"
    cfg%vars(3)%var_type = 3
    allocate(cfg%vars(3)%char_data(2))
    cfg%vars(3)%char_data = [character(len=s) :: "Jannis", "Teunissen"]
    cfg%vars(4)%var_name = "filename"
    cfg%vars(4)%var_type = 3
    allocate(cfg%vars(4)%char_data(1))
    cfg%vars(4)%char_data(1) = "this/is/a/filename"
  end subroutine

  subroutine test_copies1(cfg)
    type(cfg_t), intent(in) :: cfg
    type(cfg_t) :: dst

    print *, "---- Copy-1 PRE-COPY ----"
    print *, "first    = '", trim(cfg%vars(2)%char_data(1)), "'"
    print *, "filename = '", trim(cfg%vars(4)%char_data(1)), "'"

    dst = cfg

    print *, "---- Copy-1 POST-COPY ----"
    print *, "first    = '", trim(dst%vars(2)%char_data(1)), "'"
    print *, "filename = '", trim(dst%vars(4)%char_data(1)), "'"

    

    ! one scalar field to confirm basic copy works
    if (trim(dst%vars(1)%var_name) /= "author%fav_reals") then
      print *, "FAIL copy1: vars(1)%var_name: ", trim(dst%vars(1)%var_name), "'"
      error stop
    end if

    ! real_data: the allocatable array under test
    if (.not. allocated(dst%vars(1)%real_data)) then
      print *, "FAIL copy1: vars(1)%real_data not allocated"
      error stop
    end if
    if (size(dst%vars(1)%real_data) /= 3) then
      print *, "FAIL copy1: vars(1)%real_data size: ", size(dst%vars(1)%real_data)
      error stop
    end if
    if (dst%vars(1)%real_data(1) /= 1.337_dp) then
      print *, "FAIL copy1: vars(1)%real_data(1): ", dst%vars(1)%real_data(1)
      error stop
    end if
    if (dst%vars(1)%real_data(2) /= 13.37_dp) then
      print *, "FAIL copy1: vars(1)%real_data(2): ", dst%vars(1)%real_data(2)
      error stop
    end if
    if (dst%vars(1)%real_data(3) /= 133.7_dp) then
      print *, "FAIL copy1: vars(1)%real_data(3): ", dst%vars(1)%real_data(3)
      error stop
    end if

    ! char_data: the allocatable array under test
    if (.not. allocated(dst%vars(2)%char_data)) then
      print *, "FAIL copy1: vars(2)%char_data not allocated"
      error stop
    end if
    if (trim(dst%vars(2)%char_data(1)) /= "jannis") then
      print *, "FAIL copy1: vars(2)%char_data(1): ", trim(dst%vars(2)%char_data(1))
      error stop
    end if
    if (trim(dst%vars(3)%char_data(1)) /= "Jannis") then
      print *, "FAIL copy1: vars(3)%char_data(1): ", trim(dst%vars(3)%char_data(1))
      error stop
    end if
    if (trim(dst%vars(3)%char_data(2)) /= "Teunissen") then
      print *, "FAIL copy1: vars(3)%char_data(2): ", trim(dst%vars(3)%char_data(2))
      error stop
    end if
    if (trim(dst%vars(4)%char_data(1)) /= "this/is/a/filename") then
      print *, "FAIL copy1: vars(4)%char_data(1): '", trim(dst%vars(4)%char_data(1))
      error stop
    end if

    print *, "All Copy 1 Tests Passed"
  end subroutine

  subroutine test_copies2(cfg)
    type(cfg_t), intent(in) :: cfg
    type(cfg_t) :: dst

    print *, "---- Copy-1 PRE-COPY ----"
    print *, "first    = '", trim(cfg%vars(2)%char_data(1)), "'"
    print *, "filename = '", trim(cfg%vars(4)%char_data(1)), "'"

    dst = cfg

    print *, "---- Copy-2 POST-COPY ----"
    print *, "first    = '", trim(dst%vars(2)%char_data(1)), "'"
    print *, "filename = '", trim(dst%vars(4)%char_data(1)), "'"

    ! one scalar field to confirm basic copy works
    if (trim(dst%vars(1)%var_name) /= "author%fav_reals") then
      print *, "FAIL copy2: vars(1)%var_name: '", trim(dst%vars(1)%var_name)
      error stop
    end if

    ! real_data
    if (.not. allocated(dst%vars(1)%real_data)) then
      print *, "FAIL copy2: vars(1)%real_data not allocated"
      error stop
    end if
    if (size(dst%vars(1)%real_data) /= 2) then
      print *, "FAIL copy2: vars(1)%real_data size: ", size(dst%vars(1)%real_data)
      error stop
    end if
    if (dst%vars(1)%real_data(1) /= 1.0_dp) then
      print *, "FAIL copy2: vars(1)%real_data(1): ", dst%vars(1)%real_data(1)
      error stop
    end if
    if (dst%vars(1)%real_data(2) /= 2.0_dp) then
      print *, "FAIL copy2: vars(1)%real_data(2): ", dst%vars(1)%real_data(2)
      error stop
    end if

    ! char_data
    if (trim(dst%vars(2)%char_data(1)) /= "J.") then
      print *, "FAIL copy2: vars(2)%char_data(1): ", trim(dst%vars(2)%char_data(1))
      error stop
    end if
    if (trim(dst%vars(3)%char_data(1)) /= "H.J.") then
      print *, "FAIL copy2: vars(3)%char_data(1): ", trim(dst%vars(3)%char_data(1))
      error stop
    end if
    if (trim(dst%vars(3)%char_data(2)) /= "Teunissen") then
      print *, "FAIL copy2: vars(3)%char_data(2): ", trim(dst%vars(3)%char_data(2))
      error stop
    end if
    if (trim(dst%vars(4)%char_data(1)) /= "another_file") then
      print *, "FAIL copy2: vars(4)%char_data(1): ", trim(dst%vars(4)%char_data(1))
      error stop
    end if

    print *, "All Copy 2 Tests Passed"
  end subroutine

end program allocatable_component_struct_array_02