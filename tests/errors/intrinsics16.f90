! tests throwing an error when an unsupported or invalid intrinsic procedure is encountered

program intrinsics16
  call sub()
contains
  subroutine sub()
    intrinsic chdir     ! A valid intrinsic subroutine that is supported only by gnu
    intrinsic aaaa      ! An invalid intrinsic subroutine
    intrinsic :: bbbb   ! An invalid intrinsic function

    call chdir('.')
    call aaaa('.')
    print *, bbbb('.')
  end subroutine sub
end program intrinsics16
