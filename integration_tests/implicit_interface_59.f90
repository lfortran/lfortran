! A non-CHARACTER actual argument is passed by classic F77 storage association
! to a CHARACTER dummy of an external procedure that is DEFINED IN THE SAME
! FILE, so the compiler can see the real dummy list at the call site.
!
! This is the companion of implicit_interface_51, which compiles the definition
! separately. There the caller only knows the per-call synthesized interface,
! whose dummy is numeric, so no hidden length is emitted -- exactly what
! gfortran does, and the callee must not read LEN(). Here the definition is
! visible, so LFortran takes the ABI decision from the real CHARACTER dummy and
! emits the hidden trailing length; without that the call and the definition
! would disagree on the argument count and the LLVM module would not verify.
!
! This also pins down the call-site test that decides between the CHARACTER and
! the storage-associated branch: it must come from the actual argument's ASR
! type, not from the LLVM type of the emitted value, which conveys nothing once
! LLVM uses opaque pointers.
!
! This test assumes a little-endian target (as do the CI platforms): the least
! significant byte of buf(1) aliases the first character of the dummy.
program implicit_interface_59
    implicit none
    integer(8) :: buf(2)
    external :: bump_first_byte
    buf(1) = 65     ! low byte is 'A'
    buf(2) = 0
    call bump_first_byte(buf)
    ! The callee overwrote the first character with 'Z' (code 90) in place.
    if (iand(buf(1), 255_8) /= 90) error stop
    ! The remaining bytes of buf(1) and the other element must be untouched.
    if (buf(1) /= 90) error stop
    if (buf(2) /= 0) error stop
    print *, "OK"
end program implicit_interface_59

subroutine bump_first_byte(text)
    implicit none
    character(len=8), intent(inout) :: text
    if (len(text) /= 8) error stop
    if (ichar(text(1:1)) /= 65) error stop
    text(1:1) = 'Z'
end subroutine bump_first_byte
