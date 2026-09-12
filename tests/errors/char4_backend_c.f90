program char4_backend_c
! The C, C++ and WASM backends represent a character value as a byte string,
! so they cannot carry the four byte code units of ISO 10646. They must say so
! rather than silently emitting a byte string.
character(kind=4, len=3) :: wide
wide = 4_"abc"
print *, len(wide)
end program
