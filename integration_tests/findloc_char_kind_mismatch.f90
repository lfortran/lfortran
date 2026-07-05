program findloc_char_kind_mismatch
  ! findloc with mismatched character kinds should emit a clear error
  ! message naming both types. This is a compile-time error test —
  ! gfortran rejects it with:
  !   Error: Argument 'array' of 'findloc' intrinsic at (1) must be in
  !   type conformance to argument 'value' at (2)
  character(kind=4, len=1) :: names(1)
  character(kind=1, len=1) :: key
  print *, findloc(names, key)
end program
