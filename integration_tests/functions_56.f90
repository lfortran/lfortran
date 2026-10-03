program functions_56
  use functions_56_mod
  implicit none

  type(mha_layer) :: layer

  layer%sequence_length = 2
  layer%head_size = 1
  layer%n_heads = 1

  call layer%do_backward()

  ! Check allocation
  if (.not. allocated(layer%v_heads)) then
     error stop "v_heads not allocated"
  end if

  ! Check shape
  if (size(layer%v_heads,1) /= 2 .or. size(layer%v_heads,2) /= 1 .or. size(layer%v_heads,3) /= 1) then
     error stop "unexpected shape of v_heads"
  end if

  ! Check values
  if (any(layer%v_heads /= 1.0)) then
     error stop "v_heads values incorrect"
  end if

  print *, "OK"

end program