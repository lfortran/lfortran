program intrinsics_60
    character :: c, d, e, f
    open(11, file="intrinsics_60_data.txt", form="formatted", access="stream", status="old")
    read(11, *) c, d, e, f
    print *, c, d, e, f
    print *, iachar(c), iachar(d), iachar(e), iachar(f)
    if (iachar(c) /= 97) error stop
    if (iachar(d) /= 90) error stop
    if (iachar(e) /= 195) error stop
    if (iachar(f) /= 194) error stop
  end program
