program intrinsics_465
implicit none
character(:), allocatable :: s
character(132) :: raw

raw = '   "output": ["node_6_output"],   '
s = trim(adjustl(raw))
if (index(s, '"output"') /= 1) then
   print *, "first index =", index(s, '"output"')
   error stop "index of needle in first value should be 1"
end if

raw = '   "opType": "Flatten"   '
s = trim(adjustl(raw))
if (len(s) /= 19) then
   print *, "len(s) =", len(s)
   error stop "deferred-length assignment should give len 19"
end if
if (index(s, '"output"') /= 0) then
   print *, "stale index =", index(s, '"output"')
   error stop "index() must not read past the declared length of string"
end if

if (index(s, '"opType"') /= 1) then
   print *, "opType index =", index(s, '"opType"')
   error stop "index of opType in new value should be 1"
end if
end program intrinsics_465