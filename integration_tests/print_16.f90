program print_16
! A list-directed output statement with an empty output list writes an
! empty record. The leading blank emitted for list-directed output under
! --std=f23 must only be written when there are output items.
implicit none
integer :: u, n, ios
character(len=20) :: buf

open(newunit=u, file="print_16.txt", status="replace", form="formatted")
write(u, *)
write(u, '(a)', advance='no') "ab"
write(u, *)
write(u, *) "x"
close(u)

open(newunit=u, file="print_16.txt", status="old", form="formatted")
buf = "?"
read(u, '(a)', advance='no', size=n, iostat=ios) buf
if (n /= 0) error stop "empty list must write an empty record"
buf = "?"
read(u, '(a)', advance='no', size=n, iostat=ios) buf
if (n /= 2) error stop "empty list must not append a blank"
if (buf(1:2) /= "ab") error stop
buf = "?"
read(u, '(a)', advance='no', size=n, iostat=ios) buf
if (n /= 2) error stop "non-empty list must start with a blank"
if (buf(1:2) /= " x") error stop
close(u, status="delete")

print *
write(*, *)
print *, "PASS"
end program
