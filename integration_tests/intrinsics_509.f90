program intrinsics_509
implicit none
! selected_real_kind must return a kind whose precision is at least p.
! kind 8 has precision 15. kind 16 (binary128) has precision 33.
! Where x87 kind 10 exists (precision 18), p=16 selects 10; otherwise 16.
! p=30 and p=33 are past kind 10, so both select 16.
integer, parameter :: dp = selected_real_kind(15)
integer, parameter :: k16 = selected_real_kind(16)
integer, parameter :: k30 = selected_real_kind(30)
integer, parameter :: k33 = selected_real_kind(33)
real(k16) :: x16
real(k30) :: x30
real(k33) :: x33
character(len=32) :: field
character(len=22) :: e22
character(len=24) :: e24
character(len=8) :: f8
character(len=12) :: es12
character(len=46) :: es46
character(len=48) :: es48
character(len=40) :: f40
real(k30) :: xeps
integer :: p

if (dp /= 8) error stop
if (k30 /= 16) error stop
if (k33 /= 16) error stop
if (precision(x30) /= 33 .or. range(x30) /= 4931) error stop
if (precision(x33) /= 33 .or. range(x33) /= 4931) error stop
! x86 GFortran selects kind 10 (80-bit, precision 18) for p=16.
if (k16 == 16) then
    if (precision(x16) /= 33) error stop
else if (k16 == 10) then
    if (precision(x16) < 16) error stop
else
    error stop
end if
if (kind(x16) /= k16) error stop
if (kind(x30) /= 16) error stop
if (kind(x33) /= 16) error stop

p = 15
if (selected_real_kind(p) /= 8) error stop
p = 16
if (selected_real_kind(p) /= k16) error stop
p = 30
if (selected_real_kind(p) /= 16) error stop
p = 33
if (selected_real_kind(p) /= 16) error stop

! E/ES of a binary128 value must keep an exponent outside the real(64) range.
! selected_real_kind(30) is kind 16 on both LFortran and GFortran.
write(field, '(E12.3E4)') tiny(x30)
call check_real16_field(field)
write(field, '(E12.3E4)') huge(x30)
call check_real16_field(field)
write(field, '(ES11.2E4)') tiny(x30)
call check_real16_field(field)
write(field, '(ES11.2E4)') huge(x30)
call check_real16_field(field)

! e >= 10 is wide enough for the exponent of 1. e >= 12 must not drop a digit.
write(e22, '(E22.5E10)') 1.0_k30
if (e22 /= '   0.10000E+0000000001') error stop
write(e22, '(ES22.5E10)') 1.0_k30
if (e22 /= '   1.00000E+0000000000') error stop
write(e22, '(EN22.4E10)') 1.0_k30
if (e22 /= '    1.0000E+0000000000') error stop
write(e24, '(E24.5E12)') 1.0_k30
if (e24 /= '   0.10000E+000000000001') error stop

! A remainder past the last printed digit still rounds under RU and RD.
write(f8, '(RU,F8.4)') 1.0e-10_k30
if (f8 /= '  0.0001') error stop
write(f8, '(RD,F8.4)') -1.0e-10_k30
if (f8 /= ' -0.0001') error stop
write(f8, '(F8.4)') 1.0e-10_k30
if (f8 /= '  0.0000') error stop

! 1 + epsilon is not a 34-digit terminating decimal. Directed rounding and a
! 35-digit field must see the remainder past those digits.
xeps = 1.0_k30 + epsilon(1.0_k30)
write(f8, '(RU,F8.4)') xeps
if (f8 /= '  1.0001') error stop
write(f8, '(RU,F8.2)') xeps
if (f8 /= '    1.01') error stop
write(f8, '(RD,F8.4)') -xeps
if (f8 /= ' -1.0001') error stop
write(es12, '(RU,ES12.2)') xeps
if (es12 /= '    1.01E+00') error stop
write(es46, '(ES46.34E4)') xeps
if (es46 /= '    1.0000000000000000000000000000000002E+0000') error stop

! One more digit: the 37th is 5 with a nonzero tail, so nearest rounds 8 to 9.
! An exact power of two must keep its integer digits, not a 36-digit prefix.
xeps = 1.0_k30 + 2*epsilon(1.0_k30)
write(es48, '(ES48.35E4)') xeps
if (es48 /= '     1.00000000000000000000000000000000039E+0000') error stop
write(f40, '(F40.0)') 2.0_k30**120
if (f40 /= '  1329227995784915872903807060280344576.') error stop
contains
    subroutine check_real16_field(field)
        character(len=*), intent(in) :: field
        integer :: i, epos
        logical :: nonzero
        if (index(field, '493') == 0) error stop
        if (index(field, 'Infinity') /= 0) error stop
        if (index(field, 'NaN') /= 0 .or. index(field, 'nan') /= 0) error stop
        epos = index(field, 'E')
        if (epos == 0) epos = index(field, 'e')
        if (epos == 0) epos = index(field, 'D')
        if (epos <= 1) error stop
        nonzero = .false.
        do i = 1, epos - 1
            if (field(i:i) >= '1' .and. field(i:i) <= '9') nonzero = .true.
        end do
        if (.not. nonzero) error stop
    end subroutine check_real16_field
end program
