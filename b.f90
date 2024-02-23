function verify_(string, set, back, kind) result(r)
                character(len=*) :: string
                character(len=*) :: set
                logical, optional :: back
                integer, intent(in) :: kind
                integer(4) :: r
                integer :: i, j
                logical :: back_val, matched

                back_val = .false.
                r = 0

                if( present(back) ) then
                    back_val = back
                end if

                if( back_val ) then
                    do i = len(string), 1, -1
                        matched = .false.
                        do j = 1, len(set)
                            if( string(i:i) == set(j:j) ) then
                                matched = .true.
                            end if
                        end do
                        if (.not. matched) then
                            r = i
                            return
                        end if
                    end do
                else
                    do i = 1, len(string), 1
                        matched = .false.
                        do j = 1, len(set)
                            if( string(i:i) == set(j:j) ) then
                                matched = .true.
                            end if
                        end do
                        if (.not. matched) then
                            r = i
                            return
                        end if
                    end do
                end if
            end function
