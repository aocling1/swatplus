!Change in time_read.f90 from call xmon (time%day_start, mo, day_mo) to call xmon (time%day_start,time%year_start, mo, day_mo)
!Distinguishing between equal and leap years
subroutine xmon(jd, year, mo, day_mo)
    use time_module
    use input_file_module
    implicit none
    integer, intent(in) :: jd, year
    integer, intent(out) :: mo, day_mo
    integer :: i_mo, m1, nda

    if ((mod(year, 4) == 0 .and. mod(year, 100) /= 0) .or. mod(year, 400) == 0) then
          do i_mo = 1, 12
              m1 = i_mo + 1
              nda = ndays(m1)
              if (jd <= nda) then
                  mo = i_mo
                  day_mo = jd - ndays(i_mo)
                  return
              end if
          end do
    else
          do i_mo = 1, 12
              m1 = i_mo + 1
              nda = ndays_noleap(m1)
              if (jd <= nda) then
                  mo = i_mo
                  day_mo = jd - ndays_noleap(i_mo)
                  return
              end if
          end do
    end if
end subroutine xmon
