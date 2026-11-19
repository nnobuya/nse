subroutine therm( istp, ti, de, t9, ye, rd, de_lg )

  use mod_set, only: ndt, n_in, rti, rrh, rrh_lg, rte, rye, rrd

  implicit none

  !..io
  integer, intent(in) :: istp
  real*8 , intent(in) :: ti
  real*8 , intent(out):: de, t9, ye, rd, de_lg

  !..local
  real*8 :: h_rh, h_te, h_rd
  integer:: i


  if(n_in == 0) then

     !..get i
     int_pol: do i = 2, ndt
        if( ti < rti(i) ) exit int_pol
     end do int_pol

     !..interpol
     h_rh = ( rrh(i) - rrh(i-1) ) /( rti(i) - rti(i-1) )
     h_te = ( rte(i) - rte(i-1) ) /( rti(i) - rti(i-1) )
     h_rd = ( rrd(i) - rrd(i-1) ) /( rti(i) - rti(i-1) )

     de = rrh(i-1) + h_rh *( ti - rti(i-1) )
     t9 = rTe(i-1) + h_te *( ti - rti(i-1) )
     t9 = t9 /1.d9
     rd = rrd(i-1) + h_rd *( ti - rti(i-1) )

     de_lg = log10(de)

  else

     de = rrh(istp)
     t9 = rte(istp) /1.d9
     ye = rye(istp)
     rd = rrd(istp)

     de_lg = rrh_lg(istp)

  end if


  return

end subroutine therm
