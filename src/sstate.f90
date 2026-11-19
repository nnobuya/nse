subroutine sstate(rho, t9, ye, x, &
     & enu_ec, enu_pc, dyedt_ec, dyedt_pc, qv_ave, recy, rpcy)

  use mod_const  , only: nel
  use mod_set    , only: na, nz, nbet, qm
  use mod_wagoner, only: wagoner_np, wagoner_pn
  use mod_ffn    , only: ibm, ifn, jfn, sffn

  implicit none

  !..io
  real*8, intent(in) :: rho, t9, ye, x(nel)
  real*8, intent(out):: enu_ec, enu_pc, dyedt_ec, dyedt_pc, qv_ave
  real*8, intent(out):: recy(nel), rpcy(nel)

  real*8:: y(nel), te_l, rhye_l

  real*8  :: recr(1:ibm), recn(1:ibm), rpcr(1:ibm), rpcn(1:ibm)

  !..local
  integer :: j, i0, i1
  real*8 :: qv(ibm), total


  !x(1:nel) = 10.d0**xrl(1:nel) *(1.d0 /rho)


  !-----weak-process rates by fnn(1982)   at bnl. aug. 1986
  te_l   = log10(t9) + 9.d0
  rhye_l = log10(rho *ye)

  !..weak-process rates by FFN(1982) at BNL. June 1986
  call sffn( te_l, rhye_l, recr(:), recn(:), rpcr(:), rpcn(:) )
  ! in : rhol, tl
  ! out: recr, recn, rpcr, rpcn


  !..change wagoner rate
  if( nbet == 1 ) then
     stop 'check: temperature!!'
     recr(1) = wagoner_pn(te_l)
     rpcr(1) = wagoner_np(te_l)
  end if


  dyedt_ec = 0.d0
  dyedt_pc = 0.d0

  enu_ec = 0.d0
  enu_pc = 0.d0

  qv(1:ibm) = 0.d0
  total     = 0.d0
  qv_ave    = 0.d0

  do j = 1, ibm
     i0 = ifn(j)
     i1 = jfn(j)

     if( i0 *i1 /= 0 ) then

        qv(j) = ( qm(i1) - qm(i0) ) + 0.511d0

        y(i0) = x(i0) /dble( na(i0) )
        y(i1) = x(i1) /dble( na(i1) )

        recy(i0) = recr(j) *y(i0)
        rpcy(i1) = rpcr(j) *y(i1)

        dyedt_ec = dyedt_ec - recr(j) *y(i0)
        dyedt_pc = dyedt_pc + rpcr(j) *y(i1)
        
        enu_ec = enu_ec + recn(j) *y(i0)
        enu_pc = enu_pc + rpcn(j) *y(i1)

        if ( nz(i0) >= 6 ) then
           total  = total  + y(i0)
           qv_ave = qv_ave + qv(j) *y(i0)
        end if

     end if
  end do


  qv_ave = qv_ave /total

  enu_ec = enu_ec *9.574143d17
  enu_pc = enu_pc *9.574143d17

  !..minimum check
  dyedt_ec = max(- dyedt_ec, 1.d-30)
  dyedt_ec = - dyedt_ec

  dyedt_pc = max( dyedt_pc, 1.d-30)


  return

end subroutine sstate
