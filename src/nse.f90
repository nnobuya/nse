program nse

  use mod_const, only: nel
  use mod_set  , only: ndt, neu, n_in, &
       & temp_lim, t_min, ti_init, dt_init, ye_init, set_nuclei, set_rhot
  use mod_out  , only: out_main, out_table
  use mod_ffn  , only: ffn_set, ffn_init

  implicit none

  !..main
  integer:: istp = 0
  real*8 :: ti, ye, de, t9, rad, de_lg
  real*8 :: dti   = 0.d0
  real*8 :: dye = 0.d0
  real*8 :: dye_neu, dye_pn, dye_np
  real*8 :: dye_ec, dye_pc, qv_ave
  real*8 :: rmu_p_lg, rmu_n_lg
  real*8 :: ye_out_bak

  real*8 :: enu, enu_ec, enu_pc, recy(1:nel), rpcy(1:nel)
  real*8 :: x(1:nel)



  ! ================================================================== !
  !                                                                    !
  !    read                                                            !
  !                                                                    !
  ! ================================================================== !

  !..open files
  call ofile

  !..set nuclei
  call set_nuclei

  !..set rhot
  call set_rhot(51)

  !    read                                                            !
  ! ================================================================== !


  !..ffn rate set
  call ffn_init(42, 43)
  call ffn_set

  call logo


  ! ================================================================== !
  !                                                                    !
  !    main loop                                                       !
  !                                                                    !
  ! ================================================================== !

  if( n_in == 0 ) then
     ti = ti_init
  else
     ti = 0.d0
  end if

  dti = dt_init
  ye  = ye_init


  write(*,*)

  ye_out_bak = 0.0

  main_loop: do

     !..parameter
     istp = istp + 1

     ! --------------------------------------------------------------- !
     !    NSE Abundances                                               !
     ! --------------------------------------------------------------- !

     call therm( istp, ti, de, t9, ye, rad, de_lg )

     call eq_nse( istp, t9, ye, de, rmu_p_lg, rmu_n_lg, x(1:nel) )
     !  in: istp, t9, ye, de
     ! i&o: rmu_p_lg, rmu_n_lg
     ! out: x(:)

     !    NSE Abundances                                               !
     ! --------------------------------------------------------------- !



     ! --------------------------------------------------------------- !
     !    Weak Interaction                                             !
     ! --------------------------------------------------------------- !

     call sstate( min(de,1.d13), t9, ye, x(:), &
          & enu_ec, enu_pc, dye_ec, dye_pc, qv_ave, recy(:), rpcy(:))
     !  in : de, t9, ye, x(nel)
     !  out: enu_ec, enu_pc, dye_ec, dye_pc, qv_ave, recy(nel), rpcy(nel)


     if( neu == 1 ) then
        call neutrino( ti, ye, rad, dye_np, dye_pn, istp )
        dye_neu = dye_np - dye_pn
     else
        dye_neu = 0.d0
     end if

     enu = enu_ec + enu_pc
     dye = ( dye_ec + dye_pc ) + dye_neu

     !    Weak Interaction                                             !
     ! --------------------------------------------------------------- !


 
     !..output
     call out_main(istp, ti, ye, de, t9, x(:), &
          & recy(:), rpcy(:), dye, dti, enu)
     !  in: all


     !..table
     if( n_in == 0 .and. istp == 1 ) then
        write(65,*) 'no table output'
     else if( n_in == 1 ) then
        call out_table( istp, de_lg, t9, ye, x(:), &
             & enu_ec, enu_pc, dye_ec, dye_pc, qv_ave )
     end if
     ! in: all

     !..output & stop
     if( ( ti >= t_min .and. t9*1.d9 <= temp_lim ) &
          & .or. istp >= 10000000 &
          & .or. ( n_in == 1 .and. istp == ndt) ) then
        exit main_loop
     end if


     ! --------------------------------------------------------------- !
     !    update                                                       !
     ! --------------------------------------------------------------- !

     if( n_in == 0 ) then

        dti = min( 0.005d0 * abs( ye /dye ), 0.001d0 )

        ti  = ti + dti
        ye  = ye + dye *dti
     else
        dti = 0.d0
     end if

     if (ye /= ye_out_bak) write(*,'(5x, "Ye =", f10.5)') ye
     ye_out_bak = ye


     !    update                                                       !
     ! --------------------------------------------------------------- !


  end do main_loop

  !    main loop                                                       !
  ! ================================================================== !


  call out_main( 0, ti, ye, de, t9, x(:), recy(:), rpcy(:), dye, dti, enu )
  !  in: all


  close(60)
  close(61)
  close(62)
  close(63)
  close(64)


  stop 'normal temination'

end program nse
