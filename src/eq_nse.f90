subroutine eq_nse( istep, t9, ye, de, rmu_p_lg, rmu_n_lg, x )

  use mod_const, only: nel
  use mod_set  , only: n_in, nz

  implicit none

  integer, parameter:: n_try = 12

  real*8, external:: coul_mu

! 3,4,5,7,8,9,10,12,16,17
! 6, 11, 13,14,15

  real(8), parameter:: rmu_p_ret(3:n_try) = &
       & (/0.d0,  0.5d0, -0.5d0, -5.d0, -0.1d0, -0.5d0, -1.0d0, &
       &  -10.d0,  1.d0,  10.d0/)
  real(8), parameter:: rmu_n_ret(3:n_try) = &
       & (/0.d0, 0.5d0, -0.5d0,  5.d0, 0.1d0,  0.5d0,  1.0d0, &
       &   10.d0,  1.d0, -10.d0/)

  ! NG: (rmu_p_ret, rmu_n_ret) =
  !  (5.0, 5.0), (-20.0,20.0), (0.0,0.0), (-50.0,50.0),
  !  (100.0,-100.0), (-100.0,100.0)


  integer, intent(in)   :: istep
  real*8 , intent(in)   :: t9, ye, de
  real*8 , intent(out)  :: x(1:nel)
  real*8 , intent(inout):: rmu_p_lg, rmu_n_lg

  integer:: i, i_try, n_conv
  real*8 :: gel(1:nel), rmu_coul(1:nel)
  real*8 :: de_nr, ye_nr   !! newton raphson

  logical, save:: conv_prev = .true.

  call part_func( ye, de, t9, gel(:) )
  !  in: t9, ye, de
  ! out: g(nel), gel(nel)


  !..initial
  de_nr = de
  ye_nr = ye

  iter_loop: do i_try = 1, n_try

     if ( istep == 1 .and. .not. conv_prev) cycle iter_loop

     !..set initial chemical potential
     if( ( istep == 1 .and. i_try == 1 ) .or. i_try == 2 ) then

        !..initial rmu_p_lg, rmu_n_lg
        call ini_set( de, t9, ye, gel(:), rmu_p_lg, rmu_n_lg )
        !  in: de, t9, ye, gel
        ! out: rmu_p_lg, rmu_n_lg
     else
        rmu_p_lg = rmu_p_ret(i_try)
        rmu_n_lg = rmu_n_ret(i_try)
     end if


     call new_raph( gel(:), de_nr, ye_nr, rmu_p_lg, rmu_n_lg, x(:), n_conv )
     ! in   : gel(nel)
     ! inout: de_nr, ye_nr, rmu_p_lg, rmu_n_lg
     ! out  : xrl(nel)

     if( n_conv == 0 .or. n_conv == 1 .or. n_conv == 2) exit iter_loop

     de_nr = de
     ye_nr = ye

  end do iter_loop

  if (i_try <= n_try) then
     conv_prev = .true.
     write(70,'("converged at trial =", i5, 1p, *(e14.5))') &
          & i_try, ye, t9, log10(de)
  else
     conv_prev = .false.
     write(71,'("not converge", 1p, *(e14.5))') ye, t9, log10(de)
     write(* ,'("not converge", 1p, *(e14.5))') ye, t9, log10(de)
  end if

  if (n_conv == 2) write(*,*) 'not good convergence'


  !write(70) ye, de, t9 *1.d9, n_conv, i_try


  return

end subroutine eq_nse
