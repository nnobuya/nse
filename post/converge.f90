program rhoye

  implicit none

  real*8 :: ye, de, te
  integer:: n_conv, i_try, io
  integer:: num_conv = 0, num_bad = 0, num_not = 0

  integer:: i, ier
  real*8 :: ye0 = 0.d0
  character:: ifl(1:3)*50, ofl(1:3)*50



  open(50, file = '../res/converge.log', form = 'unformatted', action = 'read')

  open(60, file = './res/conv_not.dat' , status = 'replace', action = 'write')
  open(61, file = './res/conv_good.dat', status = 'replace', action = 'write')
  open(62, file = './res/conv_bad.dat' , status = 'replace', action = 'write')


  rd_loop: do

     read(50,iostat=ier) ye, de, te, n_conv, i_try
     if( ier /= 0 ) exit rd_loop

     if ( ye /= ye0 ) then
        write(*,'(5x,"ye =",f7.3)') ye
        ye0 = ye
     end if

     !..count
     if ( n_conv == 0 ) then
        num_conv = num_conv + 1
     else if( n_conv == 1) then
        num_bad  = num_bad  + 1
     else if( n_conv == -1) then
        num_not  = num_not  + 1
     else
        stop 'error'
     end if


     io = 61 + n_conv

     write(io,'(1p4e14.5)') &
          & ye, log10(de), log10(te), log10(de *ye)

  end do rd_loop

  close(50)
  close(60)
  close(61)
  close(62)

  write(*,'(x,"converged",7x,"bad",7x,"not")')
  write(*,'(3i10)') num_conv, num_bad, num_not



  stop

end program rhoye
