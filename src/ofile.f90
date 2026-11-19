subroutine ofile

  use mod_set, only: set_init, n_in

  implicit none

  ! ------------------------------------------------------------------ !
  !     input                                                          !
  ! ------------------------------------------------------------------ !

  open(15,file='../in/nse.in', action = 'read')
  call set_init(15)

  !open(40,file='../in/dat/part.fz471' , action = 'read')
  open(40,file='../in/dat/part.ame.fz4421', action = 'read')

  !!e- e+ capture rate
  !!T9(0.01-100), lrho(1-11)
  !open(42,file='../in/ntwk/ecap.z219', action='read')
  !open(43,file='../in/ntwk/pcap.z219', action='read')

  open(42,file='../in/ntwk/ecap.z3859', action='read')
  open(43,file='../in/ntwk/pcap.z3859', action='read')


  !! rhot table
  if( n_in == 0 ) then
     open(51,file='../in/hydro.in', action = 'read')
  else if( n_in == 1 ) then
     open(51,file='../in/table.in', action = 'read')
  else
     stop 'ofile(): error bad n_in'
  end if


  open(52, file = '../in/pns.neu.dat', action = 'read')

  !     input                                                          !
  ! ------------------------------------------------------------------ !


  ! ------------------------------------------------------------------ !
  !     output                                                         !
  ! ------------------------------------------------------------------ !

  open(60,file='../res/nse.log'  , action = 'write')
  open(61,file='../res/hydro.out', action = 'write')
  open(62,file='../res/abund.out', action = 'write')
  open(63,file='../res/flow.out' , action = 'write')
  open(64,file='../res/x_end.dat', action = 'write')
  open(65,file='../res/nse.tbl'  , action = 'write')
  open(66,file='../res/nse.dat'  , action = 'write', form = 'unformatted' )

  !..check
  !open(70,file='../res/converge.log', form = 'unformatted', action = 'write')
  open(70,file='../res/converge.log', action = 'write')
  open(71,file='../res/no_converge.log', action = 'write')

  !     output                                                         !
  ! ------------------------------------------------------------------ !

  return

end subroutine ofile
