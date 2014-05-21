 use bitmasks

 BEGIN_PROVIDER [integer, det_num]
  det_num = 10
 END_PROVIDER

 BEGIN_PROVIDER [ integer(bit_kind), det_provider, (N_int,2,det_num)]
&BEGIN_PROVIDER [ double precision , det_coef_provider, (det_num) ]
 use bitmasks
 implicit none
 integer :: i
 det_provider = 0
 det_provider(1,1,1 ) = #001f ! 0000 0000 0001 1111
 det_provider(1,1,2 ) = #003b ! 0000 0000 0011 1011
 det_provider(1,1,3 ) = #008f ! 0000 0000 1000 1111
 det_provider(1,1,4 ) = #0057 ! 0000 0000 0101 0111
 det_provider(1,1,5 ) = #100f ! 0001 0000 0000 1111
 det_provider(1,1,6 ) = #001f ! 0000 0000 0001 1111
 det_provider(1,1,7 ) = #003b ! 0000 0000 0011 1011
 det_provider(1,1,8 ) = #00c7 ! 0000 0000 1100 0111
 det_provider(1,1,9 ) = #00ab ! 0000 0000 1010 1011
 det_provider(1,1,10) = #0073 ! 0000 0000 0111 0011
 det_provider(1,2,1 ) = #0007 ! 0000 0000 0001 0111
 det_provider(1,2,2 ) = #0023 ! 0000 0000 0010 0011
 det_provider(1,2,3 ) = #0023 ! 0000 0000 0010 0011
 det_provider(1,2,4 ) = #0023 ! 0000 0000 0010 0011
 det_provider(1,2,5 ) = #0015 ! 0000 0000 0001 0101
 det_provider(1,2,6 ) = #000d ! 0000 0000 0000 1101
 det_provider(1,2,7 ) = #0007 ! 0000 0000 0000 0111
 det_provider(1,2,8 ) = #0007 ! 0000 0000 0000 0111
 det_provider(1,2,9 ) = #0007 ! 0000 0000 0000 0111
 det_provider(1,2,10) = #0007 ! 0000 0000 0000 0111
 det_coef_provider = (/   &
   0.993536117982429D+00, &
  -0.556089064313864D-01, &
   0.403074722590178D-01, &
   0.403074717461626D-01, &
  -0.340290975461932D-01, &
  -0.340290958781670D-01, &
  -0.333949939765448D-01, &
   0.333418373363987D-01, &
  -0.316337211787351D-01, &
  -0.316337207748718D-01  &
/)

!do i=1,10
!  call write_bitstring( 6, det_provider(1,1,i), N_int )
!enddo
!print *,  ''
!do i=1,10
!  call write_bitstring( 6, det_provider(1,2,i), N_int )
!enddo
!print *,  ''


END_PROVIDER
