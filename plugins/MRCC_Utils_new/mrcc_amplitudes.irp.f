subroutine get_excitation_operators_for_one_ref(det_ref,i_state,ndetnonref,N_connect_ref,excitation_operators,amplitudes_phase_less,index_connected)
  use bitmasks
  implicit none
  integer(bit_kind), intent(in)  :: det_ref(N_int,2)
  integer, intent(in)            :: i_state,ndetnonref
  integer*2, intent(out)         :: excitation_operators(5,ndetnonref)
  integer, intent(out)           :: index_connected(ndetnonref)
  integer, intent(out)           :: N_connect_ref
  double precision, intent(out)  :: amplitudes_phase_less(ndetnonref)
  
  integer                        :: i,j,k,l,degree,h1,p1,h2,p2,s1,s2
  integer                        :: exc(0:2,2,2)
  double precision               :: phase,hij
  BEGIN_DOC
  ! This subroutine provides all the amplitudes and excitation operators
  ! that one needs to go from the reference to the non reference wave function
  ! you enter with det_ref that is a reference determinant
  !
  ! N_connect_ref is the number of determinants belonging to psi_non_ref
  ! that are connected to det_ref.
  !
  ! amplitudes_phase_less(i) = amplitude phase less t_{I->i} = <I|H|i> * lambda_mrcc(i) * phase(I->i)
  !
  ! excitation_operators(:,i) represents the holes and particles that
  ! link the ith connected determinant to det_ref
  ! if                           :: 
  ! excitation_operators(5,i) =  2 :: double excitation alpha
  ! excitation_operators(5,i) = -2 :: double excitation beta
  !!! excitation_operators(1,i)  :: hole 1
  !!! excitation_operators(2,i)  :: particle 1
  !!! excitation_operators(3,i)  :: hole 2
  !!! excitation_operators(4,i)  :: particle 2
  ! else if                      :: 
  ! excitation_operators(5,i) =  1 :: single excitation alpha
  !!! excitation_operators(1,i)  :: hole 1
  !!! excitation_operators(2,i)  :: particle 1
  ! else if                      :: 
  ! excitation_operators(5,i) = -1 :: single excitation beta
  !!! excitation_operators(3,i)  :: hole 1
  !!! excitation_operators(4,i)  :: particle 1
  ! else if                      :: 
  !!! excitation_operators(5,i) =  0 :: double excitation alpha/beta
  !!! excitation_operators(1,i)  :: hole 1 alpha
  !!! excitation_operators(2,i)  :: particle 1 alpha
  !!! excitation_operators(3,i)  :: hole 2 beta
  !!! excitation_operators(4,i)  :: particle 2 beta
  END_DOC
  N_connect_ref = 0
  do i = 1, ndetnonref
    call i_H_j_phase_out(det_ref,psi_non_ref(1,1,i),N_int,hij,phase,exc,degree)
    if (dabs(hij) <= mo_integrals_threshold) then
        cycle
    endif
    N_connect_ref +=1
    index_connected(N_connect_ref) = i
    call decode_exc(exc,degree,h1,p1,h2,p2,s1,s2)
    amplitudes_phase_less(N_connect_ref) = hij * lambda_mrcc(i_state,i) !*phase
    
    if (degree==2) then

      excitation_operators(1,N_connect_ref) = h1
      excitation_operators(2,N_connect_ref) = p1
      excitation_operators(3,N_connect_ref) = h2
      excitation_operators(4,N_connect_ref) = p2
      if(s1==s2.and.s1==1)then                      ! double alpha
        excitation_operators(5,N_connect_ref) =  2
      elseif(s1==s2.and.s1==2)then                  ! double beta
        excitation_operators(5,N_connect_ref) = -2
      else                                          ! double alpha/beta
        excitation_operators(5,N_connect_ref) =  0
      endif

    else if(degree==1) then

      if(s1==1)then                                ! mono alpha
        excitation_operators(5,N_connect_ref) = 1
        excitation_operators(1,N_connect_ref) = h1
        excitation_operators(2,N_connect_ref) = p1
      else                                         ! mono beta
        excitation_operators(5,N_connect_ref) = -1
        excitation_operators(3,N_connect_ref) = h1
        excitation_operators(4,N_connect_ref) = p1
      endif

    else

      N_connect_ref-=1

    endif
    
  enddo
  
end
