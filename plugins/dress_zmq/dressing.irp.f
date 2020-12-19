use bitmasks


BEGIN_PROVIDER [ integer, N_dress_teeth ]
  N_dress_teeth = 10
END_PROVIDER

BEGIN_PROVIDER [ double precision, dress_norm_acc, (0:N_det, N_states) ]
&BEGIN_PROVIDER [ double precision, dress_norm, (0:N_det, N_states) ]
&BEGIN_PROVIDER [ double precision, dress_teeth_size, (0:N_det, N_states) ]
&BEGIN_PROVIDER [ integer, dress_teeth, (0:N_dress_teeth+1, N_states) ]
  implicit none
  integer :: i, j, st, nt
  double precision :: norm_sto, jump, norm_mwen, norm_loc
  
  if(N_states /= 1) stop "dress_sto may not work with N_states /= 1"
  
  do st=1,N_states
    dress_teeth(0,st) = 1
    norm_sto = 1d0
    do i=1,N_det
      dress_teeth(1,st) = i
      jump = (1d0 / dfloat(N_dress_teeth)) * norm_sto
      if(psi_coef_generators(i,1)**2 < jump / 2d0) exit
      norm_sto -= psi_coef_generators(i,1)**2
    end do

    norm_loc = 0d0
    dress_norm_acc(0,st) = 0d0
    nt = 1
    
    do i=1,dress_teeth(1,st)-1
      dress_norm_acc(i,st) = dress_norm_acc(i-1,st) + psi_coef_generators(i,st)**2
    end do

    do i=dress_teeth(1,st), N_det_generators!-dress_teeth(1,st)+1
      norm_mwen = psi_coef_generators(i,st)**2!-1+dress_teeth(1,st),st)**2
      dress_norm_acc(i,st) = dress_norm_acc(i-1,st) + norm_mwen 
      norm_loc += norm_mwen
      if(norm_loc > (jump*dfloat(nt))) then
        nt = nt + 1
        dress_teeth(nt,st) = i
      end if
    end do
    if(nt > N_dress_teeth+1) then
      print *, "foireouse dress_teeth", nt, dress_teeth(nt,st), N_det
      stop
    end if

    dress_teeth(N_dress_teeth+1,st) = N_det+1
    norm_loc = 0d0
    do i=N_dress_teeth, 0, -1
      dress_teeth_size(i,st) = dress_norm_acc(dress_teeth(i+1,st)-1,st) - dress_norm_acc(dress_teeth(i,st)-1, st)
      dress_norm_acc(dress_teeth(i,st):dress_teeth(i+1,st)-1,st) -= dress_norm_acc(dress_teeth(i,st)-1, st)
      dress_norm_acc(dress_teeth(i,st):dress_teeth(i+1,st)-1,st) = &
          dress_norm_acc(dress_teeth(i,st):dress_teeth(i+1,st)-1,st) / dress_teeth_size(i,st)
      dress_norm(dress_teeth(i,st), st) = dress_norm_acc(dress_teeth(i,st), st)
      do j=dress_teeth(i,st)+1, dress_teeth(i+1,1)-1
        dress_norm(j,1) = dress_norm_acc(j, st) - dress_norm_acc(j-1, st)
      end do
    end do
  end do
END_PROVIDER



BEGIN_PROVIDER [ double precision, delta_ij, (N_states,N_det,2) ]
  use bitmasks
  implicit none

  integer                        :: i,j,k
  
  double precision, allocatable  :: dress(:), del(:,:), del_s2(:,:)
  double precision               :: E_CI_before(N_states), relative_error
!  double precision, save         :: errr = 0d0

  allocate(dress(N_states), del(N_states, N_det), del_s2(N_states, N_det))

  delta_ij = 0d0

  E_CI_before(:) = dress_E0_denominator(:) + nuclear_repulsion
  threshold_selectors = 1.d0
  threshold_generators = 1d0 
!  if(errr /= 0d0) then
!    errr = errr / 2d0 
!  else
!    errr = 1d-4
!  end if
  relative_error = 1.d-4
  call write_double(6,relative_error,"Convergence of the stochastic algorithm")

  call ZMQ_dress(E_CI_before, dress, del, del_s2, abs(relative_error))
  delta_ij(:,:,1) = del(:,:)
  delta_ij(:,:,2) = del_s2(:,:)

  deallocate(dress, del, del_s2)
  
END_PROVIDER



