BEGIN_TEMPLATE

BEGIN_PROVIDER [ integer, output_$NAME ]
  implicit none
  BEGIN_DOC  
!  Output file for $NAME
  END_DOC
  integer :: getUnitAndOpen
  call ezfio_set_output_empty(.False.)
  output_$NAME = getUnitAndOpen(trim(ezfio_filename)//'/output/'//'$NAME','w')
END_PROVIDER

SUBST [ NAME ]

AO            ;;
MO            ;;
AO_integrals  ;;
MO_integrals  ;;

END_TEMPLATE


