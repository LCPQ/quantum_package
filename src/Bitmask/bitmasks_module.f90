module bitmasks
  integer, parameter             :: bit_kind_shift = 6   ! 5: 32 bits, 6: 64 bits
  integer, parameter             :: bit_kind_size = 64
  integer, parameter             :: bit_kind = 64/8
  integer, parameter             :: s_hole  = 1
  integer, parameter             :: s_part  = 2
  integer, parameter             :: d_hole1 = 3
  integer, parameter             :: d_part1 = 4
  integer, parameter             :: d_hole2 = 5
  integer, parameter             :: d_part2 = 6
end module
