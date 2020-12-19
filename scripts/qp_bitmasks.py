#! /usr/bin/env python2

BIT_KIND_SIZE=64


def int_to_string(s):
    """Transforms any integer to a string representation
    >>> print int_to_string(10)
    1010
    >>> print int_to_string(1024)
    10000000000
    >>> print int_to_string(123456789)
    111010110111100110100010101
    >>> print int_to_string(12345678912345678910)
    1010101101010100101010011000111110000001011001010010010000111110
    >>> print int_to_string(0)
    0
    >>> print int_to_string(-128)
    Traceback (most recent call last):
      File "<stdin>", line 1, in <module>
      File "qp_bitmasks.py", line 23, in int_to_string
        assert s>=0
    AssertionError
    """
    assert type(s) in (int, long)
    assert s>=0
    return str(s) if s in (0,1) else int_to_string(s>>1) + str(s&1) 


def string_to_bitmask(s,bit_kind_size=BIT_KIND_SIZE):
    """Transforms a string into an bitmask
    >>> print string_to_bitmask(10)
    Traceback (most recent call last):
      File "<stdin>", line 1, in <module>
      File "qp_bitmasks.py", line 30, in string_to_bitmask
        assert type(s) == str
    AssertionError
    >>> print string_to_bitmask('10')
    ['0000000000000000000000000000000000000000000000000000000000000010']
    >>> print string_to_bitmask('1010'*64)
    ['1010101010101010101010101010101010101010101010101010101010101010', '1010101010101010101010101010101010101010101010101010101010101010', '1010101010101010101010101010101010101010101010101010101010101010', '1010101010101010101010101010101010101010101010101010101010101010']
    >>> print string_to_bitmask('1010'*64,4)
    ['1010', '1010', '1010', '1010', '1010', '1010', '1010', '1010', '1010', '1010', '1010', '1010', '1010', '1010', '1010', '1010', '1010', '1010', '1010', '1010', '1010', '1010', '1010', '1010', '1010', '1010', '1010', '1010', '1010', '1010', '1010', '1010', '1010', '1010', '1010', '1010', '1010', '1010', '1010', '1010', '1010', '1010', '1010', '1010', '1010', '1010', '1010', '1010', '1010', '1010', '1010', '1010', '1010', '1010', '1010', '1010', '1010', '1010', '1010', '1010', '1010', '1010', '1010', '1010']
    """
    assert type(s) == str
    assert bit_kind_size > 0

    while len(s) % bit_kind_size != 0:
        s = '0'+s
    return [ s[i:i+bit_kind_size] for i in range(0,len(s),bit_kind_size) ]


def int_to_bitmask(s,bit_kind_size=BIT_KIND_SIZE):
    """Transforms a string into an bitmask
    >>> int_to_bitmask(1)
    ['0000000000000000000000000000000000000000000000000000000000000001']
    >>> int_to_bitmask(-1)
    ['1111111111111111111111111111111111111111111111111111111111111111']
    >>> int_to_bitmask(10)
    ['0000000000000000000000000000000000000000000000000000000000001010']
    >>> int_to_bitmask(-10)
    ['1111111111111111111111111111111111111111111111111111111111110110']
    >>> 
    """
    assert type(s) in (int, long)
    if s>=0:
      s = int_to_string(s)
      result = string_to_bitmask( s, bit_kind_size )
    else:
      s = int_to_string(-s-1)
      result = string_to_bitmask( s, bit_kind_size )
      result = [ x.replace('1','.').replace('0','1').replace('.','0') for x in result ]
    return result



class BitMask(object):

  """
  >>> A = BitMask( [ -127,47  ], bit_kind_size=8)
  >>> print A
  ['10000001', '00101111']
  >>> print A[0]
  -127
  >>> print A[1]
  47
  >>> A[0] = 127
  >>> print A
  ['01111111', '00101111']
  >>> A[1] = '100'
  >>> print A
  ['01111111', '00000100']
  >>> A[1] = '10000001'
  >>> print A
  ['01111111', '10000001']
  >>> print A[1]
  -127
  """
  

  def __init__(self,l=[],bit_kind_size=BIT_KIND_SIZE):
    self.bit_kind_size = bit_kind_size
    self._data_int = l

  def get_N_int(self):
    return len(self._data_int)
  N_int = property(fget=get_N_int)

  def __getitem__(self,i):
    return self._data_int[i]

  def __setitem__(self,i,value):
    if type(value) in (int,long):
        self._data_int[i] = value
    elif type(value) == str:
        s = string_to_bitmask(value,bit_kind_size=self.bit_kind_size)[0]
        if s[0] == '0':
          self._data_int[i] = int(s,2)
        else:
          s = s.replace('0','.').replace('1','0').replace('.','1')
          self._data_int[i] = -int(s,2)-1

  def __repr__(self):
    result = []
    for i in self._data_int:
      result += int_to_bitmask(i,bit_kind_size=self.bit_kind_size)
    return str(result)



if __name__ == '__main__':
  import doctest
  doctest.testmod()
