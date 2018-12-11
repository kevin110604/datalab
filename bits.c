/*
 * Modified CS:APP Data Lab
 *
 * bits.c - Source file with your solutions to the Lab.
 *          This is the file you will hand in to your instructor.
 */

/* Read the following instructions carefully.

You will provide your solution to the Data Lab by
editing the collection of functions in this source file.

INTEGER CODING RULES:

  Replace the "return" statement in each function with one
  or more lines of C code that implements the function. Your code
  must conform to the following style:

  int Funct(arg1, arg2, ...) {
      int var1 = Expr1;
      ...
      int varM = ExprM;

      varJ = ExprJ;
      ...
      varN = ExprN;
      return ExprR;
  }

  Each "Expr" is an expression using ONLY the following:
  1. Integer constants 0 through 255 (0xFF), inclusive. You are
      not allowed to use big constants such as 0xffffffff.
  2. Function arguments and local variables (no global variables).
  3. Unary integer operations ! ~
  4. Binary integer operations & ^ | + << >>

  Some of the problems restrict the set of allowed operators even further.
  Each "Expr" may consist of multiple operators. You are not restricted to
  one operator per line.

  You are expressly forbidden to:
  1. Use any control constructs such as if, do, while, for, switch, etc.
  2. Define or use any macros.
  3. Define any additional functions in this file.
  4. Call any functions.
  5. Use any other operations, such as &&, ||, -, or ?:
  6. Use any form of casting.
  7. Use any data type other than int.  This implies that you
     cannot use arrays, structs, or unions.

  You may assume that your machine:
  1. Uses 2s complement, 32-bit representations of integers.
  2. Performs right shifts arithmetically.
  3. Has unpredictable behavior when shifting if the shift amount
     is less than 0 or greater than 31.

EXAMPLES OF ACCEPTABLE CODING STYLE:
  // pow2plus1 - returns 2^x + 1, where 0 <= x <= 31
  int pow2plus1(int x) {
      // exploit ability of shifts to compute powers of 2
      return (1 << x) + 1;
  }

  // pow2plus4 - returns 2^x + 4, where 0 <= x <= 31
  int pow2plus4(int x) {
      // exploit ability of shifts to compute powers of 2
      int result = (1 << x);
      result += 4;
      return result;
  }

FLOATING POINT CODING RULES

For the problems that require you to implement floating-point operations,
the coding rules are less strict.  You are allowed to use looping and
conditional control.  You are allowed to use both ints and unsigneds.
You can use arbitrary integer and unsigned constants. You can use any
arithmetic,
logical, or comparison operations on int or unsigned data.

You are expressly forbidden to:
  1. Define or use any macros.
  2. Define any additional functions in this file.
  3. Call any functions.
  4. Use any form of casting.
  5. Use any data type other than int or unsigned.  This means that you
     cannot use arrays, structs, or unions.
  6. Use any floating point data types, operations, or constants.


NOTES:
  1. Each function has a maximum number of operations (integer, logical,
     or comparison) that you are allowed to use for your implementation
     of the function.
     Note that assignment ('=') is not counted; you may use as many of
     these as you want without penalty.
  2. Use the btest test harness to check your functions for correctness.
  3. The maximum number of ops for each function is given in the
     header comment for each function. If there are any inconsistencies
     between the maximum ops in the writeup and in this file, consider
     this file the authoritative source.
 */

/*
 * absVal - absolute value of x
 *   Example: absVal(-1) = 1.
 *   You may assume -TMax <= x <= TMax
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 10
 *   Rating: 4
 */
int absVal(int x)
{
    int x_sign = x >> 31;
    return (x ^ x_sign) + (~x_sign + 1);
}

/*
 * addOK - Determine if can compute x+y without overflow
 *   Example: addOK(0x80000000, 0x80000000) = 0,
 *            addOK(0x80000000, 0x70000000) = 1,
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 20
 *   Rating: 3
 */
int addOK(int x, int y)
{
    int x_sign = x >> 31;
    int y_sign = y >> 31;
    int res_sign = (x + y) >> 31;
    return !!(x_sign ^ y_sign) | !(res_sign ^ x_sign);
}

/*
 * allEvenBits - return 1 if all even-numbered bits in word set to 1
 *   where bits are numbered from 0 (least significant) to 31 (most significant)
 *   Examples allEvenBits(0xFFFFFFFE) = 0, allEvenBits(0x55555555) = 1
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 12
 *   Rating: 2
 */
int allEvenBits(int x)
{
    int res = 0x55;
    res = (res << 8) + res;
    res = (res << 16) + res;
    return !((x & res) ^ res);
}

/*
 * allOddBits - return 1 if all odd-numbered bits in word set to 1
 *   where bits are numbered from 0 (least significant) to 31 (most significant)
 *   Examples allOddBits(0xFFFFFFFD) = 0, allOddBits(0xAAAAAAAA) = 1
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 12
 *   Rating: 2
 */
int allOddBits(int x)
{
    int res = 0xaa;
    res = (res << 8) + res;
    // res = (res << 16) + res;
    res = (res << 15) + (res >> 1);
    res = res << 1;
    return !((x & res) ^ res);
}

/*
 * anyEvenBit - return 1 if any even-numbered bit in word set to 1
 *   where bits are numbered from 0 (least significant) to 31 (most significant)
 *   Examples anyEvenBit(0xA) = 0, anyEvenBit(0xE) = 1
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 12
 *   Rating: 2
 */
int anyEvenBit(int x)
{
    int res = 0x55;
    res = (res << 8) + res;
    res = (res << 16) + res;
    return !!(x & res);
}

/*
 * anyOddBit - return 1 if any odd-numbered bit in word set to 1
 *   where bits are numbered from 0 (least significant) to 31 (most significant)
 *   Examples anyOddBit(0x5) = 0, anyOddBit(0x7) = 1
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 12
 *   Rating: 2
 */
int anyOddBit(int x)
{
    int res = 0xaa;
    res = (res << 8) + res;
    // res = (res << 16) + res;
    res = (res << 15) + (res >> 1);
    res = res << 1;
    return !!(x & res);
}

/*
 * bang - Compute !x without using !
 *   Examples: bang(3) = 0, bang(0) = 1
 *   Legal ops: ~ & ^ | + << >>
 *   Max ops: 12
 *   Rating: 4
 */
int bang(int x)
{
    int special = x >> 30;
    int neg_x = ~x + 1;
    int neg_x_xor_x = neg_x ^ x;
    special >>= 1;
    neg_x_xor_x >>= 30;
    neg_x_xor_x >>= 1;
    special = ~special + 1;
    return ((~neg_x_xor_x + 1) | special) ^ 1;
}

/*
 * bitAnd - x&y using only ~ and |
 *   Example: bitAnd(6, 5) = 4
 *   Legal ops: ~ |
 *   Max ops: 8
 *   Rating: 1
 */
int bitAnd(int x, int y)
{
    return ~(~x | ~y);
}

/*
 * bitCount - returns count of number of 1's in word
 *   Examples: bitCount(5) = 2, bitCount(7) = 3
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 40
 *   Rating: 4
 */
int bitCount(int x)
{
    /*** too many ops ***/
    int count = 0;
    count += x & 0x01;
    x >>= 1;
    count += x & 0x01;
    x >>= 1;
    count += x & 0x01;
    x >>= 1;
    count += x & 0x01;
    x >>= 1;
    count += x & 0x01;
    x >>= 1;
    count += x & 0x01;
    x >>= 1;
    count += x & 0x01;
    x >>= 1;
    count += x & 0x01;
    x >>= 1;
    count += x & 0x01;
    x >>= 1;
    count += x & 0x01;
    x >>= 1;
    count += x & 0x01;
    x >>= 1;
    count += x & 0x01;
    x >>= 1;
    count += x & 0x01;
    x >>= 1;
    count += x & 0x01;
    x >>= 1;
    count += x & 0x01;
    x >>= 1;
    count += x & 0x01;
    x >>= 1;
    count += x & 0x01;
    x >>= 1;
    count += x & 0x01;
    x >>= 1;
    count += x & 0x01;
    x >>= 1;
    count += x & 0x01;
    x >>= 1;
    count += x & 0x01;
    x >>= 1;
    count += x & 0x01;
    x >>= 1;
    count += x & 0x01;
    x >>= 1;
    count += x & 0x01;
    x >>= 1;
    count += x & 0x01;
    x >>= 1;
    count += x & 0x01;
    x >>= 1;
    count += x & 0x01;
    x >>= 1;
    count += x & 0x01;
    x >>= 1;
    count += x & 0x01;
    x >>= 1;
    count += x & 0x01;
    x >>= 1;
    count += x & 0x01;
    x >>= 1;
    count += x & 0x01;
    return count;
}

/*
 * bitMask - Generate a mask consisting of all 1's
 *   lowbit and highbit
 *   Examples: bitMask(5, 3) = 0x38
 *   Assume 0 <= lowbit <= 31, and 0 <= highbit <= 31
 *   If lowbit > highbit, then mask should be all 0's
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 16
 *   Rating: 3
 */
int bitMask(int highbit, int lowbit)
{
    /*** too many ops ***/
    int neg_lowbit = ~lowbit + 1;
    int large = !((highbit + neg_lowbit) >> 31);
    int small = !large;
    int n = highbit + neg_lowbit;
    int res = ~0;
    res <<= n;
    res <<= 1;
    res = ~res;
    res <<= lowbit;
    return ((~large + 1) & res) + ((~small + 1) & 0);
}

/*
 * bitMatch - Create mask indicating which bits in x match those in y
 *            using only ~ and &
 *   Example: bitMatch(0x7, 0xE) = 0x6
 *   Legal ops: ~ &
 *   Max ops: 14
 *   Rating: 1
 */
int bitMatch(int x, int y)
{
    return (~(x & ~y) & ~(~x & y)); /*** NXor ***/
}

/*
 * bitNor - ~(x|y) using only ~ and &
 *   Example: bitNor(0x6, 0x5) = 0xFFFFFFF8
 *   Legal ops: ~ &
 *   Max ops: 8
 *   Rating: 1
 */
int bitNor(int x, int y)
{
    return (~x & ~y);
}

/*
 * bitOr - x|y using only ~ and &
 *   Example: bitOr(6, 5) = 7
 *   Legal ops: ~ &
 *   Max ops: 8
 *   Rating: 1
 */
int bitOr(int x, int y)
{
    return ~(~x & ~y);
}

/*
 * bitParity - returns 1 if x contains an odd number of 0's
 *   Examples: bitParity(5) = 0, bitParity(7) = 1
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 20
 *   Rating: 4
 */
int bitParity(int x)
{
    x ^= x >> 16;
    x ^= x >> 8;
    x ^= x >> 4;
    x ^= x >> 2;
    x ^= x >> 1;
    return x & 1;
}

/*
 * bitReverse - Reverse bits in a 32-bit word
 *   Examples: bitReverse(0x80000002) = 0x40000001
 *             bitReverse(0x89ABCDEF) = 0xF7D3D591
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 45
 *   Rating: 4
 */
int bitReverse(int x)
{
    int mask16 = (1 << 16) + (~0);
    int mask8 = (0xff << 16) + 0xff;
    int mask4 = (0x0f << 8) + 0x0f;
    int mask2 = (0x33 << 8) + 0x33;
    int mask1 = (0x55 << 8) + 0x55;
    int tmp;

    mask4 += (mask4 << 16);
    mask2 += (mask2 << 16);
    mask1 += (mask1 << 16);

    tmp = (x >> 16) & mask16;
    x <<= 16;
    x += tmp;

    tmp = (x >> 8) & mask8;  // org high bit
    x &= mask8;              // org low bit
    x <<= 8;                 // move low to high
    x += tmp;

    tmp = (x >> 4) & mask4;
    x &= mask4;
    x <<= 4;
    x += tmp;

    tmp = (x >> 2) & mask2;
    x &= mask2;
    x <<= 2;
    x += tmp;

    tmp = (x >> 1) & mask1;
    x &= mask1;
    x <<= 1;
    x += tmp;

    return x;
}

/*
 * bitXor - x^y using only ~ and &
 *   Example: bitXor(4, 5) = 1
 *   Legal ops: ~ &
 *   Max ops: 14
 *   Rating: 1
 */
int bitXor(int x, int y)
{
    return ~(~(x & ~y) & ~(~x & y));
}

/*
 * byteSwap - swaps the nth byte and the mth byte
 *  Examples: byteSwap(0x12345678, 1, 3) = 0x56341278
 *            byteSwap(0xDEADBEEF, 0, 2) = 0xDEEFBEAD
 *  You may assume that 0 <= n <= 3, 0 <= m <= 3
 *  Legal ops: ! ~ & ^ | + << >>
 *  Max ops: 25
 *  Rating: 2
 */
int byteSwap(int x, int n, int m)
{
    /*
    int byte_0 = x & 0x000000ff;
    int byte_1 = (x >> 8) & 0x000000ff;
    int byte_2 = (x >> 16) & 0x000000ff;
    int byte_3 = x >> 24;
    int d0 = (n ^ 0) | (m ^ 0);
    int d1 = (n ^ 1) | (m ^ 1);
    int d2 = (n ^ 2) | (m ^ 2);
    int d3 = (n ^ 3) | (m ^ 3);
    int c0 = 0;
    int c1 = 8;
    int c2 = 16;
    int c3 = 24;

    int res = (byte_3 << c3) + (byte_2 << c2) + (byte_1 << c1) + (byte_0 << c0);
    */
    int equal = !(n ^ m);
    int not_equal = !equal;
    int n_mul_8 = n << 3;
    int m_mul_8 = m << 3;
    int nth_byte = (x >> n_mul_8) & 0xff;
    int mth_byte = (x >> m_mul_8) & 0xff;
    int A, B, C;
    int res;
    nth_byte <<= m_mul_8;
    mth_byte <<= n_mul_8;
    A = 0xff << m_mul_8;
    B = 0xff << n_mul_8;
    C = ~(A + B) & x;
    res = C + nth_byte + mth_byte;
    return ((~not_equal + 1) & res) + ((~equal + 1) & x);
}

/*
 * conditional - same as x ? y : z
 *   Example: conditional(2,4,5) = 4
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 16
 *   Rating: 3
 */
int conditional(int x, int y, int z)
{
    int choose_y = !!x;
    int choose_z = !x;
    return ((~choose_y + 1) & y) + ((~choose_z + 1) & z);
}

/*
 * countLeadingZero - count the number of zero bits preceding the
 *                    most significant one bit
 *   Example: countLeadingZero(0x00000F00) = 20,
 *            countLeadingZero(0x00000001) = 31
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 50
 *   Rating: 4
 */
int countLeadingZero(int x)
{
    int q, move;
    int count = 0;
    int check;
    int y = x;

    q = y >> 16;
    check = !q;
    check = (~check + 1);
    move = 16 & check;
    y <<= move;
    count += move;

    q = y >> 24;
    check = !q;
    check = (~check + 1);
    move = 8 & check;
    y <<= move;
    count += move;

    q = y >> 28;
    check = !q;
    check = (~check + 1);
    move = 4 & check;
    y <<= move;
    count += move;

    q = y >> 30;
    check = !q;
    check = (~check + 1);
    move = 2 & check;
    y <<= move;
    count += move;

    q = y >> 31;
    check = !q;
    check = (~check + 1);
    move = 1 & check;
    y <<= move;
    count += move;

    q = y >> 31;
    check = !q;
    check = (~check + 1);
    move = 1 & check;
    count += move;

    return count;
}

/*
 * copyLSB - set all bits of result to least significant bit of x
 *   Example: copyLSB(5) = 0xFFFFFFFF, copyLSB(6) = 0x00000000
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 5
 *   Rating: 2
 */
int copyLSB(int x)
{
    int lsb = x & 1;
    return (lsb ^ ~0) + 1;
}

/*
 * distinctNegation - returns 1 if x != -x.
 *     and 0 otherwise
 *   Legal ops: ! ~ & ^ | +
 *   Max ops: 5
 *   Rating: 2
 */
int distinctNegation(int x)
{
    int neg = ~x + 1;
    return !!(x ^ neg);
}

/*
 * dividePower2 - Compute x/(2^n), for 0 <= n <= 30
 *                Round toward zero
 *   Examples: dividePower2(15, 1) = 7, dividePower2(-33, 4) = -2
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 15
 *   Rating: 2
 */
int dividePower2(int x, int n)
{
    /* ver. 1 */
    /*
    int shift = x >> n;
    int shift_plus = (x + (1 << n) + (~0)) >> n;
    int pos;
    int neg;
    int n_zero = !!n;
    int res;
    int tmp = 1 << 31;
    neg = (!((x & tmp) ^ tmp)) & (~n_zero + 1);
    pos = !neg;
    res = ((~pos + 1) & shift) + ((~neg + 1) & shift_plus);
    return res;
    */
    /* ver. 2 */
    /*
    int neg;
    int n_zero = !!n;
    int res;
    int tmp = 1 << 31;
    neg = (!((x & tmp) ^ tmp)) & (~n_zero + 1);
    res = (x + (((1 << n) + (~0)) & (~neg + 1))) >> n;
    return res;
    */
    int sign = x >> 31;
    int rounding = ((1 << n) + (~0)) & sign;
    int res = (x + rounding) >> n;
    return res;
}

/*
 * evenBits - return word with all even-numbered bits set to 1
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 8
 *   Rating: 1
 */
int evenBits(void)
{
    int res = 0x55;
    res = (res << 8) + res;
    res = (res << 16) + res;
    return res;
}

/*
 * ezThreeFourths - multiplies by 3/4 rounding toward 0,
 *                  Should exactly duplicate effect of C expression (x*3/4),
 *                  including overflow behavior.
 *   Examples: ezThreeFourths(11) = 8
 *             ezThreeFourths(-9) = -6
 *             ezThreeFourths(1073741824) = -268435456 (overflow)
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 12
 *   Rating: 3
 */
int ezThreeFourths(int x)
{
    int x_mul_4 = x << 2;
    int x_mul_3 = x_mul_4 + (~x + 1);
    int sign = x_mul_3 >> 31;
    int rounding = ((1 << 2) + (~0)) & sign;
    int res = (x_mul_3 + rounding) >> 2;
    return res;
}

/*
 * fitsBits - return 1 if x can be represented as an n-bit, two's complement
 *            integer.
 *            1 <= n <= 32
 *   Examples: fitsBits(5,3) = 0, fitsBits(-4,3) = 1
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 15
 *   Rating: 2
 */
int fitsBits(int x, int n)
{
    /*
    int m = 32 + (~n + 1);
    int one_or_zero = !!m;
    int n_bit_var = x << one_or_zero;
    n_bit_var <<= m + (~one_or_zero + 1);
    n_bit_var >>= one_or_zero;
    n_bit_var >>= m + (~one_or_zero + 1);
    */
    int m = 32 + (~n + 1);
    int n_bit_var = x << m;
    n_bit_var >>= m;
    return !(x ^ n_bit_var);
}

/*
 * fitsShort - return 1 if x can be represented as a 16-bit, two's complement
 *             integer.
 *   Examples: fitsShort(33000) = 0, fitsShort(-32768) = 1
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 8
 *   Rating: 1
 */
int fitsShort(int x)
{
    int short_var = x << 16;
    short_var >>= 16;
    return !(x ^ short_var);
}

/*
 * floatAbsVal - Return bit-level equivalent of absolute value of f for
 *               floating point argument f.
 *               Both the argument and result are passed as unsigned int's,
 *               but they are to be interpreted as the bit-level
 *               representations of single-precision floating point values.
 *               When argument is NaN, return argument.
 *   Legal ops: Any integer/unsigned operations incl. ||, &&. also if, while
 *   Max ops: 10
 *   Rating: 2
 */
unsigned floatAbsVal(unsigned uf)
{
    if ((uf & 0x7f800000u) == 0x7f800000u) /* NaN, exp is 255 */
        if ((uf & 0x007fffffu) != 0)       /* fraction is nonzero */
            return uf;
    unsigned miss_sign = uf & 0x7fffffffu;
    return miss_sign;
}

/*
 * floatFloat2Int - Return bit-level equivalent of expression (int) f
 *                  for floating point argument f.
 *                  Argument is passed as unsigned int, but it is to be
 *                  interpreted as the bit-level representation of a
 *                  single-precision floating point value.
 *                  Anything out of range (including NaN and infinity) should
 *                  return 0x80000000u.
 *   Legal ops: Any integer/unsigned operations incl. ||, &&. also if, while
 *   Max ops: 30
 *   Rating: 4
 */
int floatFloat2Int(unsigned uf)
{
    unsigned sign = uf & 0x80000000u;
    unsigned exponent = uf & 0x7f800000u;
    unsigned fraction = uf & 0x007fffffu;
    int power = (exponent >> 23) - 127;
    int num = fraction + 0x00800000;
    int res;

    if (exponent == 0x7f800000u) /* NaN, exp is 255 */
        if (fraction != 0)       /* fraction is nonzero */
            return 0x80000000u;
    if (uf == 0x7f800000u || uf == 0xff800000u) /* +-infinity */
        return 0x80000000u;
    if (exponent == 0u) /* denormalized number & +-0*/
        return 0;
    if (power < 0)
        return 0;
    if (power > 23) {
        res = num << (power - 23);
        if (sign) {
            if (power >= 29)
                return 0x80000000;
            return ~res + 1;
        }
        if (power >= 29)
            return 0x80000000;
        return res;
    }
    res = num >> (23 - power);
    if (sign)
        return ~res + 1;
    return res;
}

/*
 * floatInt2Float - Return bit-level equivalent of expression (float) x
 *                  Result is returned as unsigned int, but it is to be
 *                  interpreted as the bit-level representation of a
 *                  single-precision floating point values.
 *   Legal ops: Any integer/unsigned operations incl. ||, &&. also if, while
 *   Max ops: 30
 *   Rating: 4
 */
unsigned floatInt2Float(int x)
{
    /*** too many ops ***/
    int abs_x = x;
    int i;
    int power = 0;
    int flag = 0, count = 0;
    unsigned sign = x & 0x80000000;
    unsigned exponent;
    unsigned fraction = 0u;
    int G = 0, R = 0, S = 0;

    if (x == 0)
        return 0u;
    if (x == 0x80000000)
        return 0xcf000000u;

    if (x < 0)
        abs_x = -x;

    for (i = 1; i < 32; i++) {
        if (flag) {
            fraction <<= 1;
            fraction += (abs_x >> (31 - i)) & 0x1u;
            count++;
            if (i == 31) {
                fraction <<= (23 - count);
                if (count == 23)
                    G = abs_x & 0x1;
            } else if (count == 23) {
                G = (abs_x >> (31 - i)) & 0x1;
                R = (abs_x >> (31 - i - 1)) & 0x1;
                if (i == 30)
                    break;
                S = abs_x << (i + 2);
                S = !!S;
                break;
            }
        }
        if ((!flag) && ((abs_x << i) & 0x80000000)) {
            power = 31 - i;
            flag = 1;
        }
    }

    if (G && R && !S)
        fraction += 1;
    if (!G && R && S)
        fraction += 1;
    if (G && R && S)
        fraction += 1;

    power += 127;
    exponent = power & 0x000000ff;
    exponent <<= 23;

    return sign + exponent + fraction;
}

/*
 * floatIsEqual - Compute f == g for floating point arguments f and g.
 *                Both the arguments are passed as unsigned int's, but
 *                they are to be interpreted as the bit-level representations
 *                of single-precision floating point values.
 *                If either argument is NaN, return 0.
 *                +0 and -0 are considered equal.
 *   Legal ops: Any integer/unsigned operations incl. ||, &&. also if, while
 *   Max ops: 25
 *   Rating: 2
 */
int floatIsEqual(unsigned uf, unsigned ug)
{
    if ((uf & 0x7f800000u) == 0x7f800000u) /* NaN, exp is 255 */
        if ((uf & 0x007fffffu) != 0)       /* fraction is nonzero */
            return 0;
    if ((ug & 0x7f800000u) == 0x7f800000u) /* NaN, exp is 255 */
        if ((ug & 0x007fffffu) != 0)       /* fraction is nonzero */
            return 0;
    if ((uf == 0x00000000u) || (uf == 0x80000000u))
        if ((ug == 0x00000000u) || (ug == 0x80000000u))
            return 1;
    return !(uf ^ ug);
}

/*
 * floatIsLess - Compute f < g for floating point arguments f and g.
 *               Both the arguments are passed as unsigned int's, but
 *               they are to be interpreted as the bit-level representations
 *               of single-precision floating point values.
 *               If either argument is NaN, return 0.
 *               +0 and -0 are considered equal.
 *   Legal ops: Any integer/unsigned operations incl. ||, &&. also if, while
 *   Max ops: 30
 *   Rating: 3
 */
int floatIsLess(unsigned uf, unsigned ug)
{
    unsigned f_sign = uf & 0x80000000u;
    unsigned f_exponent = uf & 0x7f800000u;
    unsigned f_fraction = uf & 0x007fffffu;
    unsigned g_sign = ug & 0x80000000u;
    unsigned g_exponent = ug & 0x7f800000u;
    unsigned g_fraction = ug & 0x007fffffu;

    if (f_exponent == 0x7f800000u) /* NaN, exp is 255 */
        if (f_fraction != 0)       /* fraction is nonzero */
            return 0;
    if (g_exponent == 0x7f800000u) /* NaN, exp is 255 */
        if (g_fraction != 0)       /* fraction is nonzero */
            return 0;
    if ((uf == 0x00000000u) || (uf == 0x80000000u))
        if ((ug == 0x00000000u) || (ug == 0x80000000u))
            return 0;
    if (f_sign > g_sign) /* f<0, g>=0 */
        return 1;
    if (f_sign < g_sign) /* f>=0, g<0 */
        return 0;
    if (f_exponent > g_exponent) {
        if (f_sign)
            return 1;
        return 0;
    }
    if (f_exponent < g_exponent) {
        if (f_sign)
            return 0;
        return 1;
    }
    if (f_fraction > g_fraction) {
        if (f_sign)
            return 1;
        return 0;
    }
    if (f_fraction < g_fraction) {
        if (f_sign)
            return 0;
        return 1;
    }
    return 0; /* equal case */
}

/*
 * floatNegate - Return bit-level equivalent of expression -f for
 *               floating point argument f.
 *               Both the argument and result are passed as unsigned int's,
 *               but they are to be interpreted as the bit-level
 *               representations of single-precision floating point values.
 *               When argument is NaN, return argument.
 *   Legal ops: Any integer/unsigned operations incl. ||, &&. also if, while
 *   Max ops: 10
 *   Rating: 2
 */
unsigned floatNegate(unsigned uf)
{
    if ((uf & 0x7f800000u) == 0x7f800000u) /* NaN, exp is 255 */
        if ((uf & 0x007fffffu) != 0)       /* fraction is nonzero */
            return uf;
    uf = uf ^ 0x80000000u;
    return uf;
}

/*
 * floatPower2 - Return bit-level equivalent of the expression 2.0^x
 *               (2.0 raised to the power x) for any 32-bit integer x.
 *
 *               The unsigned value that is returned should have the
 *               identical bit representation as the single-precision
 *               floating-point number 2.0^x.
 *               If the result is too small to be represented as a denorm,
 *               return 0. If too large, return +INF.
 *
 *   Legal ops: Any integer/unsigned operations incl. ||, &&. Also if, while
 *   Max ops: 30
 *   Rating: 4
 */
unsigned floatPower2(int x)
{
    int x_plus_bias = x + 127;
    unsigned exponent = x_plus_bias & 0x000000ffu;
    unsigned res = exponent << 23;
    if (x_plus_bias < 0 || x_plus_bias > 255) {
        if (x_plus_bias > 255)
            return 0x7f800000;
        else
            return 0;
    }
    return res;
}

/*
 * floatScale1d2 - Return bit-level equivalent of expression 0.5*f for
 *                 floating point argument f.
 *                 Both the argument and result are passed as unsigned int's,
 *                 but they are to be interpreted as the bit-level
 *                 representation of single-precision floating point values.
 *                 When argument is NaN, return argument
 *   Legal ops: Any integer/unsigned operations incl. ||, &&. also if, while
 *   Max ops: 30
 *   Rating: 4
 */
unsigned floatScale1d2(unsigned uf)
{
    unsigned sign = uf & 0x80000000u;
    unsigned exponent = uf & 0x7f800000u;
    unsigned fraction = uf & 0x007fffffu;
    unsigned fraction_rshift1 = fraction >> 1;
    unsigned back = !(exponent - 0x00800000u);

    if ((uf & 0x7f800000u) == 0x7f800000u) /* NaN, exp is 255 */
        if ((uf & 0x007fffffu) != 0)       /* fraction is nonzero */
            return uf;
    if (uf == 0u || uf == 0x80000000u) /* +-0 */
        return uf;
    if (uf == 0x7f800000u || uf == 0xff800000u) /* +-infinity */
        return uf;

    if (exponent == 0u || back) {                    /* denormalized number */
        if ((fraction & 0x00000003u) == 0x00000003u) /* 4n + 3 */
            return sign + fraction_rshift1 + back * 0x00400000u + 1u;
        return sign + fraction_rshift1 + back * 0x00400000u;
    }
    return uf - 0x00800000u;
}

/*
 * floatScale2 - Return bit-level equivalent of expression 2*f for
 *               floating point argument f.
 *               Both the argument and result are passed as unsigned int's,
 *               but they are to be interpreted as the bit-level representation
 *               of single-precision floating point values.
 *               When argument is NaN, return argument
 *   Legal ops: Any integer/unsigned operations incl. ||, &&. also if, while
 *   Max ops: 30
 *   Rating: 4
 */
unsigned floatScale2(unsigned uf)
{
    unsigned sign = uf & 0x80000000u;
    unsigned exponent = uf & 0x7f800000u;
    unsigned fraction = uf & 0x007fffffu;

    if ((uf & 0x7f800000u) == 0x7f800000u) /* NaN, exp is 255 */
        if ((uf & 0x007fffffu) != 0)       /* fraction is nonzero */
            return uf;
    if (uf == 0u || uf == 0x80000000u) /* +-0 */
        return uf;
    if (uf == 0x7f800000u || uf == 0xff800000u) /* +-infinity */
        return uf;
    if (exponent == 0u) { /* denormalized number */
        fraction <<= 1;
        return sign + fraction;
    }
    return uf + 0x00800000u;
}

/*
 * floatScale64 - Return bit-level equivalent of expression 64*f for
 *                floating point argument f.
 *                Both the argument and result are passed as unsigned int's,
 *                but they are to be interpreted as the bit-level
 *                representation of single-precision floating point values.
 *                When argument is NaN, return argument
 *   Legal ops: Any integer/unsigned operations incl. ||, &&. also if, while
 *   Max ops: 35
 *   Rating: 4
 */
unsigned floatScale64(unsigned uf)
{
    unsigned sign = uf & 0x80000000u;
    unsigned exponent = uf & 0x7f800000u;
    unsigned fraction = uf & 0x007fffffu;
    unsigned exp_6msb = uf & 0x007e0000u;
    unsigned frac_msb;
    int i;

    if ((uf & 0x7f800000u) == 0x7f800000u) /* NaN, exp is 255 */
        if ((uf & 0x007fffffu) != 0)       /* fraction is nonzero */
            return uf;
    if (uf == 0u || uf == 0x80000000u) /* +-0 */
        return uf;
    if (uf == 0x7f800000u || uf == 0xff800000u) /* +-infinity */
        return uf;
    if (exponent == 0u) { /* denormalized number */
        if (exp_6msb) {
            for (i = 1; i <= 6; i++) {
                frac_msb = fraction & 0x00400000u;
                fraction <<= 1;
                if (frac_msb)
                    break;
            }
            return sign + ((6 - i) << 23) + fraction;
        }
        fraction <<= 6;
        return sign + fraction;
    }
    if (exponent >= 0x7d000000u) {
        if (sign)
            return 0xff800000u;
        return 0x7f800000u;
    }
    return uf + 0x03000000u;
}

/*
 * floatUnsigned2Float - Return bit-level equivalent of expression (float) u
 *                       Result is returned as unsigned int, but it is to be
 *                       interpreted as the bit-level representation of a
 *                       single-precision floating point values.
 *   Legal ops: Any integer/unsigned operations incl. ||, &&. also if, while
 *   Max ops: 30
 *   Rating: 4
 */
unsigned floatUnsigned2Float(unsigned u)
{
    /*** too many ops ***/
    int i;
    int power = 0;
    int flag = 0, count = 0;
    unsigned exponent;
    unsigned fraction = 0u;
    int G = 0, R = 0, S = 0;

    if (u == 0u)
        return 0u;

    for (i = 0; i < 32; i++) {
        if (flag) {
            fraction <<= 1;
            fraction += (u >> (31 - i)) & 0x1u;
            count++;
            if (i == 31) {
                fraction <<= (23 - count);
                if (count == 23)
                    G = u & 0x1;
            } else if (count == 23) {
                G = (u >> (31 - i)) & 0x1;
                R = (u >> (31 - i - 1)) & 0x1;
                if (i == 30)
                    break;
                S = u << (i + 2);
                S = !!S;
                break;
            }
        }
        if ((!flag) && ((u << i) & 0x80000000)) {
            power = 31 - i;
            flag = 1;
        }
    }

    if (G && R && !S)
        fraction += 1;
    if (!G && R && S)
        fraction += 1;
    if (G && R && S)
        fraction += 1;

    power += 127;
    exponent = power & 0x000000ff;
    exponent <<= 23;

    return exponent + fraction;
}

/*
 * getByte - Extract byte n from word x
 *           Bytes numbered from 0 (least significant) to 3 (most significant)
 *   Examples: getByte(0x12345678,1) = 0x56
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 6
 *   Rating: 2
 */
int getByte(int x, int n)
{
    int n_mul_8_bits = n << 3;
    return (x >> n_mul_8_bits) & 0x000000ff;
}

/*
 * greatestBitPos - return a mask that marks the position of the
 *                  most significant 1 bit. If x == 0, return 0
 *   Example: greatestBitPos(96) = 0x40
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 70
 *   Rating: 4
 */
int greatestBitPos(int x)
{
    int zero = !!x;
    int Tmax = 1 << 31;
    Tmax += ~0;
    x |= x >> 16;
    x |= x >> 8;
    x |= x >> 4;
    x |= x >> 2;
    x |= x >> 1;
    x >>= 1;
    x &= Tmax;
    return (x + 1) & (~zero + 1);
}

/* howManyBits - return the minimum number of bits required to represent x in
 *               two's complement
 *  Examples: howManyBits(12) = 5
 *            howManyBits(298) = 10
 *            howManyBits(-5) = 4
 *            howManyBits(0)  = 1
 *            howManyBits(-1) = 1
 *            howManyBits(0x80000000) = 32
 *  Legal ops: ! ~ & ^ | + << >>
 *  Max ops: 90
 *  Rating: 4
 */
int howManyBits(int x)
{
    int sign = x >> 31;
    int q, move;
    int count = 0;
    int check;
    int y = x ^ sign;

    q = y >> 16;
    check = !q;
    check = (~check + 1);
    move = 16 & check;
    y <<= move;
    count += move;

    q = y >> 24;
    check = !q;
    check = (~check + 1);
    move = 8 & check;
    y <<= move;
    count += move;

    q = y >> 28;
    check = !q;
    check = (~check + 1);
    move = 4 & check;
    y <<= move;
    count += move;

    q = y >> 30;
    check = !q;
    check = (~check + 1);
    move = 2 & check;
    y <<= move;
    count += move;

    q = y >> 31;
    check = !q;
    check = (~check + 1);
    move = 1 & check;
    y <<= move;
    count += move;

    q = y >> 31;
    check = !q;
    check = (~check + 1);
    move = 1 & check;
    count += move;

    return 34 + (~count);
}

/*
 * implication - return x -> y in propositional logic - 0 for false,
 *               1 for true
 *   Example: implication(1, 1) = 1
 *            implication(1, 0) = 0
 *   Legal ops: ! ~ ^ |
 *   Max ops: 5
 *   Rating: 2
 */
int implication(int x, int y)
{
    // return (x & y) | ((!x) & y) | ((!x) & (!y));
    return !(x & (!y));
}

/*
 * intLog2 - return floor(log base 2 of x), where x > 0
 *   Example: intLog2(16) = 4
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 90
 *   Rating: 4
 */
int intLog2(int x)
{
    int q, move;
    int count = 0;
    int check;

    int y; /* greatestBitPos */
    int zero = !!x;
    int Tmax = 1 << 31;
    Tmax += ~0;
    x |= x >> 16;
    x |= x >> 8;
    x |= x >> 4;
    x |= x >> 2;
    x |= x >> 1;
    x >>= 1;
    x &= Tmax;
    y = (x + 1) & (~zero + 1);

    q = (y >> 16) | 0;
    check = !q;
    check = (~check + 1);
    move = 16 & check;
    y <<= move;
    count += move;

    q = (y >> 24) | 0;
    check = !q;
    check = (~check + 1);
    move = 8 & check;
    y <<= move;
    count += move;

    q = (y >> 28) | 0;
    check = !q;
    check = (~check + 1);
    move = 4 & check;
    y <<= move;
    count += move;

    q = (y >> 30) | 0;
    check = !q;
    check = (~check + 1);
    move = 2 & check;
    y <<= move;
    count += move;

    q = (y >> 31) | 0;
    check = !q;
    check = (~check + 1);
    move = 1 & check;
    y <<= move;
    count += move;

    q = (y >> 31) | 0;
    check = !q;
    check = (~check + 1);
    move = 1 & check;
    count += move;

    return 31 + (~count + 1);
}

/*
 * isAsciiDigit - return 1 if 0x30 <= x <= 0x39 (ASCII codes for characters
 *                '0' to '9')
 *   Example: isAsciiDigit(0x35) = 1.
 *            isAsciiDigit(0x3a) = 0.
 *            isAsciiDigit(0x05) = 0.
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 15
 *   Rating: 3
 */
int isAsciiDigit(int x)
{
    int c0 = 0x30;
    int c9 = 0x39;
    int diff0 = x + (~c0 + 1);
    int diff9 = c9 + (~x + 1);
    int sign0 = diff0 >> 30;
    int sign9 = diff9 >> 30;
    sign0 >>= 1;
    sign9 >>= 1;
    return !sign0 & !sign9;
}

/*
 * isEqual - return 1 if x == y, and 0 otherwise
 *   Examples: isEqual(5,5) = 1, isEqual(4,5) = 0
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 5
 *   Rating: 2
 */
int isEqual(int x, int y)
{
    return !(x ^ y);
}

/*
 * isGreater - if x > y  then return 1, else return 0
 *   Example: isGreater(4,5) = 0, isGreater(5,4) = 1
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 24
 *   Rating: 3
 */
int isGreater(int x, int y)
{
    /* ver. 1 WRONG */
    /*
    int tmin = 1 << 31;
    int x_sign = x >> 31;
    int y_sign = y >> 31;
    int diff = x + (~y + 1);
    int sign = diff >> 31;

    // sign is positive
    // diff cannot be zero
    return ((!x) & !(y ^ tmin)) |
           ((!sign) & (!!diff) & !((!!x_sign) & (!y_sign)));
    */
    int diff = x + (~y + 1);
    int d_sign = diff >> 31;
    int x_sign = x >> 31;
    int y_sign = y >> 31;
    int sign_xor = x_sign ^ y_sign;
    int res = (d_sign + 1) & (!!diff);
    int yes = ~(sign_xor & y_sign) + 1;
    int no = ~(sign_xor & x_sign) + 1;
    return (yes | res) & (!no);
}

/*
 * isLess - if x < y  then return 1, else return 0
 *   Example: isLess(4,5) = 1.
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 24
 *   Rating: 3
 */
int isLess(int x, int y)
{
    /* ver. 1 WRONG */
    /*
    int diff = x + (~y + 1);
    int sign = diff >> 30;
    sign >>= 1;
    return (!!sign) & (!!diff);
    */
    int diff = x + (~y + 1);
    int d_sign = diff >> 31;
    int x_sign = x >> 31;
    int y_sign = y >> 31;
    int sign_xor = x_sign ^ y_sign;
    int res = d_sign & (!!diff);
    int yes = ~(sign_xor & x_sign) + 1;
    int no = ~(sign_xor & y_sign) + 1;
    return (yes | res) & (!no);
}

/*
 * isLessOrEqual - if x <= y  then return 1, else return 0
 *   Example: isLessOrEqual(4,5) = 1.
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 24
 *   Rating: 3
 */
int isLessOrEqual(int x, int y)
{
    /* ver. 1 WRONG */
    /*
    int diff = x + (~y + 1);
    int sign = diff >> 30;
    sign >>= 1;
    return (!!sign) | (!diff);
    */
    int diff = x + (~y + 1);
    int d_sign = diff >> 31;
    int x_sign = x >> 31;
    int y_sign = y >> 31;
    int sign_xor = x_sign ^ y_sign;
    int res = (~d_sign + 1) | (!diff);
    int yes = ~(sign_xor & x_sign) + 1;
    int no = ~(sign_xor & y_sign) + 1;
    return (yes | res) & (!no);
}

/*
 * isNegative - return 1 if x < 0, return 0 otherwise
 *   Example: isNegative(-1) = 1.
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 6
 *   Rating: 2
 */
int isNegative(int x)
{
    int sign = x >> 30;
    sign >>= 1;
    return !!sign;
}

/*
 * isNonNegative - return 1 if x >= 0, return 0 otherwise
 *   Example: isNonNegative(-1) = 0.  isNonNegative(0) = 1.
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 6
 *   Rating: 2
 */
int isNonNegative(int x)
{
    int sign = x >> 30;
    sign >>= 1;
    return !sign;
}

/*
 * isNonZero - Check whether x is nonzero using
 *              the legal operators except !
 *   Examples: isNonZero(3) = 1, isNonZero(0) = 0
 *   Legal ops: ~ & ^ | + << >>
 *   Max ops: 10
 *   Rating: 4
 */
int isNonZero(int x)
{
    // int neg_x = ~x + 1;
    // return ~(x ^ neg_x) & (!!x);
    int special = x >> 30;
    int neg_x = ~x + 1;
    int neg_x_xor_x = neg_x ^ x;
    special >>= 1;
    neg_x_xor_x >>= 30;
    neg_x_xor_x >>= 1;
    special = ~special + 1;
    return (~neg_x_xor_x + 1) | special;
}

/*
 * isNotEqual - return 0 if x == y, and 1 otherwise
 *   Examples: isNotEqual(5,5) = 0, isNotEqual(4,5) = 1
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 6
 *   Rating: 2
 */
int isNotEqual(int x, int y)
{
    return !!(x ^ y);
}

/*
 * isPallindrome - Return 1 if bit pattern in x is equal to its mirror image
 *   Example: isPallindrome(0x01234567E6AC2480) = 1
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 45
 *   Rating: 4
 */
int isPallindrome(int x)
{
    int y = x;
    int mask16 = (1 << 16) + (~0);
    int mask8 = (0xff << 16) + 0xff;
    int mask4 = (0x0f << 8) + 0x0f;
    int mask2 = (0x33 << 8) + 0x33;
    int mask1 = (0x55 << 8) + 0x55;
    int tmp;

    mask4 += (mask4 << 16);
    mask2 += (mask2 << 16);
    mask1 += (mask1 << 16);

    tmp = (y >> 16) & mask16;
    y <<= 16;
    y += tmp;

    tmp = (y >> 8) & mask8;  // org high bit
    y &= mask8;              // org low bit
    y <<= 8;                 // move low to high
    y += tmp;

    tmp = (y >> 4) & mask4;
    y &= mask4;
    y <<= 4;
    y += tmp;

    tmp = (y >> 2) & mask2;
    y &= mask2;
    y <<= 2;
    y += tmp;

    tmp = (y >> 1) & mask1;
    y &= mask1;
    y <<= 1;
    y += tmp;

    return !(x ^ y);
}

/*
 * isPositive - return 1 if x > 0, return 0 otherwise
 *   Example: isPositive(-1) = 0.
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 8
 *   Rating: 2
 */
int isPositive(int x)
{
    int sign = x >> 30;
    sign >>= 1;
    return !sign & !!x;
}

/*
 * isPower2 - returns 1 if x is a power of 2, and 0 otherwise
 *   Examples: isPower2(5) = 0, isPower2(8) = 1, isPower2(0) = 0
 *   Note that no negative number is a power of 2.
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 20
 *   Rating: 4
 */
int isPower2(int x)
{
    /* ver. 1 */
    /*
    int yes_or_no = 1;
    int minus_one = ~0;
    int max = 1 << 30;
    max <<= 1;
    max += minus_one;
    yes_or_no = !!((x | max) ^ minus_one);
    */
    int y = x;
    y |= y >> 16;
    y |= y >> 8;
    y |= y >> 4;
    y |= y >> 2;
    y |= y >> 1;
    y += 1;
    y >>= 1;
    return (!(x ^ y)) & (!!x);
}

/*
 * isTmax - returns 1 if x is the maximum, two's complement number,
 *     and 0 otherwise
 *   Legal ops: ! ~ & ^ | +
 *   Max ops: 10
 *   Rating: 1
 */
int isTmax(int x)
{
    int max = 0x7f;
    max = (max << 8) + 0xff;
    max = (max << 8) + 0xff;
    max = (max << 8) + 0xff;
    return !(x ^ max);
}

/*
 * isTmin - returns 1 if x is the minimum, two's complement number,
 *     and 0 otherwise
 *   Legal ops: ! ~ & ^ | +
 *   Max ops: 10
 *   Rating: 1
 */
int isTmin(int x)
{
    int min = 1 << 30;
    min <<= 1;
    return !(x ^ min);
}

/*
 * isZero - returns 1 if x == 0, and 0 otherwise
 *   Examples: isZero(5) = 0, isZero(0) = 1
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 2
 *   Rating: 1
 */
int isZero(int x)
{
    return !x;
}

/*
 * leastBitPos - return a mask that marks the position of the
 *               least significant 1 bit. If x == 0, return 0
 *   Example: leastBitPos(96) = 0x20
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 6
 *   Rating: 2
 */
int leastBitPos(int x)
{
    /* ver. 1 */
    /*
    x |= x << 16;
    x |= x << 8;
    x |= x << 4;
    x |= x << 2;
    x |= x << 1;
    return ~x + 1;
    */
    int x_and_x_minus_one = x & (x + ~0);
    return x ^ x_and_x_minus_one;
}

/*
 * leftBitCount - returns count of number of consective 1's in
 *                left-hand (most significant) end of word.
 *   Examples: leftBitCount(-1) = 32, leftBitCount(0xFFF0F0F0) = 12
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 50
 *   Rating: 4
 */
int leftBitCount(int x)
{
    int q, move;
    int count = 0;
    int check;
    int y = ~x;

    q = y >> 16;
    check = !q;
    check = (~check + 1);
    move = 16 & check;
    y <<= move;
    count += move;

    q = y >> 24;
    check = !q;
    check = (~check + 1);
    move = 8 & check;
    y <<= move;
    count += move;

    q = y >> 28;
    check = !q;
    check = (~check + 1);
    move = 4 & check;
    y <<= move;
    count += move;

    q = y >> 30;
    check = !q;
    check = (~check + 1);
    move = 2 & check;
    y <<= move;
    count += move;

    q = y >> 31;
    check = !q;
    check = (~check + 1);
    move = 1 & check;
    y <<= move;
    count += move;

    q = y >> 31;
    check = !q;
    check = (~check + 1);
    move = 1 & check;
    count += move;

    return count;
}

/*
 * logicalNeg - implement the ! operator, using all of
 *              the legal operators except !
 *   Examples: logicalNeg(3) = 0, logicalNeg(0) = 1
 *   Legal ops: ~ & ^ | + << >>
 *   Max ops: 12
 *   Rating: 4
 */
int logicalNeg(int x)
{
    int special = x >> 30;
    int neg_x = ~x + 1;
    int neg_x_xor_x = neg_x ^ x;
    special >>= 1;
    neg_x_xor_x >>= 30;
    neg_x_xor_x >>= 1;
    special = ~special + 1;
    return ((~neg_x_xor_x + 1) | special) ^ 1;
}

/*
 * logicalShift - shift x to the right by n, using a logical shift
 *                Can assume that 0 <= n <= 31
 *   Examples: logicalShift(0x87654321,4) = 0x08765432
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 20
 *   Rating: 3
 */
int logicalShift(int x, int n)
{
    int minus_one = ~0;
    int mask = 1 << 30;
    int choose_y = !!n;
    int choose_z = !n;
    mask <<= 1;
    mask += minus_one; /* 0x7fffffff */
    mask >>= n + minus_one;
    mask = ((~choose_y + 1) & mask) + ((~choose_z + 1) & minus_one);
    x >>= n;
    return x & mask;
}

/*
 * maximumOfTwo - compute the maximum of two integers without branching
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 20
 *   Rating: 4
 */
int maximumOfTwo(int x, int y)
{
    int diff = x + (~y + 1);
    int d_sign = diff >> 31;
    int x_sign = x >> 31;
    int y_sign = y >> 31;
    int sign_xor = x_sign ^ y_sign;
    int yes = sign_xor & y_sign;
    int no = sign_xor & x_sign;
    int greater = (yes | ~d_sign) & (~no);

    return (greater & x) + (~greater & y);
}

/*
 * minimumOfTwo - compute the minimum of two integers without branching
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 20
 *   Rating: 4
 */
int minimumOfTwo(int x, int y)
{
    int diff = x + (~y + 1);
    int d_sign = diff >> 31;
    int x_sign = x >> 31;
    int y_sign = y >> 31;
    int sign_xor = x_sign ^ y_sign;
    int yes = sign_xor & x_sign;
    int no = sign_xor & y_sign;
    int less = (yes | d_sign) & (~no);

    return (less & x) + (~less & y);
}

/*
 * minusOne - return a value of -1
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 2
 *   Rating: 1
 */
int minusOne(void)
{
    return ~0;
}

/*
 * multFiveEighths - multiplies by 5/8 rounding toward 0.
 *                   Should exactly duplicate effect of C expression (x*5/8),
 *                   including overflow behavior.
 *   Examples: multFiveEighths(77) = 48
 *             multFiveEighths(-22) = -13
 *             multFiveEighths(1073741824) = 13421728 (overflow)
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 12
 *   Rating: 3
 */
int multFiveEighths(int x)
{
    int x_mul_4 = x << 2;
    int x_mul_5 = x_mul_4 + x;
    int sign = x_mul_5 >> 31;
    int rounding = ((1 << 3) + (~0)) & sign;
    int res = (x_mul_5 + rounding) >> 3;
    return res;
}

/*
 * negate - return -x
 *   Example: negate(1) = -1.
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 5
 *   Rating: 2
 */
int negate(int x)
{
    return ~x + 1;
}

/*
 * oddBits - return word with all odd-numbered bits set to 1
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 8
 *   Rating: 2
 */
int oddBits(void)
{
    int res = 0xaa;
    res = (res << 8) + res;
    // res = (res << 16) + res;
    res = (res << 15) + (res >> 1);
    res = res << 1;
    return res;
}

/*
 * remainderPower2 - Compute x%(2^n), for 0 <= n <= 30
 *                   Negative arguments should yield negative remainders
 *   Examples: remainderPower2(15, 2) = 3, remainderPower2(-35, 3) = -3
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 20
 *   Rating: 3
 */
int remainderPower2(int x, int n)
{
    int mask = (1 << n) + (~0);
    int sign = x >> 31;
    int rmd = mask & x;
    int zero = !!rmd;
    return ((sign << n) + rmd) & (~zero + 1);
}

/*
 * replaceByte(x,n,c) - Replace byte n in x with c
 *                      Bytes numbered from 0 (LSB) to 3 (MSB)
 *   Examples: replaceByte(0x12345678, 1, 0xab) = 0x1234ab78
 *   You can assume 0 <= n <= 3 and 0 <= c <= 255
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 10
 *   Rating: 3
 */
int replaceByte(int x, int n, int c)
{
    int n_mul_8_bits = n << 3;
    int org = x & (0x000000ff << n_mul_8_bits);
    int res = x >> (n_mul_8_bits + 8);
    res = res << 8;
    res = res | c;
    res = res << n_mul_8_bits;
    res = res | (x ^ org);
    return res;
}

/*
 * rotateLeft - Rotate x to the left by n
 *              Can assume that 0 <= n <= 31
 *   Examples: rotateLeft(0x87654321, 4) = 0x76543218
 *   Legal ops: ~ & ^ | + << >> !
 *   Max ops: 25
 *   Rating: 3
 */
int rotateLeft(int x, int n)
{
    /* ver. 1 WRONG */
    /*
    int m = 32 + (~n + 1);
    int y = x << n;
    int z = x >> m;
    return (y + z);
    */
    int mask = (1 << n) + (~0);
    int zero = !!n;
    int m = (32 + (~n + 1)) & (~zero + 1);
    int tmp = (x >> m) & mask;
    return (x << n) + tmp;
}

/*
 * rotateRight - Rotate x to the right by n
 *               Can assume that 0 <= n <= 31
 *   Examples: rotateRight(0x87654321, 4) = 0x18765432
 *   Legal ops: ~ & ^ | + << >> !
 *   Max ops: 25
 *   Rating: 3
 */
int rotateRight(int x, int n)
{
    int mask = (1 << n) + (~0);
    int zero = !!n;
    int m = (32 + (~n + 1)) & (~zero + 1);
    int mask2 = ((1 << 31) >> n) << 1;
    int tmp = x & mask;
    return ((x >> n) & (~mask2)) + (tmp << m);
}

/*
 * satAdd - adds two numbers but when positive overflow occurs, returns
 *          maximum possible value, and when negative overflow occurs,
 *          it returns minimum positive value.
 *   Examples: satAdd(0x40000000, 0x40000000) = 0x7fffffff
 *             satAdd(0x80000000, 0xffffffff) = 0x80000000
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 30
 *   Rating: 4
 */
int satAdd(int x, int y)
{
    int res = x + y;
    int x_sign = x >> 31;
    int y_sign = y >> 31;
    int r_sign = res >> 31;
    int flow = ~(x_sign ^ y_sign) & (x_sign ^ r_sign);
    int o_or_u = r_sign;
    int Tmin = 1 << 31;
    int Tmax = Tmin + (~0);
    res = (~flow & res) + (flow & ((o_or_u & Tmax) + (~o_or_u & Tmin)));
    return res;
}

/*
 * satMul2 - multiplies by 2, saturating to Tmin or Tmax if overflow
 *   Examples: satMul2(0x30000000) = 0x60000000
 *             satMul2(0x40000000) = 0x7FFFFFFF (saturate to TMax)
 *             satMul2(0x80000001) = 0x80000000 (saturate to TMin)
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 20
 *   Rating: 3
 */
int satMul2(int x)
{
    int Tmin = 1 << 31;
    int Tmax = Tmin + ~0;
    int y = x << 1;
    int x_sign = x >> 31;
    int y_sign = y >> 31;
    int not_ok = x_sign ^ y_sign;
    int ok = ~not_ok;
    int res = (y & ok) + (not_ok & ((Tmax & y_sign) + (Tmin & x_sign)));
    return res;
}

/*
 * satMul3 - multiplies by 3, saturating to Tmin or Tmax if overflow
 *   Examples: satMul3(0x10000000) = 0x30000000
 *             satMul3(0x30000000) = 0x7FFFFFFF (Saturate to TMax)
 *             satMul3(0x70000000) = 0x7FFFFFFF (Saturate to TMax)
 *              satMul3(0xD0000000) = 0x80000000 (Saturate to TMin)
 *             satMul3(0xA0000000) = 0x80000000 (Saturate to TMin)
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 25
 *   Rating: 3
 */
int satMul3(int x)
{
    int res1 = x << 1;
    int x_sign = x >> 31;
    int r1_sign = res1 >> 31;
    int flow1 = x_sign ^ r1_sign;
    int res = res1 + x;
    int r_sign = res >> 31;
    int flow = flow1 | (x_sign ^ r_sign);
    int o_or_u = ~x_sign;
    int Tmin = 1 << 31;
    int Tmax = Tmin + (~0);
    res = (~flow & res) + (flow & ((o_or_u & Tmax) + (~o_or_u & Tmin)));
    return res;
}

/*
 * sign - return 1 if positive, 0 if zero, and -1 if negative
 *   Examples: sign(130) = 1
 *             sign(-23) = -1
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 10
 *   Rating: 2
 */
int sign(int x)
{
    int sign = x >> 30;
    sign >>= 1;
    return sign | !!x;
}

/*
 * signMag2TwosComp - Convert from sign-magnitude to two's complement
 *                    where the MSB is the sign bit
 *   Example: signMag2TwosComp(0x80000005) = -5.
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 15
 *   Rating: 4
 */
int signMag2TwosComp(int x)
{
    int value;
    int neg_value;
    int x_sign;
    int max = 1 << 30; /* 0x7fffffff */
    max <<= 1;
    max += ~0;
    value = x & max;
    neg_value = ~value + 1;
    x_sign = x >> 30;
    x_sign >>= 1;
    return (~x_sign & value) + (x_sign & neg_value);
}

/*
 * specialBits - return bit pattern 0xffca3fff
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 3
 *   Rating: 1
 */
int specialBits(void)
{
    int res = 0xd7 << 14;
    return ~res;
}

/*
 * subtractionOK - Determine if can compute x-y without overflow
 *   Example: subtractionOK(0x80000000, 0x80000000) = 1,
 *            subtractionOK(0x80000000, 0x70000000) = 0,
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 20
 *   Rating: 3
 */
int subtractionOK(int x, int y)
{
    /*** too many ops***/
    int neg_y = ~y + 1;
    int x_sign;
    int y_sign;
    int res_sign;
    int min = 1 << 30;
    int min_or_not;
    int common;
    int special;
    int choose_common;
    int choose_specail;
    int OK;

    min <<= 1;
    min_or_not = !(y ^ min);
    choose_common = !min_or_not;
    choose_specail = min_or_not;

    x_sign = x >> 30;
    y_sign = neg_y >> 30;
    res_sign = (x + neg_y) >> 30;
    x_sign >>= 1;
    y_sign >>= 1;
    res_sign >>= 1;

    common = (!!(x_sign ^ y_sign) | !(res_sign ^ x_sign));
    special = (!(min_or_not & (!x_sign)));
    OK = ((~choose_common + 1) & common) + ((~choose_specail + 1) & special);
    return OK;
}

/*
 * thirdBits - return word with every third bit (starting from the LSB)
 *             set to 1
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 8
 *   Rating: 1
 */
int thirdBits(void)
{
    int res = 0x00000049;
    int tmp = 0x00000024;
    tmp = (tmp << 4) + 0x00000009;
    res = (res << 12) + tmp;
    res = (res << 12) + tmp;
    return res;
}

/*
 * TMax - return maximum two's complement integer
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 4
 *   Rating: 1
 */
int tmax(void)
{
    int max = 1 << 30;
    max <<= 1;
    max += ~0;
    return max;
}

/*
 * tmin - return minimum two's complement integer
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 4
 *   Rating: 1
 */
int tmin(void)
{
    int min = 1 << 30;
    min <<= 1;
    return min;
}

/*
 * trueFiveEighths - multiplies by 5/8 rounding toward 0,
 *                   avoiding errors due to overflow
 *   Examples: trueFiveEighths(11) = 6
 *             trueFiveEighths(-9) = -5
 *             trueFiveEighths(0x30000000) = 0x1E000000 (no overflow)
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 25
 *   Rating: 4
 */
int trueFiveEighths(int x)
{
    /*
    int sign = x >> 31;
    int rounding = ((1 << 3) + (~0)) & sign;
    int res = (x + rounding) >> 3;
    res = res + res + res + res + res;
    return res;
    */
    int sign = x >> 31;
    int rmd = x & 0x7;
    int res = x >> 3;
    int floating, rounding;
    rmd = (rmd << 2) + rmd;
    floating = rmd;
    floating >>= 3;
    floating &= ~sign;
    rounding = rmd;
    rounding += 0x7;
    rounding >>= 3;
    rounding &= sign;
    res = (res << 2) + res + floating + rounding;
    return res;
}

/*
 * trueThreeFourths - multiplies by 3/4 rounding toward 0,
 *                    avoiding errors due to overflow
 *   Examples: trueThreeFourths(11) = 8
 *             trueThreeFourths(-9) = -6
 *             trueThreeFourths(1073741824) = 805306368 (no overflow)
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 20
 *   Rating: 4
 */
int trueThreeFourths(int x)
{
    /*
    int sign = x >> 31;
    int twobit = x & 0x3;
    int res = x >> 2;
    res = (res << 1) + res;

    twobit += (sign << 2);
    twobit = (twobit << 1) + twobit;
    twobit >>= 2;
    return res + twobit;
    */
    /*
    int x_mul_2 = x << 1;
    int x_mul_3 = x_mul_2 + x;
    int sign = x_mul_3 >> 31;
    int rounding = ((1 << 2) + (~0)) & sign;
    int res = (x_mul_3 + rounding) >> 2;
    return res;
    */
    /*
    int sign = x >> 31;
    int rounding = ((1 << 2) + (~0)) & sign;
    int res = (x + rounding) >> 2;
    int floating = x & 0x3;
    floating = floating + floating + floating;
    floating >>= 2;
    floating &= ~sign;
    res = res + res + res + floating;
    return res;
    */
    int sign = x >> 31;
    int rmd = x & 0x3;
    int res = x >> 2;
    int floating, rounding;
    rmd = rmd + rmd + rmd;
    floating = rmd;
    floating >>= 2;
    floating &= ~sign;
    rounding = rmd;
    rounding += 0x3;
    rounding >>= 2;
    rounding &= sign;
    res = res + res + res + floating + rounding;
    return res;
}

/*
 * twosComp2SignMag - Convert from two's complement to sign-magnitude
 *                    where the MSB is the sign bit
 *                    You can assume that x > TMin
 *   Example: twosComp2SignMag(-5) = 0x80000005.
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 15
 *   Rating: 4
 */
int twosComp2SignMag(int x)
{
    int abs_x;
    int x_sign = x >> 30;
    x_sign >>= 1;
    abs_x = (x ^ x_sign) + (~x_sign + 1);
    x_sign = ~x_sign + 1;
    x_sign <<= 30;
    x_sign <<= 1;
    return abs_x | x_sign;
}

/*
 * upperBits - pads n upper bits with 1's
 *             You may assume 0 <= n <= 32
 *   Example: upperBits(4) = 0xF0000000
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 10
 *   Rating: 1
 */
int upperBits(int n)
{
    int res = 1 << 30;
    res <<= !!n;
    res >>= n + ~0;
    return res;
}
