// Magic number [-2^31, 2^31 - 1]
const MAX = 2147483647;
const MIN = 0 - 2147483648;

const getSimpleQuotientModulo = (
  dividend: number,
  divisor: number,
): [number, number] =>
  dividend >= divisor
    ? (([q, m]): [number, number] => [q + 1, m])(
        getSimpleQuotientModulo(dividend - divisor, divisor),
      )
    : [0, dividend];

const mul10 = (x: number): number => {
  const x2 = x + x;
  const x4 = x2 + x2;
  const x8 = x4 + x4;
  return x8 + x2;
};

const equalizeDigits = (x: number, ys: number[]): number[] => {
  const y10 = mul10(ys[0]);
  return y10 >= x ? ys : equalizeDigits(x, [y10].concat(ys));
};

const setSign = (dividend: number, divisor: number, result: number): number =>
  dividend < 0 !== divisor < 0 ? 0 - result : result;

const simpleDivideDoer = (
  dividend: number,
  divisors: number[],
  prevResult: number,
): number => {
  if (divisors.length === 0) {
    return prevResult;
  }
  const [quotient, modulo] = getSimpleQuotientModulo(dividend, divisors[0]);
  return simpleDivideDoer(
    modulo,
    divisors.slice(1),
    mul10(prevResult) + quotient,
  );
};

const simpleDivide = (dividend: number, divisor: number): number =>
  simpleDivideDoer(dividend, equalizeDigits(dividend, [divisor]), 0);

function divide(dividend: number, divisor: number): number {
  if (
    dividend > MAX ||
    dividend < MIN ||
    divisor > MAX ||
    divisor < MIN ||
    divisor === 0
  ) {
    return MAX;
  }
  const absDividend = dividend < 0 ? 0 - dividend : dividend;
  const absDivisor = divisor < 0 ? 0 - divisor : divisor;
  const quotient = setSign(
    dividend,
    divisor,
    absDividend < absDivisor ? 0 : simpleDivide(absDividend, absDivisor),
  );
  return quotient > MAX || quotient < MIN ? MAX : quotient;
}

console.log(divide(10, 2));
console.log(divide(42323, 534), Math.floor(42323 / 534));
console.log(divide(42323, -534), Math.ceil(42323 / 534));
console.log(divide(-42323, 534), Math.ceil(42323 / 534));
console.log(divide(-42323, -534), Math.floor(42323 / 534));
console.log(divide(42323, -54233234));
console.log(divide(42323, 0));
console.log(divide(-2147483648, -1));
console.log(divide(-2147483648, 1));
console.log(divide(2147483648, 1));
console.log(divide(2147483648, -1));
