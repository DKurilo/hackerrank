type ParsedNumber =
  | {
      sign: 1 | -1;
      n: number;
      finished: boolean;
    }
  | 'empty'
  | 'error'
  | 'overflowPositive'
  | 'overflowNegative';

const MININT = -(2 ** 31);
const MAXINT = 2 ** 31 - 1;
const ERROR = 0;

const parsedToNumber = (pn: ParsedNumber): number => {
  switch (pn) {
    case 'error':
    case 'empty':
      return ERROR;
    case 'overflowNegative':
      return MININT;
    case 'overflowPositive':
      return MAXINT;
    default:
      return pn.sign * pn.n;
  }
};

const getDigit = (c: string): number | null => {
  switch (c) {
    case '0':
      return 0;
    case '1':
      return 1;
    case '2':
      return 2;
    case '3':
      return 3;
    case '4':
      return 4;
    case '5':
      return 5;
    case '6':
      return 6;
    case '7':
      return 7;
    case '8':
      return 8;
    case '9':
      return 9;
    default:
      return null;
  }
};

function myAtoi(s: string): number {
  return parsedToNumber(
    s.split('').reduce((parsed: ParsedNumber, c: string): ParsedNumber => {
      if (
        parsed === 'error' ||
        parsed === 'overflowPositive' ||
        parsed === 'overflowNegative'
      ) {
        return parsed;
      }
      const d = getDigit(c);
      if (parsed === 'empty') {
        if (c === ' ') {
          return parsed;
        }
        if (c === '-') {
          return { sign: -1, n: 0, finished: false };
        }
        if (c === '+') {
          return { sign: 1, n: 0, finished: false };
        }
        if (d !== null) {
          return { sign: 1, n: d, finished: false };
        }
        return 'error';
      }
      if (parsed.finished) {
        return parsed;
      }
      if (d === null) {
        return { ...parsed, finished: true };
      }
      const n = parsed.n * 10 + d;
      if (parsed.sign === 1 && n > MAXINT) {
        return 'overflowPositive';
      }
      if (parsed.sign === -1 && -n < MININT) {
        return 'overflowNegative';
      }
      return { sign: parsed.sign, finished: false, n };
    }, 'empty'),
  );
}

console.log(myAtoi('42'), 42);
console.log(myAtoi('   -42'), -42);
console.log(myAtoi('4193 with words'), 4193);
console.log(myAtoi('words and 987'), 0);
console.log(myAtoi('-91283472332'), -2147483648);
console.log(myAtoi('2147483648'), 2147483647);
console.log(myAtoi('-2147483649'), -2147483648);
console.log(myAtoi(''), 0);
