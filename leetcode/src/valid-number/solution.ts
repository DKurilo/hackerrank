// eslint-disable-next-line no-use-before-define
type Checker = (c: string) => Result;
type Result = { res: boolean; f: Checker };
type Predicate = (c: string) => boolean;

const isDigit: Predicate = (c) => c.length === 1 && c >= '0' && c <= '9';
const isDot: Predicate = (c) => c === '.';
const isSign: Predicate = (c) => c === '+' || c === '-';
const isE: Predicate = (c) => c === 'e' || c === 'E';
const isTerminal: Predicate = (c) => c === '#';

const noopChecker: Checker = () => ({ res: false, f: noopChecker });
const noResult: Result = { res: false, f: noopChecker };

const applyChecker = ({ res, f }: Result, c: string): Result =>
  res ? f(c) : { res: false, f: noopChecker };

const terminalCheck: Checker = (c) =>
  isTerminal(c) ? { res: true, f: noopChecker } : noResult;

const combine = (cs: Checker[]): Checker => (c) =>
  cs.reduce(({ res, f }, f1) => (res ? { res, f } : f1(c)), noResult);

const exponentaDigitCheck: Checker = (c) =>
  isDigit(c)
    ? { res: true, f: combine([exponentaDigitCheck, terminalCheck]) }
    : noResult;

const exponentaSignCheck: Checker = (c) =>
  isSign(c) ? { res: true, f: exponentaDigitCheck } : noResult;

const eCheck: Checker = (c) =>
  isE(c)
    ? { res: true, f: combine([exponentaSignCheck, exponentaDigitCheck]) }
    : noResult;

const mantissaDigitBeforeDotCheck: Checker = (c) =>
  isDigit(c)
    ? {
        res: true,
        f: combine([
          mantissaDigitBeforeDotCheck,
          // eslint-disable-next-line no-use-before-define
          dotAfterDigitCheck,
          eCheck,
          terminalCheck,
        ]),
      }
    : noResult;

const dotAfterDigitCheck: Checker = (c) =>
  isDot(c)
    ? {
        res: true,
        // eslint-disable-next-line no-use-before-define
        f: combine([mantissaDigitAfterDotCheck, eCheck, terminalCheck]),
      }
    : noResult;

const dotBeforeDigitCheck: Checker = (c) =>
  isDot(c)
    ? // eslint-disable-next-line no-use-before-define
      { res: true, f: mantissaDigitAfterDotCheck }
    : noResult;

const mantissaSignCheck: Checker = (c) =>
  isSign(c)
    ? {
        res: true,
        f: combine([dotBeforeDigitCheck, mantissaDigitBeforeDotCheck]),
      }
    : noResult;

const mantissaDigitAfterDotCheck: Checker = (c) =>
  isDigit(c)
    ? {
        res: true,
        f: combine([eCheck, mantissaDigitAfterDotCheck, terminalCheck]),
      }
    : noResult;

const firstCharCheck: Checker = combine([
  mantissaSignCheck,
  mantissaDigitBeforeDotCheck,
  dotBeforeDigitCheck,
]);

function isNumber(s: string): boolean {
  return `${s}#`
    .split('')
    .reduce(applyChecker, { res: true, f: firstCharCheck }).res;
}

console.log(
  [
    '2',
    '0089',
    '-0.1',
    '+3.14',
    '4.',
    '-.9',
    '2e10',
    '-90E3',
    '3e+7',
    '+6e-1',
    '53.5e93',
    '-123.456e789',
    '+12.e4',
  ].map(isNumber),
);

console.log(
  [
    'abc',
    '1a',
    '1e',
    'e3',
    '99e2.5',
    '--6',
    '-+3',
    '95a54e53',
    '.',
    '+.',
    '-.',
  ].map(isNumber),
);
