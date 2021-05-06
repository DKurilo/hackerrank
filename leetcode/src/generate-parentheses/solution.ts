const generateParenthesis = (n: number): string[] => {
  const doer = (ps: string[], on: number, cn: number): string[] => {
    if (on <= 0 && cn <= 0) {
      return ps;
    }
    if (on <= 0) {
      const suffix = ')'.repeat(cn);
      return ps.map((s) => s + suffix);
    }
    if (ps.length === 0) {
      return doer(['('], on - 1, cn);
    }
    if (on === cn) {
      return doer(
        ps.map((s) => `${s}(`),
        on - 1,
        cn,
      );
    }
    return doer(
      ps.map((s) => `${s}(`),
      on - 1,
      cn,
    ).concat(
      doer(
        ps.map((s) => `${s})`),
        on,
        cn - 1,
      ),
    );
  };
  return doer([], n, n);
};

console.log(generateParenthesis(2), ['(())', '()()']);
console.log(generateParenthesis(3), [
  '((()))',
  '(()())',
  '(())()',
  '()(())',
  '()()()',
]);
console.log(generateParenthesis(1), ['()']);
console.log(generateParenthesis(8).length);
console.log(generateParenthesis(16).length);
