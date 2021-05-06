const isValid = (s: string): boolean =>
  s.split('').reduce((ps, c) => {
    if (ps.length === 1 && ps[0] === 'f') {
      return ps;
    }
    if (['{', '[', '('].includes(c)) {
      return [c].concat(ps);
    }
    if (['}', ']', ')'].includes(c)) {
      if (ps.length > 0 && ps[0] === { '}': '{', ']': '[', ')': '(' }[c]) {
        return ps.slice(1);
      }
      return ['f'];
    }
    return ps;
  }, []).length === 0;

console.log(isValid('()'), true);
console.log(isValid('{}'), true);
console.log(isValid('[]'), true);
console.log(isValid('()[]{}'), true);
console.log(isValid('(]'), false);
console.log(isValid('([)]'), false);
console.log(isValid('{[]}'), true);
console.log(isValid('sdfs{fsfsf[fsdfs]fsdfsd}fsdsdf'), true);
