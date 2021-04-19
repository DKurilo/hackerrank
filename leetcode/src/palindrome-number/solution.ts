const revert = (x: number, y = 0): number =>
  x === 0 ? y : revert(Math.floor(x / 10), y * 10 + (x % 10));

function isPalindrome(x: number): boolean {
  return x >= 0 && x === revert(x);
}

console.log(isPalindrome(121));
console.log(isPalindrome(142423324241));
console.log(isPalindrome(-121));
console.log(isPalindrome(0));
console.log(isPalindrome(10));
console.log(isPalindrome(11));
