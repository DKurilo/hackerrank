class ListNode {
  val: number;

  next: ListNode | null;

  constructor(val?: number, next?: ListNode | null) {
    this.val = val === undefined ? 0 : val;
    this.next = next === undefined ? null : next;
  }
}

type ListNodeF = ListNode | null;

const arrToListNode = (xs: number[]): ListNodeF =>
  xs.length === 0 ? null : new ListNode(xs[0], arrToListNode(xs.slice(1)));

const listNodeToArr = (ln: ListNodeF): number[] =>
  ln === null ? [] : [ln.val].concat(listNodeToArr(ln.next));

const fix = <T>(f: (g: (x: T) => T) => (x: T) => T): ((x: T) => T) => {
  const g = (x: T): T => f(g)(x);
  return g;
};

/**
 * Definition for singly-linked list.
 * class ListNode {
 *     val: number
 *     next: ListNode | null
 *     constructor(val?: number, next?: ListNode | null) {
 *         this.val = (val===undefined ? 0 : val)
 *         this.next = (next===undefined ? null : next)
 *     }
 * }
 */
const removeNthFromEnd = (head: ListNodeF, n: number): ListNodeF => {
  const nth: ListNodeF = fix<[ListNodeF, number]>((rec) => ([ln, k]) =>
    k <= 1 || ln === null ? [ln, 0] : rec([ln.next, k - 1]),
  )([head, n])[0];
  if (nth === null) {
    return head;
  }
  return fix<[ListNodeF, ListNodeF]>((rec) => ([ln1, ln2]) =>
    ln2.next === null
      ? [ln1.next, ln2]
      : [new ListNode(ln1.val, rec([ln1.next, ln2.next])[0]), null],
  )([head, nth])[0];
};

console.log(
  listNodeToArr(removeNthFromEnd(arrToListNode([1, 2, 3, 4, 5]), 2)),
  [1, 2, 3, 5],
);

console.log(listNodeToArr(removeNthFromEnd(arrToListNode([1]), 1)), []);

console.log(listNodeToArr(removeNthFromEnd(arrToListNode([1, 2]), 1)), [1]);

console.log(listNodeToArr(removeNthFromEnd(arrToListNode([1, 2]), 2)), [2]);

console.log(listNodeToArr(removeNthFromEnd(arrToListNode([1, 2]), 3)), [1, 2]);

const arrsCompare = <T>(xs: T[], ys: T[]): boolean => {
  if (xs.length !== ys.length) {
    return false;
  }
  if (xs.length === 0) {
    return true;
  }
  if (xs[0] !== ys[0]) {
    return false;
  }
  return arrsCompare(xs.slice(1), ys.slice(1));
};
const arr = [...new Array(Math.trunc(Math.random() * 1000))].map(() =>
  Math.trunc(Math.random() * 1000000),
);
const n = Math.trunc(Math.random() * (arr.length + 10));
console.log(
  arrsCompare(
    listNodeToArr(removeNthFromEnd(arrToListNode(arr), n)),
    arr.slice(0, arr.length - n).concat(arr.slice(arr.length - n + 1)),
  ),
);
