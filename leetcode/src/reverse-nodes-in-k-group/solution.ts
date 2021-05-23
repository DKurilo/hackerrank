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

const reverseK = (head: ListNodeF, last: ListNodeF): void => {
  if (head === null || last === null) return;
  let f = head;
  let l = last;
  while (f !== l && l.next !== f) {
    const v = f.val;
    f.val = l.val;
    l.val = v;
    let nextL = f;
    while (nextL.next !== l) {
      nextL = nextL.next;
    }
    f = f.next;
    l = nextL;
  }
};

// I don't like this task. Linked list is not natural data structure for JS
// It's natural thing for Haskell and in Haskell I would split list, map reverse to each part (except last one)
// and join again. It's only one line of code like:
// reverseKGroup k = concatMap (\xs -> if length xs == k then reversexs else xs) . chunksOf k
// additional requirement, it should work in O(1) additional space.
function reverseKGroup(head: ListNodeF, k: number): ListNodeF {
  let start = head;
  let kth;
  while (start !== null) {
    kth = start;
    for (let i = 1; i < k; i += 1) {
      kth = kth.next;
      if (kth === null) {
        return head;
      }
    }
    reverseK(start, kth);
    start = kth.next;
  }
  return head;
}

console.log(listNodeToArr(reverseKGroup(arrToListNode([1, 2, 3, 4, 5]), 2)));
console.log(listNodeToArr(reverseKGroup(arrToListNode([1, 2, 3, 4, 5]), 3)));
console.log(listNodeToArr(reverseKGroup(arrToListNode([1, 2, 3, 4, 5]), 1)));
console.log(listNodeToArr(reverseKGroup(arrToListNode([1]), 1)));

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
const n = Math.trunc(Math.random() * (arr.length - 1) + 1);
