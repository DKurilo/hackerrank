/**
 * Definition for singly-linked list.
 * */
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

const findNth = (head: ListNode, n: number): ListNode => {
  let nth = head;
  for (let i = 0; i < n; i += 1) {
    nth = nth.next === null ? nth : nth.next;
  }
  return nth;
};

const findLastAndBeforeKthFromEnd = (
  head: ListNodeF,
  k: number,
): [ListNode, ListNode] => {
  let last = head;
  let beforeKthFromEnd = head;
  let lastIndex = 0;
  while (last.next !== null) {
    lastIndex += 1;
    last = last.next;
    if (lastIndex > k) {
      beforeKthFromEnd = beforeKthFromEnd.next;
    }
  }
  if (lastIndex < k) {
    beforeKthFromEnd = findNth(head, lastIndex - (k % (lastIndex + 1)));
  }
  return [last, beforeKthFromEnd];
};

const rotateRight = (head: ListNodeF, k: number): ListNodeF => {
  if (head === null || head.next === null) {
    return head;
  }
  const [last, beforeKthFromEnd] = findLastAndBeforeKthFromEnd(head, k);
  if (last !== beforeKthFromEnd) {
    last.next = head;
    const kthFromEnd = beforeKthFromEnd.next;
    beforeKthFromEnd.next = null;
    return kthFromEnd;
  }
  return head;
};

console.log(listNodeToArr(rotateRight(arrToListNode([1, 2, 3, 4, 5]), 1)), [
  5,
  1,
  2,
  3,
  4,
]);
console.log(listNodeToArr(rotateRight(arrToListNode([1, 2, 3, 4, 5]), 2)), [
  4,
  5,
  1,
  2,
  3,
]);
console.log(
  listNodeToArr(rotateRight(arrToListNode([1, 2, 3, 4, 5]), 10000000000)),
  [1, 2, 3, 4, 5],
);
console.log(listNodeToArr(rotateRight(arrToListNode([0, 1, 2]), 1)), [2, 0, 1]);
console.log(listNodeToArr(rotateRight(arrToListNode([0, 1, 2]), 2)), [1, 2, 0]);
console.log(listNodeToArr(rotateRight(arrToListNode([0, 1, 2]), 3)), [0, 1, 2]);
console.log(listNodeToArr(rotateRight(arrToListNode([0, 1, 2]), 4)), [2, 0, 1]);
console.log(listNodeToArr(rotateRight(null, 4)), []);
console.log(listNodeToArr(rotateRight(arrToListNode([6]), 4000000)), [6]);
