# class TreeNode:
#     def __init__(self, x):
#         self.val = x
#         self.left = None
#         self.right = None
#         self.next = None

# BFS like traverse
def link(tr):
    if tr is None or tr.left is None:
        return
    (root, tr.next) = (tr, tr.left)
    (head, tail) = (tr, tr)
    while head.next:
        tr, head = deq(head)
        tail = enq(tail, tr.left)
        tail = enq(tail, tr.right)
    # break at 1, 2, 4, ...
    (i, m) = (0, 1)
    prev = tr = root
    while tr:
        (prev, tr) = (tr, tr.next)
        i = i + 1
        if i == m:
            prev.next = None
            (i, m) = (0, 2 * m)

def deq(head):
    return head, head.next

def enq(tail, x):
    if x:
        tail.next = x
        return x
    else:
        return tail
