import string

BLACK = 0
RED = 1

class Node:
    def __init__(self, key, color = RED):
        self.key = key;
        self.color = color;
        self.left = self.right = self.parent = None

    def set_left(self, x):
        self.left = x
        if x != None:
            x.parent = self

    def set_right(self, x):
        self.right = x
        if x != None:
            x.parent = self

    def set_children(self, x, y):
        self.set_left(x)
        self.set_right(y)

    #parent<->self ==> parent<->y
    def replace_by(self, y):    
        if self.parent is None:
            y.parent = None
        elif self.parent.left == self:
            self.parent.set_left(y)
        else:
            self.parent.set_right(y)
        self.parent = None

            
# rotatoins

# (a x (b y c)) ==> ((a x b) y c)
def left_rotate(t, x):
    (parent, y) = (x.parent, x.right)
    (a, b, c)   = (x.left, y.left, y.right)
    x.replace_by(y)
    x.set_children(a, b)
    y.set_children(x, c)
    if parent is None:
        t=y
    return t

# (a x (b y c)) <== ((a x b) y c)
def right_rotate(t, y):
    (parent, x) = (y.parent, y.left)
    (a, b, c)   = (x.left, x.right, y.right)
    y.replace_by(x)
    y.set_children(b, c)
    x.set_children(a, y)
    if parent is None:
        t = x
    return t

# insertion and deletion

def rb_insert(t, key): #returns the new root
    root = t
    x = Node(key)
    parent = None
    while(t):
        parent = t
        if(key < t.key):
            t = t.left
        else:
            t = t.right
    if(parent == None): #tree is empty
        return x
    elif(key < parent.key):
        parent.set_left(x)
    else:
        parent.set_right(x)
    return rb_insert_fix(root, x)

def rb_insert_fix(t, x):
    root=t
    
    root.color = Black

def remove_node(x):
    if (x is None): return
    x.parent = x.left = x.right = None

def rb_delete(t, x):
    if (x is None): return t
    [root, old_x, parent] = [t, x, x.parent]
    if (x.left is None):
        x = x.right
    elif (x.right is None):
        x = x.left
    else:
        y = tree_min(x.right)
        x.key = y.key
        if (y.parent != x):
            y.parent.left = y.right
        else:
            x.right = y.right
        remove_node(y)
        return root
    if (x != None):
        x.parent = parent
    if (parent != None):
        root = x
    else:
        if(parent.left == old_x): parent.left = x
        else: parent.right = x
    remove_node(old_x)
    return root

# Helper functions

def rbtree_clone(t):
    n = None
    if t != None:
        n = Node(t.key, t.color)
        n.set_children(rbtree_clone(t.left), rbtree_clone(t.right))
    return n

def rbtree_to_str(t):
    if t is None:
        return "."
    else:
        color = {RED:"R", BLACK:"B"}
        return "("+rbtree_to_str(t.left)+ " " + str(t.key) +":"+color[t.color]+" " + rbtree_to_str(t.right)+")"

class Test:
    def __init__(self):
        #t1 = ((1B 2R (4B 3R .)) 5B (6B 7R (8R 9B .)))
        self.t1=Node(5, BLACK)
        self.t1.set_children(Node(2), Node(7))
        self.t1.left.set_children(Node(1, BLACK), Node(4, BLACK))
        self.t1.right.set_children(Node(6, BLACK), Node(9, BLACK))
        self.t1.left.right.set_left(Node(3))
        self.t1.right.right.set_left(Node(8))
        print rbtree_to_str(self.t1)

    def run(self):

        self.test_rotate()

    def test_rotate(self):
        t = rbtree_clone(self.t1)
        x = t.right #7R
        t = left_rotate(t, x) #(6 7 (8 9 .) ==> ((6 7 8) 9 .)
        print rbtree_to_str(t)
        t = right_rotate(t, t.right) #rotate back
        print rbtree_to_str(t)
        t = rbtree_clone(self.t1)
        t = left_rotate(t, t) #(2 5 (6 7 9) ==> ((2 5 6) 7 9)
        print rbtree_to_str(t)
        t = right_rotate(t, t)
        print rbtree_to_str(t)

if __name__ == "__main__":
    Test().run()
