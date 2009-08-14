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

    def set_parent(self, x):
        self.parent == x
            

# rotatoins
#(a x (b y c)) <==> ((a x b) y c)

def left_rotate(t, x):
    y = x.right
    (a, b, c) = (x.left, y.left, y.right)
    x.set_children(a, b)
    y.set_right(c)
    if(x.parent == None):
        t=y
    elif(x.parent.left == x):
        x.parent.left=y
    else:
        x.parent.right=y
    y.set_left(x)
    

def rb_insert(t, key):
    root = t
    x = Node(key)
    parent = None
    while(t):
        parent = t
        if(key < t.key):
            t = t.left
        else:
            t = t.right
    x.parent = parent
    if(parent == None): #tree is empty
        return x
    elif(key < parent.key):
        parent.left = x
    else:
        parent.right = x
    return rb_insert_fix(root, x)

def rb_insert_fix(t, x):
    pass
