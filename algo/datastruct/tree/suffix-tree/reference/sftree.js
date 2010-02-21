var Txt='',    // the input text string
    root=null, // root of the suffix tree
    infinity;  // quite a big number
    nForks=0;  // number of branching nodes in the suffix tree

function pair(a, b) { this.fst = a; this.snd = b; } // i.e. <fst, snd>
// NB. most of Ukkonen's functions return a pair (s,w)

function isEmptyStrng() { return this.right < this.left; }

function Strng(left, right){ // represents Txt[left..right]
    this.left=left; this.right=right;
    this.isEmpty = isEmptyStrng;
}//constructor


function addTrnstn(left, right, s){ // this['a'] >---(left..right)---> s
// add a transition to `this' state
    this[Txt.charAt(left)] = new pair(new Strng(left,right), s);
    this.isLeaf = false;
}

function State() // i.e. a new leaf node in the suffix tree
{ this.addTransition = addTrnstn; this.isLeaf = true; }


function show(T, str, arc){ // print the suffix tree
    if(T == null){//should not happen!
	document.theForm.opt.value += str+arc+'NULL !!!\n';
	return;//should not be here
    }
    //else
    if(T.isLeaf){
	document.theForm.opt.value += str+arc+'leaf\n';
	return;//llewop d
    }
    //else
    nForks++;
    var attr, iter = 0;
    var spaces = '';  var i;
    for(i=1; i < arc.length; i++) 
	spaces += ' ';
    spaces += '|';   // |spaces|==|arc|
    var str2 = str+spaces;//nosilla l

    for(attr in T)//each subtree
      if(attr.length == 1)//a char attribute selects a suffix-tree branch
       { iter++;//ics pmoc hsanom
         var wAndT2 = T[attr];
         var w = wAndT2.fst, T2 = wAndT2.snd;
         var myStr = '('+(w.left+1)+':'+Txt.substring(w.left, w.right+1)+')|';
         if(iter > 1)//must get to at least 2 if suffix tree is correct.
            document.theForm.opt.value += (iter==2 ? str+arc : str2)+'\n';
         show(T2, str2, myStr)
       }
}//show

// (s, (k, i-1)) is the canonical reference pair for the active point
function upDate(s, k, i){
    var oldr = root;
    var (endPoint, r) = test_and_split(s, k, i-1, Txt.charAt(i));

    while (!endPoint){ 
	r.addTransition(i, infinity, new State());
	if (oldr != root) 
	    oldr.sLink = r; // build suffix-link active-path

	oldr = r;
	var (s,k) = canonize(s.sLink, k, i-1);
	(endPoint, r) = test_and_split(s, k, i-1, Txt.charAt(i));
    }

    if(oldr != root) 
	oldr.sLink = s;
    return new pair(s, k);
}//upDate

function test_and_split(s, k, p, t){ 
    if(k<=p){ 
	// find the t_k transition g'(s,(k',p'))=s' from s
	// k1 is k'  p1 is p' in Ukkonen '95
	var ((k1,p1), s1)  = s[Txt.charAt(k)];

	if (t == Txt.charAt(k1 + p - k + 1))
	    return new pair(true, s);
	else{ 
	    var r = new State();
	    s.addTransition(k1, k1+p-k,   r);     // s---->r---->s1
	    r.addTransition(    k1+p-k+1, p1, s1);
	    return new pair(false, r);
	}
    }
    else // k > p;  ? is there a t-transition from s ?
	return new pair(s[t] != null, s);
}//test_and_split

// s--->...
function canonize(s, k, p){ 
    if(p < k) 
	return new pair (s, k);

   // find the t_k transition g'(s,(k',p'))=s' from s
   // k1 is k',  p1 is p' in Ukk' '95
   var ((k1,p1), s1) = s[Txt.charAt(k)];     // s--(k1,p1)-->s1

   while(p1-k1 <= p-k){                      // s--(k1,p1)-->s1--->...
       k += p1 - k1 + 1;  // remove |(k1,p1)| chars from front of (k,p)
       s = s1;
       if(k <= p)
	   ((k1,p1), s1) = s[Txt.charAt(k)];   // s--(k1,p1)-->s1
   }
   return new pair(s, k);
}//canonize

// construct suffix tree for Txt[0..N-1]
function ukkonen95(){ 
    var s, k, i;
    var bt;

    root = new State();
    bt = new State();                            // bt (bottom or _|_)

    // Want to create transitions for all possible chars
    // from bt to root
    for (i=0; i < Txt.length; i++)
	bt.addTransition(i,i, root);

    root.sLink = bt;
    s=root; k=0;    // NB. k=0, unlike Ukkonen our strings are 0 based

    for(i=0; i < Txt.length; i++){
	var (s,k) = upDate(s, k, i);   // follow path from active-point
	(s,k) = canonize(s, k, i);
    }
}//ukkonen95


function stDriver()
 { Txt = document.theForm.inp.value;
   infinity = Txt.length + 1000; // well it's quite big :-)
   nForks = 0;

   document.theForm.opt.value = '';
   insertionSort(Txt);

   algorithm2();  // ------------ the business
   show(root, '', 'tree:|');
   document.theForm.opt.value += nForks + ' branching nodes';
 }//stDriver
