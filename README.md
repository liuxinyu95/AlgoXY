Elementary Algorithms
====

Edition: 0.6180339887498949

2020/10

This book introduces about elementary algorithms and data structure. It includes side-by-side comparison about purely functional realization and their imperative counterpart.

<img src="https://file.ituring.com.cn/ScreenShow/01004d1e213e7f854b92" width="400">

Contents
--------

The book can be downloaded in English ([EN](https://github.com/liuxinyu95/AlgoXY/releases/download/v0.6180333/elementary-algorithms.pdf)). The book in Chinese ([中文](http://www.ituring.com.cn/book/1907)) was published. I recently switched my focus to the Mathematics of programming, the new book is also available in ([github](https://github.com/liuxinyu95/unplugged))


- Preface
- Chapter 1, Binary Search Tree, the 'hello world' data structure;
- Chapter 2, The evolution of insertion sort;
- Chapter 3, Red-black tree, not so complex as it was thought;
- Chapter 4, AVL tree;
- Chapter 5, Radix tree, Trie and Prefix Tree;
- Chapter 6, B-Trees;
- Chapter 7, Binary Heaps;
- Chapter 8, From grape to the world cup, the evolution of selection sort;
- Chapter 9, Binomial heap, Fibonacci heap, and pairing heap;
- Chapter 10, Queue, not so simple as it was thought;
- Chapter 11, Sequences, The last brick;
- Chapter 12, Divide and conquer, Quick sort vs. Merge sort
- Chapter 13, Searching
- Appendix

Install
--------

To build the book in PDF format from the sources, you need
the following software pre-installed.

- TeXLive, The book is built with XeLaTeX, a Unicode friendly version of TeX;

### Install TeXLive

In Debian/Ubuntu like Linux environment, do **NOT** install the TeXLive through apt-get. Go to TeXLive [official site](https://tug.org/texlive/) to download the setup script.

```bash
$ wget http://mirror.ctan.org/systems/texlive/tlnet/install-tl.zip
$ unzip install-tl.zip
$ cd install-tl
$ sudo ./install-tl -gui text -repository http://mirror.ctan.org/systems/texlive/tlnet
```

In Windows, TeXLive provide a [gui based installer](https://tug.org/texlive/), in Mac OS X, there's a [MacTeX](https://www.tug.org/mactex/).

### Install ImageMagick and Graphviz

```bash
$ sudo apt-get install imagemagick
$ sudo apt-get install graphviz
```

For Windows and Mac OS X installer, ImageMagick can be download through http://www.imagemagick.org; Graphviz can be download from: http://graphviz.org

### Others

You need the GNU make tool, in Debian/Ubuntu like Linux, it can be installed through the apt-get command:

```bash
$ sudo apt-get install build-essential
```

In Windows, you can install the MSYS for it. In Mac OS X, please install the developer tool from this command line:

```bash
$ xcode-select --install
```

### Build the book PDF

enter the folder contains the book TeX manuscript, run

```bash
$ make
```

This will generate algoxy-en.pdf and algoxy-zh-cn.pdf. If you only need the Chinese version for example, you can run `make cn` instead.

### Other branches

The other two branches, `zh-cn` and `jvm` are deprecated. Please do NOT checkout/track them.

--

LIU Xinyu

liuxinyu95@gmail.com

``Cogito ergo sum''
