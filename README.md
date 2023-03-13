Elementary Algorithms
====

Edition: $\displaystyle e = \sum \limits _{n=0}^{\infty }{\frac {1}{n!}} = 1 + {\frac {1}{1}}+{\frac {1}{1\cdot 2}}+{\frac {1}{1\cdot 2\cdot 3}}+\cdots = 2.718283$

2023/03

This book introduces about elementary algorithms and data structure. It includes side-by-side comparison about purely functional realization and their imperative counterpart.

<img src="https://user-images.githubusercontent.com/332938/95418499-442e4b00-096a-11eb-81b9-496020aa5f10.jpg" width="400">

Contents
--------

I am adding exercises and answers to the second edition from 2023/03. I wrote the first edition from 2009 to 2017, then rewrote the second edition from 2020 to 2023. The PDF can be downloaded for preview ([EN](https://github.com/liuxinyu95/AlgoXY/files/10954434/algoxy-en.pdf), [中文](https://github.com/liuxinyu95/AlgoXY/files/10954457/algoxy-zh-cn.pdf)). The 1st edition in Chinese ([中文](http://www.ituring.com.cn/book/1907)) was published in 2017. I recently switched my focus to the Mathematics of programming, the new book is also available in ([github](https://github.com/liuxinyu95/unplugged))


- Preface
- Chapter 0, List;
- Chapter 1, Binary Search Tree;
- Chapter 2, Insertion sort;
- Chapter 3, Red-black tree;
- Chapter 4, AVL tree;
- Chapter 5, Radix tree, Trie and Prefix Tree;
- Chapter 6, B-Trees;
- Chapter 7, Binary Heaps;
- Chapter 8, Selection sort;
- Chapter 9, Binomial heap, Fibonacci heap, and pairing heap;
- Chapter 10, Queue;
- Chapter 11, Sequence;
- Chapter 12, Divide and conquer sort;
- Chapter 13, Search;
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

This will generate algoxy-en.pdf and algoxy-zh-cn.pdf. If you only need the Chinese version for example, you can run `make cn` instead. Run `make force-cn` or `make force-en` to force build the book.

--

LIU Xinyu

liuxinyu95@gmail.com
