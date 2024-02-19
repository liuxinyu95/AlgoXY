Elementary Functional Algorithms
====

Edition: $\displaystyle e = \sum \limits _{n=0}^{\infty }{\frac {1}{n!}} = 1 + {\frac {1}{1}}+{\frac {1}{1\cdot 2}}+{\frac {1}{1\cdot 2\cdot 3}}+\cdots = 2.718283$

2024/02

This book presents elementary functional algorithms and data structures with 120 exercises and answers. I wrote the first edition from 2009 to 2017, then rewrote from 2020 to 2023. The **second edition** in PDF can be **downloaded** ([EN](https://github.com/liuxinyu95/AlgoXY/files/14003500/algoxy-en.pdf), [中文](https://github.com/liuxinyu95/AlgoXY/files/14003498/algoxy-zh-cn.pdf)). The first edition in Chinese ([中文](https://book.douban.com/subject/26931430/)) was published in 2017.

<img src="https://user-images.githubusercontent.com/332938/95418499-442e4b00-096a-11eb-81b9-496020aa5f10.jpg" width="400">

Contents
--------

- Preface
- Chapter 1, List;
- Chapter 2, Binary Search Tree;
- Chapter 3, Insertion sort;
- Chapter 4, Red-black tree;
- Chapter 5, AVL tree;
- Chapter 6, Radix tree, Trie and Prefix Tree;
- Chapter 7, B-Trees;
- Chapter 8, Binary Heaps;
- Chapter 9, Selection sort;
- Chapter 10, Binomial heap, Fibonacci heap, and pairing heap;
- Chapter 11, Queue;
- Chapter 12, Sequence;
- Chapter 13, Divide and conquer sort;
- Chapter 14, Search I;
- Chapter 15, Search II;
- Appendices and answers

Install
--------

You may use [gitpod](https://gitpod.io/new#https://github.com/liuxinyu95/algoxy) to build the PDF book from cloud desktop. For local build, you need TeXLive. We use LuaLaTeX, an extended version of TeX.

### Install TeXLive

In Debian/Ubuntu like Linux environment, do **NOT** install the TeXLive through apt-get. Go to TeXLive [official site](https://tug.org/texlive/) to download the setup script. In Windows, TeXLive provides a [gui based installer](https://tug.org/texlive/windows.html), in Mac OS X, there's a [MacTeX](https://www.tug.org/mactex/).

### Others

You need the GNU make tool, in Debian/Ubuntu like Linux, it can be installed through:

```bash
$ sudo apt-get install build-essential
```

In Windows, you can use WSL or MSYS. In Mac OS X, please install the developer tool from this command line:

```bash
$ xcode-select --install
```

### Font setting

The default build supports Linux, Mac OS X, and Windows. You can install additional font (like [Noto CJK](https://github.com/notofonts/noto-cjk/)) typesetting (see `prelude.sty`). Some system fonts, e.g. STKaiti, were moved to `/System/Library/AssetsV2/com_apple_MobileAsset_Font7` in Mac OS X from 2022, you need add the path to the local TeXLive configuration:

```bash
sudo tlmgr conf texmf OSFONTDIR /System/Library/AssetsV2/com_apple_MobileAsset_Font7
```

### Build the book PDF

enter the folder contains the book TeX manuscript, run

```bash
$ make
```

This will generate algoxy-en.pdf and algoxy-zh-cn.pdf. If you only need the Chinese version for example, you can run `make cn` instead. Run `make force-cn` or `make force-en` to force build the book.

--

LIU Xinyu
