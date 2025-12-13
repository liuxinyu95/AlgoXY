Elementary Functional Algorithms
====

Edition: $\displaystyle e = \sum \limits _{n=0}^{\infty }{\frac {1}{n!}} = 1 + {\frac {1}{1}}+{\frac {1}{1\cdot 2}}+{\frac {1}{1\cdot 2\cdot 3}}+\cdots = 2.718283$

2025/12

This book presents elementary functional algorithms and data structures with 120 exercises and answers. I wrote the first edition from 2009 to 2017, then rewrote from 2020 to 2023. The **second edition** in PDF can be **downloaded** ([EN](https://github.com/user-attachments/files/18287890/algoxy-en.pdf), [中文](https://github.com/user-attachments/files/18287891/algoxy-zh-cn.pdf)). The first edition in Chinese ([中文](https://book.douban.com/subject/26931430/)) was published in 2017. For more about [mathematics in programming](https://link.springer.com/book/10.1007/978-981-97-2432-1).

<!-- <img src="https://user-images.githubusercontent.com/332938/95418499-442e4b00-096a-11eb-81b9-496020aa5f10.jpg" width="400"> -->

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

Preface
--------
Some people did not see their works published, like Fermat and Galois; Some people refused to publish any 'imperfect' works despite they had already been great and important (at least from our view point), like Gauss; Some people lost some of their works in fires or wars, like Riemann. We owe Samuel Fermat, who collected his father's notes and mails, published a special edition of _Arithmetica, augmented with Fermat's comments_; We owe Liouville, who re-discovered and published Galois' papers in _Journal de Mathématiques Pures et Appliquées_; We owe Dedekind, who collected the burnt notebook from Mrs Riemann after her husband's early death. We appreciate those publishers who save, protect, and spread human knowledge no matter it's profitable or not (in terms of money). The development of technology and internet greatly changes the way that people write, log, edit, and publish things. I was able to write and share about elementary functional algorithms and data structures online from 2009. It was accumulated to a draft book by 2015, and was published in 2017 in Chinese.

I always think I am so lucky compare to the people in the past. It's impossible to make this happen before, while there are plenty of wonderful works in this domain yet to be published. Thanks to the editors, the publisher, and every reader. I collected the feedback, questions, and comments, started to re-write the book from the end of 2020, and completed a new edition by May 2023. The main changes are:

1. Rewrite all chapters, use simple notations instead of long text descriptions.

2. Reorganize the book structure. **(a)** Move the list to the first chapter, make it friendly to the new readers to the functional programming; **(b)** Add a section of paired-list B-tree in chapter 7. Add the remove algorithms to the red-black tree and AVL tree to the appendix; **(c)** Remove the chapter of the suffix tree. For imperative string matching, focus on KMP algorithm, and remove the Boyer-Moore algorithm; **(d)** Add the answers to all the 120 exercises.

3. Unify the programming language of the example programs. This is one of the most feedback to the first edition. Use Haskell for functional examples, and consolidate imperative ones in a language named 'Bourbaki' (Nicolas Bourbaki is the pseudonym of a group of (mainly) French mathematicians, including André Weil, Henri Cartan, and etc. The virtual figure indicates the programming language does not exist, while it reflects some popular languages behind, like Python, Java, C and etc.). Collect the examples as an appendix after each chapter.

4. Correct all known errors in the first edition.

Chris Okasaki and Richard Bird established the main content of purely functional data structures and algorithms. I essentially tailored and re-organized the materials under their framework. Although the new functional applications and technologies evolve fast, the elementary algorithms are relative stable. This slow changing part allows me to spend over 10 years, iteratively write and improve this book.

Install
--------

We use LuaLaTeX, an extended version of TeX, which is provided in TeXLive distribution.

### Install TeXLive

In Linux/Unix like environment, do **NOT** install the TeXLive through package manager (e.g. apt-get or apk). Go to TeXLive [official site](https://tug.org/texlive/) to download the setup script. In Windows, TeXLive provides a [gui based installer](https://tug.org/texlive/windows.html), in Mac OS X, there's a [MacTeX](https://www.tug.org/mactex/).

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
