A：是个集合包含了例如a b c d
B：也是类似的集合1 2 3

序列长度未知，是指ABBBAA的模式不固定。比如可以ABBBAA、ABABAB,AABBA等等。而每个A和B有都可以遍选{a b c d}和{1 2
3} 

Main> f ['a'..'d'] ['1'..'3'] "ABBAA"
["a12bc","a12bd","a12cb","a12cd","a12db","a12dc","a13bc","a13bd","a13cb","a13cd","a13db","a13dc","a21bc","a21bd","a21cb","a21cd","a21db","a21dc","a23bc","a23bd","a23cb","a23cd","a23db","a23dc","a31bc","a31bd","a31cb","a31cd","a31db","a31dc","a32bc","a32bd","a32cb","a32cd","a32db","a32dc","b12ac","b12ad","b12ca","b12cd","b12da","b12dc","b13ac","b13ad","b13ca","b13cd","b13da","b13dc","b21ac","b21ad","b21ca","b21cd","b21da","b21dc","b23ac","b23ad","b23ca","b23cd","b23da","b23dc","b31ac","b31ad","b31ca","b31cd","b31da","b31dc","b32ac","b32ad","b32ca","b32cd","b32da","b32dc","c12ab","c12ad","c12ba","c12bd","c12da","c12db","c13ab","c13ad","c13ba","c13bd","c13da","c13db","c21ab","c21ad","c21ba","c21bd","c21da","c21db","c23ab","c23ad","c23ba","c23bd","c23da","c23db","c31ab","c31ad","c31ba","c31bd","c31da","c31db","c32ab","c32ad","c32ba","c32bd","c32da","c32db","d12ab","d12ac","d12ba","d12bc","d12ca","d12cb","d13ab","d13ac","d13ba","d13bc","d13ca","d13cb","d21ab","d21ac","d21ba","d21bc","d21ca","d21cb","d23ab","d23ac","d23ba","d23bc","d23ca","d23cb","d31ab","d31ac","d31ba","d31bc","d31ca","d31cb","d32ab","d32ac","d32ba","d32bc","d32ca","d32cb"] 

我提供一个C/C++的实现：
#include <iostream>

using namespace std;

const char *cElements[] = {"abc", "1234", "!@#"};
const char strPattern[] = "ABAC";
const int lenPattern = sizeof(strPattern) - 1;
char str[lenPattern+1];

void allCombination(int level)
{
    if (level == lenPattern) {
        cout << str << ", ";
        return;
    }
    int k = strPattern[level] - 'A';
    const char *p = cElements[k];
    int len = strlen(p);
    for (int i = 0; i < len; ++i) {
        str[level] = p[i];
        allCombination(level+1);
    }

}

int main()
{
    str[lenPattern] = 0;
    allCombination(0);
    return 0;

} 
