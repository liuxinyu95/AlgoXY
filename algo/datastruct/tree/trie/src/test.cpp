#include "inttrie.hpp"
#include "intpatricia.hpp"
#include "trie.hpp"
#include "patricia.hpp"

int main(int, char**){
  IntTrieTest().run();
  IntPatriciaTest().run();
  TrieTest().run();
  PatriciaTest().run();
}
