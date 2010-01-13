#include "inttrie.hpp"
#include "intpatricia.hpp"
#include "trie.hpp"
#include "patricia.hpp"
#include "edict.hpp"

int main(int, char**){
  IntTrieTest().run();
  IntPatriciaTest().run();
  TrieTest().run();
  PatriciaTest().run();
  EDictTest().run();
}
