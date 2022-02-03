#include <iostream>
#include <memory>
#include <re2/re2.h>
#include <stdexcept>
#include <string_view>

void remove_comments(std::string &contents) {
  RE2::Replace(&contents, "\\/\\/.+?\\n", " ");
  RE2::Replace(&contents, "\\/\\*[\\S\\s]+?\\*\\/", " ");
}

constexpr std::string_view file = "int a = 2\n\
bool b = false\n\
struct a{}\
// this is a comment\n\
/* so is\nthis */";

void loop(std::string &contents, auto callback) {
  re2::StringPiece input(contents);
  std::string token;
  while (RE2::FindAndConsume(
      &input, "([^\\s(){}\\[\\];\"]+|\\(|\\)|{|}|\\[|\\]|(?:\".+\"))", &token)) {
    callback(token);
  }
}

int main() {
  std::string content = file.data();
  std::cout << "pre:\n" << content << '\n';
  remove_comments(content);
  std::cout << "post:\n" << content << '\n';
  auto print = [](std::string_view s) { std::cout << "token: " << s << '\n'; };
  loop(content, print);
}