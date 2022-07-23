#pragma once
typedef unsigned long int uint64_t;
typedef long int int64_t;
typedef unsigned char uchar;
struct セルフprimative_strview {
  uint64_t size;
  const uchar *str;
};
typedef struct セルフprimative_strview セルフprimative_strview;
uint64_t selfprint(const セルフprimative_strview *s);
int64_t geterrno();
int64_t selfflush();
// This produces a random number 1 - 6 inclusive
int64_t randomnumber();