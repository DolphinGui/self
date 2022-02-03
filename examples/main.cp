struct u32{
  // this uses the nosignedwrap flag when
  // doing ops.
private:
  // TODO: implement arrays
  byte a;
  byte b;
  byte c;
  byte d;
public:
  void operator = (u32 RHS){
    asm("mov %a %RHS")
  }
  operator literal(token lit){}
  friend u32 operator + (u32 RHS){
    asm("%3 = add nsw i32 %1, %0")
    asm("ret i32 %3")
  }
  friend u32 operator - (u32 RHS){
    asm("%3 = sub nsw i32 %1, %0")
    asm("ret i32 %3")
  }
}


u32 main(){
  u32 a = 1 - 1
  return a
}