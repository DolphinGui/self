import <compiler>

funct array(var size: compiler::size_t, var T: typename){
  var arr = struct{
    operator [](var index: size_t){
      return &this + index;
    }
    operator = (list init){
      for(int i = 0; i < init.size(), i++){
        this[i] = init[i]
      }
      return this
    }
  }
  for(var i = 0; i < size, i++)
    arr.members.add(T)
  compiler::set_packing(arr, false)
  return arr
}

funct int(var size : compiler::size_t, var signed: compiler::bool){
  assert(size % 8 == 0, "size is not representable in bytes.")
  var max = get_int_max(size, signed)
  var min = get_int_min(size, signed)
  var int = struct{
    var internal: array(size, byte)
    operator = (var literal / 8: size_t){
      using namespace compiler
      assert(literal < max, "value is too big")
      assert(literal > min, "value is too small")
      assign(internal, literal)
      return this
    }
    operator = (typeof(this) RHS){
      using namespace compiler
      assign(internal, RHS)
      return this
    }
  }
  return int
}

var i8 = int(8, true)
var i16 = int(16, true)
var i32 = int(32, true)
var i64 = int(64, true)
var u8 = int(8, false)
var u16 = int(16, false)
var u32 = int(32, false)
var u64 = int(64, false)